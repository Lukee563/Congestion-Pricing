# =============================================================================
# Speed_Data_Cleaning.R
# Mirrors final_model_data structure but uses avg_speed as outcome
# Restricted to 2024-01-01 onward (speed data availability)
#
# Run AFTER Data_Cleaning.R with these objects in keep_list:
#   hex_grid, hex_features, weather_weekly
# =============================================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tidyr)
  library(sf)
})

message("[", Sys.time(), "] Starting speed data cleaning pipeline...")

# ── Config ────────────────────────────────────────────────────────────────────

TOLL_DATE         <- as.Date("2025-01-05")
END_DATE          <- as.Date("2026-04-01")
study_weeks_speed <- seq.Date(as.Date("2024-01-06"), END_DATE, by = "week")  # Saturday-anchored to match weather_weekly

# ── 1. Load speed data ────────────────────────────────────────────────────────
message("[", Sys.time(), "] Loading speed data...")

speed_data <- read_csv("../../data/cleaned/speed_data.csv", show_col_types = FALSE) |>
  select(-any_of("...1")) |>
  filter(id != "1", !is.na(cent_lat), !is.na(cent_lon)) |>
  mutate(week = as.Date(floor_date(as.Date(week_start), "week", week_start = 6))) |>
  filter(week %in% study_weeks_speed)

message(sprintf("  %s rows | %d stations | %d weeks",
                format(nrow(speed_data), big.mark = ","),
                n_distinct(speed_data$id),
                n_distinct(speed_data$week)))

# ── 2. Map stations to hex grid ───────────────────────────────────────────────
message("[", Sys.time(), "] Mapping speed stations to hex grid...")

speed_sf <- speed_data |>
  distinct(id, cent_lat, cent_lon) |>
  st_as_sf(coords = c("cent_lon", "cent_lat"), crs = 4326) |>
  st_transform(2263)

hex_speed_map <- data.frame(
  hex_id           = hex_grid$hex_id,
  nearest_speed_id = speed_sf$id[st_nearest_feature(hex_grid, speed_sf)]
)

message(sprintf("  %d hexes -> %d unique speed stations",
                nrow(hex_speed_map), n_distinct(hex_speed_map$nearest_speed_id)))

# ── 3. Build balanced hex-week panel ─────────────────────────────────────────
message("[", Sys.time(), "] Building balanced hex-week panel...")

speed_panel <- hex_speed_map |>
  crossing(week = study_weeks_speed) |>
  left_join(
    speed_data |> select(id, week, avg_speed, med_speed, sd_speed),
    by = c("nearest_speed_id" = "id", "week"),
    relationship = "many-to-many"
  )

message(sprintf("  %s rows | %d hexes x %d weeks",
                format(nrow(speed_panel), big.mark = ","),
                n_distinct(speed_panel$hex_id),
                n_distinct(speed_panel$week)))

# ── 4. Merge hex features + weather ──────────────────────────────────────────
message("[", Sys.time(), "] Merging hex features and weather...")

speed_model_data <- speed_panel |>
  left_join(hex_features, by = "hex_id") |>
  left_join(
    weather_weekly |> filter(week %in% study_weeks_speed),
    by = c("week", "nearest_station" = "station")
  ) |>
  mutate(
    borough    = toupper(BOROUGH_RECOVERED),
    post       = as.numeric(week >= TOLL_DATE),
    treatment  = in_zone * post,
    week_index = as.numeric(week - min(week)) / 7
  ) |>
  select(-BOROUGH_RECOVERED)

# ── 5. Baseline speed + weather imputation ────────────────────────────────────
message("[", Sys.time(), "] Computing baseline speed and imputing weather...")

speed_model_data <- speed_model_data |>
  group_by(hex_id) |>
  mutate(baseline_speed = mean(avg_speed[week < TOLL_DATE], na.rm = TRUE)) |>
  ungroup() |>
  group_by(nearest_station) |>
  mutate(
    avg_temp = if_else(
      is.na(avg_temp) | is.nan(avg_temp),
      ifelse(all(is.na(avg_temp)), NA_real_, mean(avg_temp, na.rm = TRUE)),
      avg_temp
    ),
    tot_precip = if_else(
      is.na(tot_precip) | is.nan(tot_precip),
      ifelse(all(is.na(tot_precip)), 0, mean(tot_precip, na.rm = TRUE)),
      tot_precip
    )
  ) |>
  ungroup() |>
  mutate(
    avg_temp   = if_else(is.na(avg_temp), mean(avg_temp, na.rm = TRUE), avg_temp),
    tot_precip = if_else(is.na(tot_precip), 0, tot_precip)
  )

# ── 6. Diagnostics ────────────────────────────────────────────────────────────
message("[", Sys.time(), "] Running diagnostics...")

n_missing   <- sum(is.na(speed_model_data$avg_speed))
pct_missing <- round(100 * n_missing / nrow(speed_model_data), 1)

message(sprintf("  Rows              : %s", format(nrow(speed_model_data), big.mark = ",")))
message(sprintf("  Hexes             : %d", n_distinct(speed_model_data$hex_id)))
message(sprintf("  Weeks             : %d", n_distinct(speed_model_data$week)))
message(sprintf("  Treatment hexes   : %d", n_distinct(speed_model_data$hex_id[speed_model_data$in_zone == 1])))
message(sprintf("  Missing avg_speed : %s (%s%%)", format(n_missing, big.mark = ","), pct_missing))
message(sprintf("  Date range        : %s to %s", min(speed_model_data$week), max(speed_model_data$week)))
message(sprintf("  Pre-treatment wks : %d", n_distinct(speed_model_data$week[speed_model_data$post == 0])))
message(sprintf("  Post-treatment wks: %d", n_distinct(speed_model_data$week[speed_model_data$post == 1])))

# ── 7. Export ─────────────────────────────────────────────────────────────────
write_csv(speed_model_data, "../../data/cleaned/speed_model_data.csv")
message("[", Sys.time(), "] Done. Written to ../../data/cleaned/speed_model_data.csv")