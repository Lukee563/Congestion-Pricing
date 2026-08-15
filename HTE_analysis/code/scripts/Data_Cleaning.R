rm(list=ls())
library(readr)
library(dplyr)
library(tidyr)
library(writexl)
library(tigris)
library(lubridate)
library(sf)
library(data.table)
library(purrr)

# Global Constants
START_DATE  <- as.Date("2024-01-01")
END_DATE    <- as.Date("2026-04-01")
TOLL_DATE   <- as.Date("2025-01-05")
ST_60_Y     <- 217000
WEEK_START  <- 6   # Saturday

# Floor the bounds to the SAME anchor the data uses
START_WK <- floor_date(START_DATE, "week", week_start = WEEK_START)
END_WK   <- floor_date(END_DATE,   "week", week_start = WEEK_START)
study_weeks <- seq.Date(START_WK, END_WK, by = "week")

message("[", Sys.time(), "] Study window: ", START_WK, " to ", END_WK,
        " (", length(study_weeks), " weeks)")


# 1. WEATHER DATA IMPORT & AGGREGATION
#    Weather file labels (YYYYMM) are derived from the study window with one
#    month of padding on each side, so changing the dates never leaves the
#    loader pointing at a hardcoded range.
message("[", Sys.time(), "] Loading and aggregating raw weather inputs...")
lab_start <- format(START_WK %m-% months(1), "%Y%m")
lab_end   <- format(END_WK   %m+% months(1), "%Y%m")

labels <- crossing(y = 2020:2027, m = sprintf("%02d", 1:12)) %>%
  mutate(label = paste0(y, m)) %>%
  filter(label >= lab_start & label <= lab_end) %>%
  pull(label)

weather_paths <- paste0("../../data/raw/weather/", labels, ".csv")
weather_paths <- weather_paths[file.exists(weather_paths)]

weather_raw <- map(weather_paths, ~read_csv(.x, show_col_types = FALSE)) %>%
  rbindlist(fill = TRUE)

weather_weekly <- weather_raw %>%
  mutate(
    date = as.Date(time),
    week = floor_date(date, "week", week_start = WEEK_START)
  ) %>%
  filter(week %in% study_weeks) %>%
  group_by(week, station) %>%
  summarise(
    avg_temp     = mean(`temp_2m_avg [degF]`, na.rm = TRUE),
    avg_humidity = mean(`relative_humidity_avg [percent]`, na.rm = TRUE),
    tot_precip   = if (all(is.na(`precip_total [inch]`))) NA_real_
    else sum(`precip_total [inch]`, na.rm = TRUE),
    obs_count    = n(),
    .groups = "drop"
  )

# 2. SPATIAL SETUP: LANDMASS BOUNDARY & WATER IDENTIFICATION
message("[", Sys.time(), "] Fetching NYC land borders and erasing water bodies...")

nyc_counties <- counties("NY", cb = TRUE) %>%
  filter(NAME %in% c("New York", "Kings", "Queens", "Bronx")) %>%
  st_transform(2263)

nyc_fips <- c("061", "047", "081", "005")
nyc_water <- map_df(nyc_fips, ~area_water("NY", .x, year = 2022)) %>%
  st_transform(2263) %>%
  st_union()

nyc_boundary <- st_difference(st_union(nyc_counties), nyc_water)

hex_grid <- st_make_grid(nyc_boundary, cellsize = 1640, square = FALSE) %>%
  st_sf() %>%
  .[nyc_boundary, ] %>%
  mutate(hex_id = row_number())

hex_borough_lookup <- hex_grid %>%
  st_centroid() %>%
  st_join(nyc_counties %>% select(BOROUGH_RECOVERED = NAME), join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  select(hex_id, BOROUGH_RECOVERED) %>%
  distinct(hex_id, .keep_all = TRUE)

# 3. SPATIAL FEATURE ENGINEERING: WEATHER STATIONS & SUBWAYS
message("[", Sys.time(), "] Mapping weather stations and subway networks to hex grid...")

stations_lookup <- weather_raw %>%
  distinct(station, .keep_all = TRUE) %>%
  filter(!is.na(`latitude [degrees_north]`)) %>%
  st_as_sf(coords = c("longitude [degrees_east]", "latitude [degrees_north]"), crs = 4326) %>%
  st_transform(2263)

nearest_idx <- st_nearest_feature(hex_grid, stations_lookup)
hex_station_map <- data.frame(
  hex_id          = hex_grid$hex_id,
  nearest_station = stations_lookup$station[nearest_idx]
)

subways <- read.csv("../../data/raw/MTA_Subway_Stations_20260516.csv")
subway_stations_sf <- subways %>%
  filter(!is.na(GTFS.Latitude), !is.na(GTFS.Longitude)) %>%
  st_as_sf(coords = c("GTFS.Longitude", "GTFS.Latitude"), crs = 4326) %>%
  st_transform(2263)

nearest_subway_idx <- st_nearest_feature(hex_grid, subway_stations_sf)
distances <- st_distance(hex_grid, subway_stations_sf[nearest_subway_idx, ], by_element = TRUE)
hex_subway_map <- data.frame(
  hex_id         = hex_grid$hex_id,
  subway_dist_ft = as.numeric(distances)
)

# 4. COLLISION CLEANING & REBALANCED PANEL PRODUCTION
message("[", Sys.time(), "] Processing collision files into a balanced land-only panel...")

MotorCollisions <- read_csv("../../data/raw/MotorCollisions.csv", show_col_types = FALSE) %>%
  filter(BOROUGH != "STATEN ISLAND")

collisions_clean <- MotorCollisions %>%
  mutate(date = mdy(`CRASH DATE`),
         week = floor_date(date, "week", week_start = WEEK_START)) %>%
  filter(week %in% study_weeks,
         !is.na(LATITUDE), !is.na(LONGITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(2263) %>%
  st_filter(hex_grid) %>%
  st_join(hex_grid) %>%
  st_drop_geometry()

collision_panel <- collisions_clean %>%
  group_by(hex_id, week) %>%
  summarise(y_count = n(), .groups = "drop") %>%
  complete(hex_id = hex_grid$hex_id, week = study_weeks, fill = list(y_count = 0))

# 5. CBD BOUNDARY INTERSECTIONS & SPATIAL ATTRIBUTES
message("[", Sys.time(), "] Building geographic feature columns and treatments...")

cbd_csv <- read_csv("../../data/raw/mta_shapefile.csv", show_col_types = FALSE)
cbd_polygon <- st_as_sf(cbd_csv, wkt = "polygon", crs = 4326) %>%
  st_union() %>%
  st_transform(2263)

hex_centroids_2263 <- st_centroid(hex_grid)
hex_centroids_4326 <- st_transform(hex_centroids_2263, 4326)
in_cbd_matrix      <- st_intersects(hex_centroids_2263, cbd_polygon, sparse = FALSE)

hex_features <- hex_centroids_2263 %>%
  mutate(
    x_coord   = st_coordinates(.)[,1],
    y_coord   = st_coordinates(.)[,2],
    lon       = st_coordinates(hex_centroids_4326)[,1],
    lat       = st_coordinates(hex_centroids_4326)[,2],
    dist_60th = abs(y_coord - ST_60_Y),
    is_manh   = as.numeric(st_intersects(., nyc_counties %>% filter(NAME == "New York"), sparse = FALSE)),
    in_zone   = as.numeric(in_cbd_matrix[, 1])
  ) %>%
  st_drop_geometry() %>%
  left_join(hex_station_map,    by = "hex_id") %>%
  left_join(hex_subway_map,     by = "hex_id") %>%
  left_join(hex_borough_lookup, by = "hex_id")

# 6. MASTER COMPILATION & IMPUTATION
message("[", Sys.time(), "] Merging master matrices and generating baseline markers...")

final_model_data <- collision_panel %>%
  left_join(hex_features, by = "hex_id") %>%
  left_join(weather_weekly, by = c("week", "nearest_station" = "station")) %>%
  mutate(
    borough    = toupper(BOROUGH_RECOVERED),
    post       = as.numeric(week >= TOLL_DATE),
    treatment  = in_zone * post,
    week_index = as.numeric(week - min(week)) / 7
  ) %>%
  select(-BOROUGH_RECOVERED)

# Baseline pre-policy collision risk per hexagon
final_model_data <- final_model_data %>%
  group_by(hex_id) %>%
  mutate(baseline_risk = mean(y_count[week < TOLL_DATE], na.rm = TRUE)) %>%
  ungroup()

# Weather missingness: same-week cross-station mean (seasonality-preserving)
week_means <- weather_weekly %>%
  group_by(week) %>%
  summarise(week_temp   = mean(avg_temp,   na.rm = TRUE),
            week_precip = mean(tot_precip, na.rm = TRUE),
            .groups = "drop")

final_model_data <- final_model_data %>%
  left_join(week_means, by = "week") %>%
  mutate(
    avg_temp   = coalesce(avg_temp,   week_temp),
    tot_precip = coalesce(tot_precip, week_precip)
  ) %>%
  select(-week_temp, -week_precip)

# Checks
n_temp_na   <- sum(is.na(final_model_data$avg_temp))
n_precip_na <- sum(is.na(final_model_data$tot_precip))
n_bad_week  <- sum(!(final_model_data$week %in% study_weeks))

if (n_bad_week > 0)
  stop("Week mismatch: ", n_bad_week, " rows on weeks outside study_weeks. ",
       "Check WEEK_START alignment.")
if (n_temp_na > 0 || n_precip_na > 0)
  stop("Residual weather NAs after imputation: temp=", n_temp_na,
       " precip=", n_precip_na, ". A study week likely has zero reporting ",
       "stations; widen the weather label range or patch that week.")

message("[", Sys.time(), "] Integrity checks passed: 0 bad weeks, 0 weather NAs.")


# 7. EXPORT & CLEANUP
message("[", Sys.time(), "] Writing final datasets out to local directory...")

write_csv(final_model_data, "../../data/cleaned/model_data_final.csv")
st_write(hex_grid, "../../data/cleaned/spatial_files/hex_grid.geojson", delete_dsn = TRUE)

message("Processing complete for ", n_distinct(final_model_data$hex_id),
        " hexes over ", n_distinct(final_model_data$week), " weeks (",
        nrow(final_model_data), " rows).")

# Table Generation
library(kableExtra)
# Variables to summarize, in display order, with labels, panel, and digits 
sumvars <- tibble::tribble(
  ~var,             ~label,                                ~panel,                   ~digits,
  "y_count",        "Weekly collisions",                   "Outcome",                 2,
  "baseline_risk",  "Baseline weekly risk (pre-policy)",   "Outcome",                 2,
  "treatment",      "Treated (in-zone $\\times$ post)",    "Treatment \\& geography", 3,
  "in_zone",        "In congestion zone",                  "Treatment \\& geography", 3,
  "is_manh",        "Manhattan",                           "Treatment \\& geography", 3,
  "dist_60th",      "Distance to 60th St.\\ (ft)",         "Treatment \\& geography", 0,
  "subway_dist_ft", "Distance to subway (ft)",             "Treatment \\& geography", 0,
  "avg_temp",       "Avg.\\ temperature ($^\\circ$F)",     "Weather controls",        1,
  "tot_precip",     "Total precipitation (in)",            "Weather controls",        2
)

# Compute stats over the estimation sample (hexagon-week)
stat_tbl <- final_model_data %>%
  select(all_of(sumvars$var)) %>%
  pivot_longer(everything(), names_to = "var", values_to = "value") %>%
  group_by(var) %>%
  summarise(
    Mean = mean(value, na.rm = TRUE), SD = sd(value, na.rm = TRUE),
    Min = min(value, na.rm = TRUE), Median = median(value, na.rm = TRUE),
    Max = max(value, na.rm = TRUE), .groups = "drop"
  )
nrow(final_model_data)
n_distinct(final_model_data$hex_id)
print(stat_tbl)