# ==============================================================================
# PROJECT: NYC Congestion Pricing - Causal Impact Analysis (GRF)
# AUTHOR: Luke Catalano
# AUDIT DATE: 2026-04-08
# VERSION: Final Audited & Debugged (v7)
# ==============================================================================

# 1. ENVIRONMENT & STABILITY SETUP
# ------------------------------------------------------------------------------
options(scipen = 999)
rm(list=ls())
graphics.off()
while (!is.null(dev.list())) dev.off() 
set.seed(123)

# System Memory Management
Sys.setenv(R_MAX_VSIZE = "48Gb") 

library(pacman)
p_load(readr, dplyr, tidyr, lubridate, sf, tigris, data.table, purrr, grf, ggplot2, scales, ggrepel)

# Output directory
if(!dir.exists("../../outputs")) dir.create("../../outputs", recursive = TRUE)

# Constants
EPSG_NYC   <- 2263 
ST_60_Y    <- 217000 
START_DATE <- as.Date("2022-01-01")
END_DATE   <- as.Date("2026-03-22") 
TOLL_DATE  <- as.Date("2025-01-05")

study_weeks <- seq.Date(START_DATE, END_DATE, by = "week")

# 2. WEATHER AGGREGATION
# ------------------------------------------------------------------------------
weather_labels <- crossing(y = 2021:2026, m = sprintf("%02d", 1:12)) %>%
  mutate(label = paste0(y, m)) %>%
  filter(label >= "202104" & label <= "202603") %>%
  pull(label)

weather_raw <- map(weather_labels, ~{
  path <- paste0("../../data/raw/weather/", .x, ".csv")
  if(file.exists(path)) read_csv(path, show_col_types = FALSE) else NULL
}) %>% compact() %>% rbindlist(fill = TRUE)

weather_weekly <- weather_raw %>%
  mutate(date = as.Date(time),
         week = floor_date(date, "week", week_start = 6)) %>%
  filter(week %in% study_weeks) %>%
  group_by(week, station) %>%
  summarise(
    avg_temp   = mean(`temp_2m_avg [degF]`, na.rm = TRUE),
    tot_precip = sum(`precip_total [inch]`, na.rm = TRUE),
    .groups = "drop"
  )

# 3. SPATIAL INFRASTRUCTURE & SURGICAL LANDMASS ISOLATION
# ------------------------------------------------------------------------------
# THE AUDIT FIX: We use a negative buffer to decouple Manhattan from Brooklyn piers
nyc_boundary <- counties("NY", cb = TRUE) %>%
  filter(NAME %in% c("New York", "Kings", "Queens", "Bronx")) %>% 
  st_transform(EPSG_NYC)

# Shrink the 'Treatment' mask to pull away from water boundaries
manhattan_shrunk <- nyc_boundary %>% 
  filter(NAME == "New York") %>% 
  st_buffer(dist = -150) # Increased to 150ft for absolute safety

hex_grid <- st_make_grid(nyc_boundary, cellsize = 1640, square = FALSE) %>%
  st_sf() %>%
  mutate(hex_id = row_number()) %>%
  .[nyc_boundary, ]

hex_meta <- hex_grid %>%
  mutate(
    centroid = st_centroid(geometry),
    y_coord  = st_coordinates(centroid)[,2],
    
    # Identify borough by standard intersection
    BOROUGH = as.character(st_join(st_sf(centroid), nyc_boundary)$NAME),
    
    # Treatment Logic: Centroid must reside within the 'shrunk' landmask
    is_on_manh_land = as.logical(st_intersects(centroid, manhattan_shrunk, sparse = FALSE)),
    is_on_manh_land = replace_na(is_on_manh_land, FALSE),
    in_zone = if_else(is_on_manh_land == TRUE & y_coord < ST_60_Y, 1, 0),
    
    dist_60th = abs(y_coord - ST_60_Y)
  ) %>%
  st_drop_geometry() %>%
  distinct(hex_id, .keep_all = TRUE)

# Weather station mapping
stations_sf <- weather_raw %>%
  distinct(station, .keep_all = TRUE) %>%
  filter(!is.na(`latitude [degrees_north]`)) %>%
  st_as_sf(coords = c("longitude [degrees_east]", "latitude [degrees_north]"), crs = 4326) %>%
  st_transform(EPSG_NYC)

hex_meta$nearest_station <- stations_sf$station[st_nearest_feature(st_centroid(hex_grid), stations_sf)]

# 4. PANEL ASSEMBLY & FEATURE ENGINEERING
# ------------------------------------------------------------------------------
collisions <- read_csv("../../data/raw/MotorCollisions.csv", show_col_types = FALSE) %>%
  mutate(date = mdy(`CRASH DATE`),
         week = floor_date(date, "week", week_start = 6)) %>%
  filter(date >= START_DATE, date <= END_DATE, !is.na(LATITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(EPSG_NYC) %>%
  st_join(hex_grid) %>%
  st_drop_geometry()

panel <- collisions %>%
  group_by(hex_id, week) %>%
  summarise(y_count = n(), .groups = "drop") %>%
  complete(hex_id = hex_grid$hex_id, week = study_weeks, fill = list(y_count = 0))

model_data <- panel %>%
  left_join(hex_meta, by = "hex_id") %>%
  left_join(weather_weekly, by = c("week", "nearest_station" = "station")) %>%
  mutate(
    post       = as.numeric(week >= TOLL_DATE),
    treatment  = in_zone * post,
    week_idx   = as.numeric(week - min(week)) / 7
  ) %>%
  group_by(hex_id) %>%
  mutate(baseline_risk = mean(y_count[week < TOLL_DATE], na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(baseline_risk = replace_na(baseline_risk, 0)) %>%
  mutate(across(c(avg_temp, tot_precip), ~replace_na(., mean(., na.rm = TRUE))))

# 5. CAUSAL MODELING (STABILIZED GRF)
# ------------------------------------------------------------------------------
final_df <- model_data %>%
  select(y_count, treatment, in_zone, BOROUGH, baseline_risk, y_coord, 
         dist_60th, week_idx, avg_temp, tot_precip, hex_id) %>%
  drop_na()

Y <- as.numeric(final_df$y_count)
W <- as.numeric(final_df$treatment)
X_raw <- final_df %>% select(baseline_risk, y_coord, dist_60th, week_idx, avg_temp, tot_precip) %>% as.matrix()

X_scaled <- scale(X_raw)
X_center <- attr(X_scaled, "scaled:center")
X_scale  <- attr(X_scaled, "scaled:scale")

message("[", Sys.time(), "] Training Deterministic Forest...")
cf <- causal_forest(X_scaled, Y, W, clusters = final_df$hex_id, num.trees = 1500, 
                    sample.fraction = 0.2, honesty = TRUE, seed = 123)

# 6. PREDICTION & IMPACT MAPPING
# ------------------------------------------------------------------------------
target_week <- median(final_df$week_idx[final_df$treatment == 1])
X_map_df <- final_df %>% distinct(hex_id, .keep_all = TRUE) %>% mutate(week_idx = target_week)
X_map_mat <- X_map_df %>% select(baseline_risk, y_coord, dist_60th, week_idx, avg_temp, tot_precip) %>% 
  as.matrix() %>% scale(center = X_center, scale = X_scale)

preds <- predict(cf, newdata = X_map_mat, estimate.variance = TRUE)
impact_results <- X_map_df %>% mutate(
  tau = preds$predictions,
  se  = sqrt(preds$variance.estimates),
  is_significant = abs(tau / se) > 1.96,
  pct_change = ifelse(baseline_risk > 0, (tau / baseline_risk) * 100, 0)
)

# 7. METRICS & DIAGNOSTIC TABLES
# ------------------------------------------------------------------------------
ate_res <- average_treatment_effect(cf, target.sample = "treated")
ate_table <- data.frame(Metric = c("ATE", "SE", "t-stat", "p-val"),
                        Value = c(round(ate_res[1], 4), round(ate_res[2], 4), 
                                  round(ate_res[1]/ate_res[2], 2), round(2*pnorm(-abs(ate_res[1]/ate_res[2])), 4)))

borough_summary <- impact_results %>% group_by(BOROUGH) %>% 
  summarise(Hexagons = n(), Avg_Tau = mean(tau), Avg_Risk_Delta = mean(pct_change), Sig_Share = mean(is_significant), .groups = "drop")

write_csv(ate_table, "../../outputs/table_global_ate.csv")
write_csv(borough_summary, "../../outputs/table_borough_heterogeneity.csv")

# 8. FINAL VISUALIZATION (THE SUCCESS MAP)
# ------------------------------------------------------------------------------
plot_sf <- hex_grid %>% left_join(impact_results, by = "hex_id") %>% filter(!is.na(tau))
cbd_poly <- plot_sf %>% filter(in_zone == 1) %>% st_union()

# Top Significant Points
hotspots_sf <- impact_results %>% filter(is_significant == TRUE) %>% arrange(desc(tau)) %>% head(5) %>%
  left_join(hex_grid, by = "hex_id") %>% st_as_sf()
coolspots_sf <- impact_results %>% filter(is_significant == TRUE) %>% arrange(tau) %>% head(5) %>%
  left_join(hex_grid, by = "hex_id") %>% st_as_sf()

final_viz <- ggplot(plot_sf) +
  geom_sf(aes(fill = pct_change, alpha = is_significant), color = NA) +
  geom_sf(data = cbd_poly, fill = NA, color = "black", linewidth = 0.8) +
  geom_sf(data = nyc_boundary, fill = NA, color = "black", linewidth = 0.2, alpha = 0.5) +
  
  # AUDIT FIX: Using geom_label_repel with stat = "sf_coordinates"
  geom_label_repel(data = hotspots_sf, aes(label = paste0("+", round(pct_change), "%"), geometry = geometry),
                   stat = "sf_coordinates", color = "#67001f", fill = "white", fontface = "bold", size = 3.2, 
                   nudge_y = 6000, direction = "y", point.padding = unit(10, "pt")) +
  
  geom_label_repel(data = coolspots_sf, aes(label = paste0(round(pct_change), "%"), geometry = geometry),
                   stat = "sf_coordinates", color = "#053061", fill = "white", fontface = "bold", size = 3.2, 
                   nudge_y = -6000, direction = "y", point.padding = unit(10, "pt")) +
  
  scale_fill_gradient2(low = "#053061", mid = "#f7f7f7", high = "#67001f", midpoint = 0, 
                       limits = c(-60, 60), oob = squish, labels = label_percent(scale = 1), name = "Risk Δ (%)",
                       guide = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  scale_alpha_manual(values = c("TRUE" = 1, "FALSE" = 0.3), guide = "none") +
  theme_void() + 
  labs(title = "Causal Impact Success Map: NYC Congestion Pricing", 
       subtitle = "Surgically Isolated Treatment Zone vs. Displacement Spillovers",
       caption = "Faded areas represent non-significant local effects (p > 0.05).")

ggsave("../../outputs/Impact_Success_Map.png", final_viz, width = 12, height = 9, dpi = 300)

# 9. STRUCTURAL VALIDATION MAP
# ------------------------------------------------------------------------------
struct_viz <- ggplot() +
  geom_sf(data = plot_sf, aes(fill = as.factor(in_zone)), color = "white", linewidth = 0.1) +
  geom_sf(data = cbd_poly, fill = NA, color = "black", linewidth = 1) +
  scale_fill_manual(values = c("0" = "#053061", "1" = "#f7f7f7"), labels = c("Control", "CBD Treatment"), name = "Zone") +
  theme_void() + labs(title = "Structural Validation: Negative Buffer Audit", subtitle = "Manhattan Decoupled from Brooklyn Piers")

ggsave("../../outputs/Structural_Validation_Map.png", struct_viz, width = 12, height = 9, dpi = 300)

message("--- ALL AUDITED ASSETS EXPORTED TO ../../outputs ---")