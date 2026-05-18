rm(list=ls())
library(readr)
library(dplyr)
library(tidyr)
library(tigris)
library(lubridate)
library(sf)
library(data.table)
library(purrr)


# 1. BASE MATRIX CONSTRUCTION (RUNS ONCE)
spatial_construction <- function(){
  START_DATE  <- as.Date("2022-01-01")
  END_DATE    <- as.Date("2026-04-01") 
  ST_60_Y     <- 217000 
  study_weeks <- seq.Date(START_DATE, END_DATE, by = "week")
  
  message("[", Sys.time(), "] Loading and aggregating raw weather inputs...")
  
  labels <- crossing(y = 2021:2026, m = sprintf("%02d", 1:12)) %>%
    mutate(label = paste0(y, m)) %>%
    filter(label >= "202104" & label <= "202603") %>%
    pull(label)
  
  weather_raw <- map(labels, ~{
    read_csv(paste0("../../data/raw/weather/", .x, ".csv"), show_col_types = FALSE)
  }) %>% rbindlist(fill = TRUE)
  
  weather_weekly <- weather_raw %>%
    mutate(
      date = as.Date(time),
      week = floor_date(date, "week", week_start = 6)
    ) %>%
    filter(week %in% study_weeks) %>% 
    group_by(week, station) %>%
    summarise(
      avg_temp      = mean(`temp_2m_avg [degF]`, na.rm = TRUE),
      avg_humidity  = mean(`relative_humidity_avg [percent]`, na.rm = TRUE),
      tot_precip    = if(all(is.na(`precip_total [inch]`))) NA else sum(`precip_total [inch]`, na.rm = TRUE),
      obs_count     = n(),
      .groups = "drop"
    )
  
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
  
  message("[", Sys.time(), "] Mapping weather stations and subway networks to hex grid...")
  stations_lookup <- weather_raw %>%
    distinct(station, .keep_all = TRUE) %>%
    filter(!is.na(`latitude [degrees_north]`)) %>%
    st_as_sf(coords = c("longitude [degrees_east]", "latitude [degrees_north]"), crs = 4326) %>%
    st_transform(2263)
  
  nearest_idx <- st_nearest_feature(hex_grid, stations_lookup)
  hex_station_map <- data.frame(
    hex_id = hex_grid$hex_id,
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
    hex_id = hex_grid$hex_id,
    subway_dist_ft = as.numeric(distances)
  )
  
  message("[", Sys.time(), "] Processing collision files into a balanced land-only panel...")
  MotorCollisions <- read_csv("../../data/raw/MotorCollisions.csv", show_col_types = FALSE) %>%
    filter(BOROUGH != "STATEN ISLAND")
  
  collisions_clean <- MotorCollisions %>%
    mutate(date = mdy(`CRASH DATE`),
           week = floor_date(date, "week", week_start = 6)) %>% 
    filter(date >= START_DATE, date <= END_DATE, 
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
    left_join(hex_station_map, by = "hex_id") %>%
    left_join(hex_subway_map, by = 'hex_id') %>%
    left_join(hex_borough_lookup, by = "hex_id")
  
  final_panel <- collision_panel %>%
    left_join(hex_features, by = "hex_id") %>%
    left_join(weather_weekly, by = c("week", "nearest_station" = "station"))
  
  return(final_panel)
}

# Generate baseline data matrix globally once
base_panel_matrix <- spatial_construction()
gc()


# 2. ISOLATED COMPILATION FUNCTION (ANTI-CONTAMINATION)
data_prep <- function(base_data, target_date, true_policy_date = "2025-01-05") {
  message("[", Sys.time(), "] Constructing assignment vectors for target date: ", target_date)
  
  processed_data <- base_data %>%
    mutate(
      borough    = toupper(BOROUGH_RECOVERED),
      post       = as.numeric(week >= as.Date(target_date)),
      treatment  = in_zone * post,
      week_index = as.numeric(week - min(week)) / 7
    ) %>%
    select(-BOROUGH_RECOVERED)
  
  # baseline_risk is strictly pinned to weeks BEFORE the TRUE policy date
  processed_data <- processed_data %>%
    group_by(hex_id) %>%
    mutate(baseline_risk = mean(y_count[week < as.Date(true_policy_date)], na.rm = TRUE)) %>%
    ungroup() %>%
    group_by(nearest_station) %>%
    mutate(
      avg_temp = if_else(is.na(avg_temp) | is.nan(avg_temp),
                         ifelse(all(is.na(avg_temp)), NA_real_, mean(avg_temp, na.rm = TRUE)),
                         avg_temp),
      tot_precip = if_else(is.na(tot_precip) | is.nan(tot_precip),
                           ifelse(all(is.na(tot_precip)), 0, mean(tot_precip, na.rm = TRUE)),
                           tot_precip)
    ) %>%
    ungroup() %>%
    mutate(
      avg_temp = if_else(is.na(avg_temp), mean(avg_temp, na.rm = TRUE), avg_temp),
      tot_precip = if_else(is.na(tot_precip), 0, tot_precip)
    )
  
  # Target execution export paths
  if(target_date == true_policy_date){
    write_csv(processed_data, "../../data/cleaned/model_data_final.csv")
  } else {
    write_csv(processed_data, paste0("../../data/cleaned/placebos/placebo_date_", target_date, ".csv"))
  }
  
  message("Processing complete for timeline anchor: ", target_date)
}


# 3. ITERATION & PIPELINE EXECUTION
placebos <- c("2023-01-05", "2023-04-05", "2024-01-20", "2024-06-05")
TRUE_POLICY_DATE <- "2025-01-05"

# Build all placebo conditions
for(p in placebos){
  data_prep(base_panel_matrix, target_date = p, true_policy_date = TRUE_POLICY_DATE)
}

# Build true baseline data condition
data_prep(base_panel_matrix, target_date = TRUE_POLICY_DATE, true_policy_date = TRUE_POLICY_DATE)

# Drop base panel matrix to free up RAM before summary compilation
rm(base_panel_matrix); gc()


# 4. MEMORY-SAFE AGGREGATION & SUMMARY TABLE GEN
message("[", Sys.time(), "] Compiling diagnostic summary parameters...")

# Pre-allocate tracking summary data frame
summary_table <- data.frame(
  run_type = c(paste0("Placebo (", placebos, ")"), "True Policy (2025-01-05)"),
  file_path = c(paste0("../../data/cleaned/placebos/placebo_date_", placebos, ".csv"), 
                "../../data/cleaned/model_data_final.csv"),
  treated_observations = NA
)

# RAM-Safe Streaming: Open, sum treatment vectors, and close file immediately
for(row in 1:nrow(summary_table)){
  temp_stream <- read_csv(summary_table$file_path[row], col_types = cols_only(treatment = col_double()))
  summary_table$treated_observations[row] <- sum(temp_stream$treatment, na.rm = TRUE)
  rm(temp_stream); gc()
}

# Display your finalized policy evaluation footprint
print(summary_table %>% select(-file_path))

library(readr)
library(dplyr)
library(tidyr)
library(grf)
library(ggplot2)
library(sf)
library(viridis)
library(scales)


# CONFIGURATION & SYSTEM DEFENSE

set.seed(123)
cores <- parallel::detectCores() - 1

# SYSTEM FIX: Force R to create the directory structure if missing
if(!dir.exists("../../outputs/placebos")) {
  dir.create("../../outputs/placebos", recursive = TRUE)
}

# Define targets for your falsification loop
placebo_dates <- c("2023-01-05", "2023-04-05", "2024-01-20", "2024-06-05")

# Read hex grid globally once so we don't hit the disk on every single loop
hex_grid <- st_read("../../data/cleaned/spatial_files/hex_grid.geojson", quiet = TRUE)

# Establish a long-term tracker frame to store tabular metrics summary
falsification_summary_path <- "../../outputs/placebos/falsification_summary_metrics.csv"
summary_tracker <- data.frame(
  target_date = placebo_dates,
  ate_estimate = NA_real_,
  standard_error = NA_real_,
  t_statistic = NA_real_,
  calibration_factor = NA_real_
)

# REFACTORED FEATURES REGISTRY: 
# 'in_zone' and 'week_index' are removed from the forest matrix to prevent perfect
# separation. Time trends are handled via linear residualization instead.
X_features <- c("x_coord", "y_coord", "lon", "lat", "dist_60th", 
                "is_manh", "subway_dist_ft", "baseline_risk", 
                "avg_temp", "tot_precip")


# LEAN & BALANCED INDUSTRIAL LOOP
for(p_date in placebo_dates) {
  
  target_dt <- as.Date(p_date)
  message("\n======================================================================")
  message("[", Sys.time(), "] STARTING BALANCED FOREST FOR PLACEBO DATE: ", p_date)
  message("======================================================================")
  
  # 1. READ TARGET DATA RUN
  target_file_path <- paste0("../../data/cleaned/placebos/placebo_date_", p_date, ".csv")
  
  if(!file.exists(target_file_path)) {
    warning("File missing for target date: ", target_file_path, " skipping to next iteration.")
    next
  }
  
  # DYNAMIC WINDOW SLICE: Take a balanced 6-month window before/after the fake date
  model_sample <- read_csv(target_file_path, show_col_types = FALSE) %>%
    filter(week >= (target_dt - 180) & week <= (target_dt + 180))
  
  if(length(unique(model_sample$post)) < 2) {
    warning("Insufficient timeline variance for date: ", p_date, " skipping.")
    next
  }
  
  global_temp_mean <- mean(model_sample$avg_temp, na.rm = TRUE)
  
  # TIME-TREND EXTRACTION
  # We regress out the linear time trend across weeks before handing the data to GRF.
  # This filters out the trend without letting the forest cheat on W_hat.
  time_trend_model <- lm(y_count ~ week_index, data = model_sample)
  
  # 2. MATRIX ISOLATION
  Y <- as.numeric(residuals(time_trend_model)) # Detrended outcome
  W <- as.numeric(model_sample$post)
  clusters_vec <- model_sample$hex_id
  
  raw_hex_features <- model_sample %>% distinct(hex_id, .keep_all = TRUE)
  X_mat <- model_sample %>% select(all_of(X_features)) %>% as.matrix()
  
  X_scaled <- scale(X_mat)
  x_center <- attr(X_scaled, "scaled:center")
  x_scale  <- attr(X_scaled, "scaled:scale")
  
  rm(X_mat, model_sample, time_trend_model); gc()
  
  # 3. ESTIMATE NUISANCE VECTORS 
  message("[", Sys.time(), "] Slicing Nuisance Vector: W.hat")
  w_forest <- regression_forest(X_scaled, W, num.trees = 1000, clusters = clusters_vec, 
                                tune.parameters = "none", num.threads = cores, seed = 123)
  W_hat <- w_forest$predictions
  rm(w_forest); gc() 
  
  # Hard guard against extreme edge propensities forcing lm.wfit failures
  W_hat <- pmax(pmin(W_hat, 0.99), 0.01)
  
  message("[", Sys.time(), "] Slicing Nuisance Vector: Y.hat")
  y_forest <- regression_forest(X_scaled, Y, num.trees = 1000, clusters = clusters_vec, 
                                tune.parameters = "none", num.threads = cores, seed = 123)
  Y_hat <- y_forest$predictions
  rm(y_forest); gc() 
  
  # 4. FINAL CAUSAL FOREST PRODUCTION MODEL 
  message("[", Sys.time(), "] Fitting Local Linear Causal Forest structure...")
  cf_final <- causal_forest(X_scaled, Y, W, W.hat = W_hat, Y.hat = Y_hat, 
                            num.trees = 1500, clusters = clusters_vec, 
                            tune.parameters = "none", honesty = TRUE, 
                            num.threads = cores, seed = 123)
  
  rm(Y, W, W_hat, Y_hat, clusters_vec, X_scaled); gc()
  
  # 5. DIAGNOSTICS & METRIC STORAGE
  calib_results <- test_calibration(cf_final)
  calib_factor  <- as.numeric(calib_results[1, 1]) 
  
  ate <- average_treatment_effect(cf_final, target.sample = "treated")
  
  summary_tracker[summary_tracker$target_date == p_date, "ate_estimate"]      <- ate[1]
  summary_tracker[summary_tracker$target_date == p_date, "standard_error"]     <- ate[2]
  summary_tracker[summary_tracker$target_date == p_date, "t_statistic"]        <- ate[1] / ate[2]
  summary_tracker[summary_tracker$target_date == p_date, "calibration_factor"] <- calib_factor
  
  message(">>> ATE Results for ", p_date, ": ", round(ate[1], 4), " (SE: ", round(ate[2], 4), ") | Calib: ", round(calib_factor, 4))
  
  # 6. SPATIAL MAP PROJECTION EXTRACTION
  message("[", Sys.time(), "] Creating cross-sectional predictive projection grid...")
  X_map_df <- raw_hex_features %>%
    mutate(avg_temp = global_temp_mean, tot_precip = 0)
  rm(raw_hex_features); gc()
  
  X_map_mat    <- X_map_df %>% select(all_of(X_features)) %>% as.matrix()
  X_map_scaled <- scale(X_map_mat, center = x_center, scale = x_scale)
  
  message("[", Sys.time(), "] Processing Local Linear predictions across grid...")
  predictions_map <- predict(cf_final, newdata = X_map_scaled, 
                             linear.correction.variables = c(1, 2), estimate.variance = TRUE)
  
  X_map_df <- X_map_df %>%
    mutate(cate = as.numeric(predictions_map$predictions),
           se   = sqrt(as.numeric(predictions_map$variance.estimates)))
  
  # 7. EXPORT DATA LAYER PERSISTENCE (DATE-APPENDED OUTPUTS)
  full_map_sf <- hex_grid %>%
    mutate(hex_id = as.character(hex_id)) %>%
    left_join(X_map_df %>% 
                mutate(hex_id = as.character(hex_id)) %>% 
                select(hex_id, cate, se), by = "hex_id") %>%
    mutate(adj_cate = cate * calib_factor,
           t_stat   = cate / se,
           sig_95   = abs(t_stat) > 1.96) %>%
    filter(!is.na(cate))
  
  write_sf(full_map_sf, paste0("../../outputs/placebos/spatial_output_", p_date, ".geojson"))
  write_csv(st_drop_geometry(full_map_sf), paste0("../../outputs/placebos/model_output_", p_date, ".csv"))
  
  # 8. HARD RECLAIM ENV MEMORY
  rm(cf_final, X_map_df, X_map_mat, X_map_scaled, predictions_map, full_map_sf, calib_results, calib_factor, ate); gc()
  
  message("[", Sys.time(), "] Loop run completed and memory fully reclaimed for date: ", p_date)
}

# POST-LOOP EVALUATION STATS SAVE
message("\n[", Sys.time(), "] All falsification loop passes successfully executed.")
write_csv(summary_tracker, falsification_summary_path)

print(summary_tracker)