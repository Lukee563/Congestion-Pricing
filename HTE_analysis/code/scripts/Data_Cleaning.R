rm(list=ls())
library(readr)
library(dplyr)
library(tidyr)
library(nasapower)
library(writexl)
library(tigris)
library(lubridate)
library(sf)
library(data.table)
library(purrr)

# Global Constants
START_DATE <- as.Date("2022-01-01")
END_DATE   <- as.Date("2026-04-01") 
TOLL_DATE  <- as.Date("2025-01-05")
ST_60_Y    <- 217000 # Northing for 60th St in EPSG:2263
study_weeks <- seq.Date(START_DATE, END_DATE, by = "week")

# Weather Import: Map over month labels to build raw dataset
labels <- crossing(y = 2021:2026, m = sprintf("%02d", 1:12)) %>%
  mutate(label = paste0(y, m)) %>%
  filter(label >= "202104" & label <= "202603") %>%
  pull(label)

weather_raw <- map(labels, ~{
  read_csv(paste0("../../data/raw/weather/", .x, ".csv"), show_col_types = FALSE)
}) %>% rbindlist(fill = TRUE)

# Aggregate Weather to weekly station-level observations
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

# Spatial Setup: Define NYC boundary and Hexagonal Tessellation
nyc_boundary <- counties("NY", cb = TRUE) %>%
  filter(NAME %in% c("New York", "Kings", "Queens", "Bronx")) %>% 
  st_transform(2263)

hex_grid <- st_make_grid(nyc_boundary, cellsize = 1640, square = FALSE) %>%
  st_sf() %>%
  mutate(hex_id = row_number()) %>%
  .[nyc_boundary, ] # Spatial subset to NYC boundary

# Hex-to-Borough Lookup using centroids
hex_borough_lookup <- hex_grid %>%
  st_centroid() %>% 
  st_join(nyc_boundary %>% select(BOROUGH_RECOVERED = NAME), join = st_nearest_feature) %>%
  st_drop_geometry() %>%
  select(hex_id, BOROUGH_RECOVERED) %>%
  distinct(hex_id, .keep_all = TRUE)

# Weather Station Mapping: Assign nearest station to each hex
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

# Collision Cleaning: Filter Staten Island and project to 2263
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

# Create balanced panel with zero-filled collision counts
collision_panel <- collisions_clean %>%
  group_by(hex_id, week) %>%
  summarise(y_count = n(), .groups = "drop") %>%
  complete(hex_id = hex_grid$hex_id, week = study_weeks, fill = list(y_count = 0))

# CBD Boundary: Strictly define treatment area using the official geofence
cbd_csv <- read_csv("../../data/raw/mta_shapefile.csv", show_col_types = FALSE)
cbd_polygon <- st_as_sf(cbd_csv, wkt = "polygon", crs = 4326) %>%
  st_union() %>%
  st_transform(2263)

# Feature Engineering: Centroids, distance to 60th, and in_zone flag
hex_centroids_2263 <- st_centroid(hex_grid)
hex_centroids_4326 <- st_transform(hex_centroids_2263, 4326)
in_cbd_matrix <- st_intersects(hex_centroids_2263, cbd_polygon, sparse = FALSE)

hex_features <- hex_centroids_2263 %>%
  mutate(
    x_coord   = st_coordinates(.)[,1],
    y_coord   = st_coordinates(.)[,2],
    lon       = st_coordinates(hex_centroids_4326)[,1],
    lat       = st_coordinates(hex_centroids_4326)[,2],
    dist_60th = abs(y_coord - ST_60_Y), 
    is_manh   = as.numeric(st_intersects(., nyc_boundary %>% filter(NAME == "New York"), sparse = FALSE)),
    in_zone   = as.numeric(in_cbd_matrix[, 1]) 
  ) %>%
  st_drop_geometry() %>%
  left_join(hex_station_map, by = "hex_id") %>%
  left_join(hex_borough_lookup, by = "hex_id")

# Final Master Join: Panel + Features + Weather
final_model_data <- collision_panel %>%
  left_join(hex_features, by = "hex_id") %>%
  left_join(weather_weekly, by = c("week", "nearest_station" = "station")) %>%
  mutate(
    borough   = toupper(BOROUGH_RECOVERED),
    post      = as.numeric(week >= TOLL_DATE),
    treatment = in_zone * post,
    week_index = as.numeric(week - min(week)) / 7
  ) %>%
  select(-BOROUGH_RECOVERED)

# Baseline Risk & Weather Imputation (Station-level and Global)
final_model_data <- final_model_data %>%
  group_by(hex_id) %>%
  mutate(baseline_risk = mean(y_count[week < TOLL_DATE], na.rm = TRUE)) %>%
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

# Export Outputs
write_csv(final_model_data, "../../data/cleaned/model_data_final.csv")
st_write(hex_grid, "../../data/cleaned/spatial_files/hex_grid.geojson", delete_dsn = TRUE)

county_sheets <- split(final_model_data, final_model_data$borough)

message("Processing complete for ", length(unique(final_model_data$hex_id)), " hexes.")

# Define the objects to keep for the modeling and mapping pipeline
keep_list <- c(
  "cbd_polygon",      # Treatment zone boundary
  "final_model_data", # Full cleaned panel
  "hex_grid"        # Spatial geometry
)

# Remove everything else and free up RAM
rm(list = setdiff(ls(), keep_list))
gc()

# Verification
message("Environment cleared. Objects remaining: ", paste(ls(), collapse = ", "))
