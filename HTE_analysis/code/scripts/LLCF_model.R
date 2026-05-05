library(readr)
library(dplyr)
library(tidyr)
library(grf)
library(ggplot2)
library(sf)
library(viridis)
library(scales)
library(tigris)

# Data Setup
final_model_data <- read_csv("../../data/cleaned/model_data_final.csv", show_col_types = FALSE)
hex_grid <- st_read("../../data/cleaned/spatial_files/hex_grid.geojson")

gc()

set.seed(123)

# Sampling for low-compute machines
smpl_frac <- 1
model_sample <- final_model_data %>%
  slice_sample(prop = smpl_frac)


# Matrix Preparation
Y <- as.numeric(model_sample$y_count)
W <- as.numeric(model_sample$treatment)
X <- model_sample %>% 
  select(baseline_risk, y_coord, dist_60th, week_index, avg_temp, tot_precip) %>% 
  as.matrix()

X_scaled <- scale(X)
x_center <- attr(X_scaled, "scaled:center")
x_scale  <- attr(X_scaled, "scaled:scale")

# Hardware Optimization
cores <- parallel::detectCores() - 1
message("Utilizing ", cores, " cores...")

# Train Nuisance Models
message("[", Sys.time(), "] Training W.hat")
w_forest <- regression_forest(X_scaled, W, num.trees = 2000, clusters = model_sample$hex_id, 
                              tune.parameters = "all", num.threads = cores, seed = 123)
W_hat <- w_forest$predictions

message("[", Sys.time(), "] Training Y.hat")
y_forest <- regression_forest(X_scaled, Y, num.trees = 2000, clusters = model_sample$hex_id, 
                              tune.parameters = "all", num.threads = cores, seed = 123)
Y_hat <- y_forest$predictions

# Train Causal Forest
message("[", Sys.time(), "] Training Causal Forest")
cf_final <- causal_forest(X_scaled, Y, W, W.hat = W_hat, Y.hat = Y_hat, num.trees = 2000, 
                          clusters = model_sample$hex_id, tune.parameters = "all", 
                          honesty = TRUE, num.threads = cores, seed = 123)

# Global Diagnostics & Dynamic Calibration Calculation
# test_calibration returns a coefficient for 'mean.forest.prediction'
# This is the scaling factor to ensure predicted effects match observed magnitude.
calib_results <- test_calibration(cf_final)
calib_factor <- as.numeric(calib_results[1, 1]) 
message("Dynamic Calibration Factor Identified: ", round(calib_factor, 5))

ate <- average_treatment_effect(cf_final, target.sample = "treated")
message("ATE: ", round(ate[1], 4), " (SE: ", round(ate[2], 4), ")")

# Create Prediction Grid
post_treatment_weeks <- model_sample$week_index[model_sample$treatment == 1]
target_week <- median(post_treatment_weeks)

X_map_df <- model_sample %>%
  distinct(hex_id, .keep_all = TRUE) %>%
  mutate(
    week_index = target_week,
    avg_temp = mean(model_sample$avg_temp[model_sample$week_index == target_week], na.rm=TRUE),
    tot_precip = mean(model_sample$tot_precip[model_sample$week_index == target_week], na.rm=TRUE)
  ) %>%
  mutate(
    avg_temp = if_else(is.na(avg_temp), mean(model_sample$avg_temp, na.rm=TRUE), avg_temp),
    tot_precip = if_else(is.na(tot_precip), 0, tot_precip)
  )

X_map_mat <- X_map_df %>%
  select(baseline_risk, y_coord, dist_60th, week_index, avg_temp, tot_precip) %>%
  as.matrix()

X_map_scaled <- scale(X_map_mat, center = x_center, scale = x_scale)

# Predict CATEs
predictions_map <- predict(cf_final, newdata = X_map_scaled, linear.correction.variables = c(2, 3), estimate.variance = TRUE)

X_map_df <- X_map_df %>%
  mutate(
    cate = as.numeric(predictions_map$predictions),
    se = sqrt(as.numeric(predictions_map$variance.estimates))
  )

# Spatial Data Prep and Dynamic Calibration Application
full_map_sf <- hex_grid %>%
  mutate(hex_id = as.character(hex_id)) %>%
  left_join(X_map_df %>% mutate(hex_id = as.character(hex_id)) %>% 
              select(hex_id, cate, se, in_zone), by = "hex_id") %>%
  mutate(
    # Applying the dynamically identified factor
    adj_cate = cate * calib_factor,
    t_stat = cate / se,
    sig_90 = abs(t_stat) > 1.645
  ) %>%
  filter(!is.na(cate))

write_csv(full_map_sf, "../../outputs/model_output_test.csv")

# Define core results and spatial data to persist
essentials <- c(
  "cf_final",         # The trained Causal Forest model
  "X_map_df",         # The dataframe containing my CATE predictions
  "hex_grid",         # Spatial hexagons
  "cbd_polygon",      # Cordon boundary
  "x_center",         # Scaling center
  "x_scale",          # Scaling factor
  "calib_factor"      # Dynamic calibration value
)

# Remove nuisance forests (W_hat/Y_hat models) and large training matrices
rm(list = setdiff(ls(), essentials))

# garbage collection
gc()

# Verification
message("Post-model environment cleared. Memory optimized for mapping.")
print(ls())