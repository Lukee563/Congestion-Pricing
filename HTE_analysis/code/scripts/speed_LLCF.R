# =============================================================================
# LLCF_speed_model.R
# Local Linear Causal Forest — avg_speed as outcome
# Mirrors LLCF_model.R exactly, substituting speed_model_data for collision data
# =============================================================================

library(readr)
library(dplyr)
library(tidyr)
library(grf)
library(ggplot2)
library(sf)
library(viridis)
library(scales)


# 1. DATA SETUP & MEMORY MANAGEMENT
message("[", Sys.time(), "] Loading speed model data...")
speed_model_data <- read_csv("../../data/cleaned/speed_model_data.csv", show_col_types = FALSE)
hex_grid <- st_read("../../data/cleaned/spatial_files/hex_grid.geojson", quiet = TRUE)
gc()

set.seed(123)

smpl_frac <- 1.0
if (smpl_frac < 1) {
  model_sample <- speed_model_data %>% slice_sample(prop = smpl_frac)
} else {
  model_sample <- speed_model_data
}

# Drop rows with missing outcome
model_sample <- model_sample %>% filter(!is.na(avg_speed))

global_temp_mean <- mean(speed_model_data$avg_temp, na.rm = TRUE)
rm(speed_model_data); gc()


# 2. MATRIX PREPARATION & FEATURE ALIGNMENT
Y <- as.numeric(model_sample$avg_speed)
W <- as.numeric(model_sample$post)
clusters_vec <- model_sample$hex_id

X_features <- c("x_coord", "y_coord", "lon", "lat", "dist_60th",
                "is_manh", "in_zone", "subway_dist_ft", "baseline_speed",
                "avg_temp", "tot_precip")

raw_hex_features <- model_sample %>%
  distinct(hex_id, .keep_all = TRUE)

X_mat    <- model_sample %>% select(all_of(X_features)) %>% as.matrix()
X_scaled <- scale(X_mat)
x_center <- attr(X_scaled, "scaled:center")
x_scale  <- attr(X_scaled, "scaled:scale")

rm(X_mat); gc()

cores <- parallel::detectCores() - 1
message("Utilizing ", cores, " processing cores...")


# 3. NUISANCE MODEL ESTIMATION
message("[", Sys.time(), "] Estimating W.hat...")
w_forest <- regression_forest(X_scaled, W,
                              num.trees = 1000,
                              clusters = clusters_vec,
                              tune.parameters = "none",
                              num.threads = cores, seed = 123)
W_hat <- w_forest$predictions
rm(w_forest); gc()

message("[", Sys.time(), "] Estimating Y.hat...")
y_forest <- regression_forest(X_scaled, Y,
                              num.trees = 1000,
                              clusters = clusters_vec,
                              tune.parameters = "none",
                              num.threads = cores, seed = 123)
Y_hat <- y_forest$predictions
rm(y_forest); gc()


# 4. CAUSAL FOREST
message("[", Sys.time(), "] Fitting Local Linear Causal Forest...")
cf_final <- causal_forest(X_scaled, Y, W,
                          W.hat = W_hat, Y.hat = Y_hat,
                          num.trees = 1500,
                          clusters = clusters_vec,
                          tune.parameters = "none",
                          honesty = TRUE,
                          num.threads = cores, seed = 123)

rm(Y, W, W_hat, Y_hat, clusters_vec, X_scaled); gc()

calib_results <- test_calibration(cf_final)
calib_factor  <- as.numeric(calib_results[1, 1])
message("Calibration Factor: ", round(calib_factor, 5))

ate <- average_treatment_effect(cf_final, target.sample = "treated")
message("ATE (Treated Zone): ", round(ate[1], 4), " (SE: ", round(ate[2], 4), ")")


# 5. PREDICTION GRID
message("[", Sys.time(), "] Building prediction grid...")

X_map_df <- raw_hex_features %>%
  mutate(
    avg_temp   = global_temp_mean,
    tot_precip = 0
  )
rm(raw_hex_features, model_sample); gc()

X_map_mat    <- X_map_df %>% select(all_of(X_features)) %>% as.matrix()
X_map_scaled <- scale(X_map_mat, center = x_center, scale = x_scale)

message("[", Sys.time(), "] Generating Local Linear predictions...")
predictions_map <- predict(cf_final,
                           newdata = X_map_scaled,
                           linear.correction.variables = c(1, 2),
                           estimate.variance = TRUE)
message("[", Sys.time(), "] Predictions complete.")

X_map_df <- X_map_df %>%
  mutate(
    cate = as.numeric(predictions_map$predictions),
    se   = sqrt(as.numeric(predictions_map$variance.estimates))
  )


# 6. SPATIAL EXPORT
full_map_sf <- hex_grid %>%
  mutate(hex_id = as.character(hex_id)) %>%
  left_join(
    X_map_df %>%
      mutate(hex_id = as.character(hex_id)) %>%
      select(hex_id, cate, se, in_zone),
    by = "hex_id"
  ) %>%
  mutate(
    adj_cate = cate * calib_factor,
    t_stat   = cate / se,
    sig_95   = abs(t_stat) > 1.96
  ) %>%
  filter(!is.na(cate))

write_csv(st_drop_geometry(full_map_sf), "../../outputs/speed_model_output.csv")

essentials <- c("cf_final", "X_map_df", "hex_grid", "full_map_sf", "calib_factor", "ate")
rm(list = setdiff(ls(), essentials))
gc()

message("Speed model complete.")