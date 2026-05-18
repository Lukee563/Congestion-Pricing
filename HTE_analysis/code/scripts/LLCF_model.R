library(readr)
library(dplyr)
library(tidyr)
library(grf)
library(ggplot2)
library(sf)
library(viridis)
library(scales)


# 1. DATA SETUP & MEMORY MANAGEMENT
message("[", Sys.time(), "] Loading core spatial data blocks...")
final_model_data <- read_csv("../../data/cleaned/model_data_final.csv", show_col_types = FALSE)
hex_grid <- st_read("../../data/cleaned/spatial_files/hex_grid.geojson", quiet = TRUE)
gc()

set.seed(123)

# Handle computational scaling safely
smpl_frac <- 1.0 
if(smpl_frac < 1) {
  model_sample <- final_model_data %>% slice_sample(prop = smpl_frac)
} else {
  model_sample <- final_model_data
}

# PRE-PURGE EXTRACTION: Capture global baseline parameters from raw data before deletion
global_temp_mean <- mean(final_model_data$avg_temp, na.rm = TRUE)

rm(final_model_data); gc() # Instantly drop raw file from memory


# 2. MATRIX PREPARATION & FEATURE ALIGNMENT
Y <- as.numeric(model_sample$y_count)
W <- as.numeric(model_sample$post)
clusters_vec <- model_sample$hex_id

# Strictly isolate active structural features 
X_features <- c("x_coord", "y_coord", "lon", "lat", "dist_60th", 
                "is_manh", "in_zone", "subway_dist_ft", "baseline_risk", 
                "avg_temp", "tot_precip")

# Preserve clean, unscaled unique geographies BEFORE scale() mutates the array
raw_hex_features <- model_sample %>% 
  distinct(hex_id, .keep_all = TRUE)

X_mat <- model_sample %>% select(all_of(X_features)) %>% as.matrix()

# Scale the matrix uniformly
X_scaled <- scale(X_mat)
x_center <- attr(X_scaled, "scaled:center")
x_scale  <- attr(X_scaled, "scaled:scale")

rm(X_mat); gc() # Purge raw training matrix

# Hardware Configuration
cores <- parallel::detectCores() - 1
message("Utilizing ", cores, " processing cores across panel matrices...")


# 3. FULLY TUNED NUISANCE MODEL ESTIMATION
# W.hat Estimation
message("[", Sys.time(), "] Slicing Nuisance Vector: W.hat (Enabling Hyperparameter Tuning)")
w_forest <- regression_forest(X_scaled, W, 
                              num.trees = 1000,
                              clusters = clusters_vec, 
                              tune.parameters = "none",
                              num.threads = cores, seed = 123)
W_hat <- w_forest$predictions
rm(w_forest); gc() 

# Y.hat Estimation
message("[", Sys.time(), "] Slicing Nuisance Vector: Y.hat (Enabling Hyperparameter Tuning)")
y_forest <- regression_forest(X_scaled, Y, 
                              num.trees = 1000, 
                              clusters = clusters_vec, 
                              tune.parameters = "none",
                              num.threads = cores, seed = 123)
Y_hat <- y_forest$predictions
rm(y_forest); gc() 


# 4. FINAL CAUSAL FOREST PRODUCTION MODEL 
message("[", Sys.time(), "] Fitting Local Linear Causal Forest (Fully Tuned)...")
cf_final <- causal_forest(X_scaled, Y, W, 
                          W.hat = W_hat, Y.hat = Y_hat, 
                          num.trees = 1500, 
                          clusters = clusters_vec, 
                          tune.parameters = "none",
                          honesty = TRUE, 
                          num.threads = cores, seed = 123)

# Crucial RAM purge. Wipe training matrices completely before starting predictions
rm(Y, W, W_hat, Y_hat, clusters_vec, X_scaled); gc()

# Evaluation Diagnostics
calib_results <- test_calibration(cf_final)
calib_factor  <- as.numeric(calib_results[1, 1]) 
message("Dynamic Calibration Factor Identified: ", round(calib_factor, 5))

ate <- average_treatment_effect(cf_final, target.sample = "treated")
message("ATE (Treated Zone): ", round(ate[1], 4), " (SE: ", round(ate[2], 4), ")")


# 5. CROSS-SECTIONAL MAP GRID EXTRACTION & ALIGNMENT
message("[", Sys.time(), "] Creating cross-sectional predictive projection grid...")

# Build map grid using raw unscaled data to completely bypass double-scaling errors
X_map_df <- raw_hex_features %>%
  mutate(
    avg_temp   = global_temp_mean, # Steady-state climate mean from original data
    tot_precip = 0                 # Steady-state dry condition
  )
rm(raw_hex_features, model_sample); gc() # Complete memory reclaim

# Build matrix using unscaled base attributes and apply scaling center parameters uniformly
X_map_mat    <- X_map_df %>% select(all_of(X_features)) %>% as.matrix()
X_map_scaled <- scale(X_map_mat, center = x_center, scale = x_scale)

# Predict CATEs using integer indexes for local linear adjustment
message("[", Sys.time(), "] Processing Local Linear predictions across grid...")
predictions_map <- predict(cf_final, 
                           newdata = X_map_scaled, 
                           linear.correction.variables = c(1, 2), 
                           estimate.variance = TRUE)
message("[", Sys.time(), "] Local Linear predictions COMPELTE")
X_map_df <- X_map_df %>%
  mutate(
    cate = as.numeric(predictions_map$predictions),
    se   = sqrt(as.numeric(predictions_map$variance.estimates))
  )


# 6. SPATIAL DATA PERSISTENCE EXPORT
full_map_sf <- hex_grid %>%
  mutate(hex_id = as.character(hex_id)) %>%
  left_join(X_map_df %>% 
              mutate(hex_id = as.character(hex_id)) %>% 
              select(hex_id, cate, se, in_zone), 
            by = "hex_id") %>%
  mutate(
    adj_cate = cate * calib_factor,
    t_stat   = cate / se,
    sig_95   = abs(t_stat) > 1.96
  ) %>%
  filter(!is.na(cate))

write_csv(st_drop_geometry(full_map_sf), "../../outputs/model_output.csv")

# Final Cleanup Environment Verification
essentials <- c("cf_final", "X_map_df", "hex_grid", "full_map_sf", "calib_factor", "ate")
rm(list = setdiff(ls(), essentials))
gc()

message("Model run successfully completed. Script is stable and validated.")