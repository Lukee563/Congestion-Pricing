library(readr); library(dplyr); library(tidyr)
library(grf); library(sf)

# 1. LOAD
message("[", Sys.time(), "] Loading panel...")
final_model_data <- read_csv("../../data/cleaned/model_data_final.csv", show_col_types = FALSE)
hex_grid <- st_read("../../data/cleaned/spatial_files/hex_grid.geojson", quiet = TRUE)
set.seed(123)
model_sample <- final_model_data

global_temp_mean   <- mean(final_model_data$avg_temp,   na.rm = TRUE)
global_precip_mean <- mean(final_model_data$tot_precip, na.rm = TRUE)
rm(final_model_data); gc()

# 2. MATRICES
Y <- as.numeric(model_sample$y_count)
W <- as.numeric(model_sample$treatment)
clusters_vec <- model_sample$hex_id

# Causal-forest covariates: spatial + controls, NO time (keeps heterogeneity spatial)
X_features <- c("x_coord", "y_coord", "is_manh", "subway_dist_ft",
                "baseline_risk", "avg_temp", "tot_precip")

# Nuisance covariates: same PLUS week_index, so Y.hat/W.hat absorb the time trend
X_nuis_features <- c(X_features, "week_index")

raw_hex_features <- model_sample %>% distinct(hex_id, .keep_all = TRUE)

X_mat    <- model_sample %>% select(all_of(X_features))      %>% as.matrix()
X_nuis   <- model_sample %>% select(all_of(X_nuis_features)) %>% as.matrix() %>% scale()

X_scaled <- scale(X_mat)
x_center <- attr(X_scaled, "scaled:center")
x_scale  <- attr(X_scaled, "scaled:scale")
rm(X_mat); gc()

cores <- parallel::detectCores() - 1

# 3. TIME-AWARE NUISANCES
message("[", Sys.time(), "] W.hat (time-aware)...")
w_forest <- regression_forest(X_nuis, W, num.trees = 500, clusters = clusters_vec,
                              tune.parameters = "none", num.threads = cores, seed = 123)
W_hat <- w_forest$predictions; rm(w_forest); gc()

message("[", Sys.time(), "] Y.hat (time-aware)...")
y_forest <- regression_forest(X_nuis, Y, num.trees = 500, clusters = clusters_vec,
                              tune.parameters = "none", num.threads = cores, seed = 123)
Y_hat <- y_forest$predictions; rm(y_forest); gc()

# Quick orthogonalization diagnostic — did the time control help?
message("Y.hat fit (cor with Y): ", round(cor(Y, Y_hat), 4))

# 4. CAUSAL FOREST — larger leaves so each contains treated units
message("[", Sys.time(), "] Causal forest...")
cf_final <- causal_forest(X_scaled, Y, W,
                          W.hat = W_hat, Y.hat = Y_hat,
                          num.trees = 1500,
                          min.node.size = 30,
                          clusters = clusters_vec,
                          tune.parameters = "none",
                          honesty = TRUE,
                          num.threads = cores, seed = 123)
rm(Y, W, W_hat, Y_hat, X_nuis, X_scaled); gc()

# 5. DIAGNOSTICS — the numbers that actually decide the paper
message("\n=== test_calibration ===")
print(test_calibration(cf_final))

ate_all  <- average_treatment_effect(cf_final, target.sample = "overlap")
ate_zone <- average_treatment_effect(cf_final, target.sample = "overlap",
                                     subset = (model_sample$in_zone == 1))
message("Overlap ATE (all):    ", round(ate_all[1], 4),  " (SE ", round(ate_all[2], 4),  ")")
message("Overlap ATE (in-zone):", round(ate_zone[1], 4), " (SE ", round(ate_zone[2], 4), ")")

# 6. PER-HEX MAP
message("[", Sys.time(), "] Per-hex predictions...")
X_map_df  <- raw_hex_features %>% mutate(avg_temp = global_temp_mean, tot_precip = global_precip_mean)
X_map_mat <- X_map_df %>% select(all_of(X_features)) %>% as.matrix()
X_map_scaled <- scale(X_map_mat, center = x_center, scale = x_scale)

pred_llcf <- predict(cf_final, newdata = X_map_scaled,
                     linear.correction.variables = c(1, 2))           # tau (LLCF)
pred_var  <- predict(cf_final, newdata = X_map_scaled,
                     estimate.variance = TRUE)                        # variance (standard CF)

X_map_df <- X_map_df %>%
  mutate(cate = as.numeric(pred_llcf$predictions),
         se   = sqrt(as.numeric(pred_var$variance.estimates)),
         t_stat = cate / se,
         sig_95 = abs(t_stat) > 1.96)

message("Significant hexes: ", sum(X_map_df$sig_95), " / ", nrow(X_map_df))

# 7. EXPORT
full_map_sf <- hex_grid %>%
  mutate(hex_id = as.character(hex_id)) %>%
  left_join(X_map_df %>% mutate(hex_id = as.character(hex_id)) %>%
              select(hex_id, cate, se, t_stat, sig_95, in_zone), by = "hex_id") %>%
  filter(!is.na(cate))

write_csv(st_drop_geometry(full_map_sf), "../../outputs/model_output.csv")
message("[", Sys.time(), "] Done.")