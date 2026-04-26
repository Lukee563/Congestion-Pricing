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

# Geographic Clipping
options(tigris_use_cache = TRUE)
nyc_land <- counties(state = "NY", cb = TRUE) %>%
  filter(COUNTYFP %in% c("061", "047", "081", "005", "085")) %>%
  st_transform(st_crs(full_map_sf)) %>%
  st_union()

full_map_clipped <- st_intersection(full_map_sf, nyc_land)
cordon_boundary <- full_map_clipped %>% filter(in_zone == 1) %>% st_union() %>% st_cast("MULTILINESTRING")
max_val <- ceiling(max(abs(full_map_clipped$adj_cate), na.rm = TRUE) * 10) / 10

# Final Mapping
p_final <- ggplot(full_map_clipped) +
  geom_sf(fill = "#fdfdfd", color = "grey94", size = 0.01) +
  geom_sf(data = filter(full_map_clipped, sig_90 == TRUE), aes(fill = adj_cate), color = NA) +
  geom_sf(data = cordon_boundary, color = "white", size = 1.1, alpha = 0.8) +
  geom_sf(data = cordon_boundary, color = "black", size = 0.5) +
  scale_fill_gradient2(
    low = "#2166ac", mid = "white", high = "#b2182b", midpoint = 0,
    limits = c(-max_val, max_val), breaks = seq(-max_val, max_val, length.out = 5),
    name = "Adj. CATE\n(Accidents/Week)",
    guide = guide_colorbar(direction = "vertical", barheight = unit(5, "cm"), barwidth = unit(0.5, "cm"),
                           title.position = "top", frame.colour = "black", ticks.colour = "black")
  ) +
  theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "#d1e3f0", color = NA), 
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "serif"),
    plot.title = element_text(size = 22, face = "bold", margin = margin(t = 20, b = 10)),
    plot.subtitle = element_text(size = 13, color = "grey20", margin = margin(b = 25)),
    plot.caption = element_text(size = 9, color = "grey40", face = "italic", margin = margin(t = 20)),
    legend.position = c(0.12, 0.28), 
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  labs(
    title = "Spatial Heterogeneity of Congestion Pricing Impacts",
    subtitle = "Calibrated Causal Estimates Clipped to NYC Shoreline (90% CL)",
    caption = paste0("Dynamic Calibration: ", round(calib_factor, 4), 
                     " | Significance: |t| > 1.645 | Boundary: Manhattan Cordon Line.")
  )

# Save Outputs
ggsave("../../outputs/NYC_Causal_Impact.png", plot = p_final, width = 12, height = 15, dpi = 300, bg = "white")
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