model_output <- read.csv("../../outputs/model_output.csv")

full_map_sf <- hex_grid %>%
  left_join(model_output, by = "hex_id")

# Geographic Clipping
options(tigris_use_cache = TRUE)
nyc_land <- counties(state = "NY", cb = TRUE) %>%
  filter(COUNTYFP %in% c("061", "047", "081", "005", "085")) %>%
  st_transform(st_crs(full_map_sf)) %>%
  st_union()

sig_level = 1.96 # 95% CI

full_map_sf$sig_level_met = abs(full_map_sf$t_stat) > sig_level


full_map_clipped <- st_intersection(full_map_sf, nyc_land)
cordon_boundary <- full_map_clipped %>% filter(in_zone == 1) %>% st_union() %>% st_cast("MULTILINESTRING")
max_val <- ceiling(max(abs(full_map_clipped$adj_cate), na.rm = TRUE) * 10) / 10

# Final Mapping
p_final <- ggplot(full_map_clipped) +
  geom_sf(fill = "#fdfdfd", color = "grey94", size = 0.01) +
  geom_sf(data = filter(full_map_clipped, sig_level_met == TRUE), aes(fill = adj_cate), color = NA) +
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
    caption = paste0(" | Significance: |t| > 1.645 | Boundary: Manhattan Cordon Line.")
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