library(readr)
library(dplyr)
library(tidyr)
library(grf)
library(ggplot2)
library(sf)
library(viridis)
library(scales)
library(tigris)

# Load cross-sectional predictions
model_output <- read.csv("../../outputs/model_output_test.csv")

# Assume hex_grid is loaded or available in memory from previous stage
full_map_sf <- hex_grid %>%
  left_join(model_output, by = "hex_id")


# GEOGRAPHIC WATER MASKING & SHORELINE ISOLATION (STRICTLY LAND 4-BOROUGHS)
options(tigris_use_cache = TRUE)

# Fetch standard counties (Excludes Staten Island / FIPS 085)
nyc_counties <- counties(state = "NY", cb = TRUE) %>%
  filter(COUNTYFP %in% c("061", "047", "081", "005")) %>%
  st_transform(st_crs(full_map_sf))

# Fetch official water bodies to draw high-definition shorelines
nyc_fips <- c("061", "047", "081", "005")
nyc_water <- map_df(nyc_fips, ~area_water("NY", .x, year = 2022)) %>%
  st_transform(st_crs(full_map_sf)) %>%
  st_union()

# Establish strict land boundaries by subtracting water
nyc_land      <- st_difference(st_union(nyc_counties), nyc_water)
nyc_shoreline <- st_cast(nyc_land, "MULTILINESTRING") # Distinct shoreline borders

# Filter and set significance thresholds
sig_level = 1.96 
full_map_sf$sig_level_met = abs(full_map_sf$t_stat) > sig_level

# Filter hex grid using st_filter to prevent polygon edge distortion/dropping
full_map_clipped <- full_map_sf %>% 
  st_filter(nyc_land, .predicate = st_intersects)

# Isolate cordon line boundary
cordon_boundary <- full_map_clipped %>% 
  filter(in_zone == 1) %>% 
  st_union() %>% 
  st_cast("MULTILINESTRING")

max_val <- ceiling(max(abs(full_map_clipped$adj_cate), na.rm = TRUE) * 10) / 10
if(max_val == 0 || is.na(max_val)) max_val <- 0.3 # Safe fallback bounds


# DYNAMIC METRIC EXTRACTION FOR GRAPH CAPTION
message("[", Sys.time(), "] Computing policy zone statistics for chart text...")

# Filter strictly to the treated Cordon Zone
policy_zone_hexes <- full_map_clipped %>% filter(in_zone == 1)

# Extract counts based on direction and significance
pos_count     <- sum(policy_zone_hexes$adj_cate > 0, na.rm = TRUE)
sig_pos_count <- sum(policy_zone_hexes$adj_cate > 0 & policy_zone_hexes$sig_level_met == TRUE, na.rm = TRUE)

neg_count     <- sum(policy_zone_hexes$adj_cate < 0, na.rm = TRUE)
sig_neg_count <- sum(policy_zone_hexes$adj_cate < 0 & policy_zone_hexes$sig_level_met == TRUE, na.rm = TRUE)

# Calculate localized policy zone mean CATE
mean_zone_cate <- mean(policy_zone_hexes$adj_cate, na.rm = TRUE)

# Safely extract global ATE if it exists in environment, fallback to zone mean if absent
global_ate_val <- if(exists("ate")) ate[1] else mean_zone_cate

# Generate the formatted footnote text string
caption_text <- paste0(
  "Significance Threshold: |t| > ", sig_level, " (95% CI). Cordon Line: Manhattan Cordon Boundary.\n",
  "Policy Zone Metrics (Lower Manhattan): Total Hexes = ", nrow(policy_zone_hexes), 
  " | Positive CATE (Increases) = ", pos_count, " (Significant: ", sig_pos_count, ")",
  " | Negative CATE (Decreases) = ", neg_count, " (Significant: ", sig_neg_count, ")\n",
  "Policy Zone Mean CATE = ", round(mean_zone_cate, 4), " accidents/week per hex",
  " | Global Target Sample ATE = ", round(global_ate_val, 4), " accidents/week"
)


# VISUALIZATION
p_final <- ggplot(full_map_clipped) +
  # 1. Background Land Base (For non-significant hexes)
  geom_sf(fill = "#fcfcfc", color = "grey93", size = 0.05) +
  
  # 2. Significant CATE Hex Layers
  geom_sf(data = filter(full_map_clipped, sig_level_met == TRUE), aes(fill = adj_cate), color = NA) +
  
  # 3. Coastline Overlay (Separates treatment effects from neutral slate water)
  geom_sf(data = nyc_shoreline, color = "grey65", size = 0.35) +
  
  # 4. Central Business District Cordon Boundary Line
  geom_sf(data = cordon_boundary, color = "white", size = 1.2, alpha = 0.8) +
  geom_sf(data = cordon_boundary, color = "black", size = 0.6) +
  
  # 5. Continuous Diverging Palette Optimization
  scale_fill_gradient2(
    low = "#053061", mid = "#f7f7f7", high = "#67001f", midpoint = 0,
    limits = c(-max_val, max_val), 
    oob = scales::squish, 
    breaks = seq(-max_val, max_val, length.out = 5), 
    labels = label_number(accuracy = 0.01),
    name = "Adj. CATE\n(Accidents/Week)",
    guide = guide_colorbar(direction = "vertical", barheight = unit(7, "cm"), barwidth = unit(0.5, "cm"),
                           title.position = "top", frame.colour = "black", ticks.colour = "black")
  ) +
  theme_minimal() + 
  theme(
    panel.background = element_rect(fill = "#eaeef1", color = NA), # Neutral background water matrix
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    text = element_text(family = "serif"),
    plot.title = element_text(size = 22, face = "bold", margin = margin(t = 20, b = 10)),
    plot.subtitle = element_text(size = 13, color = "grey20", margin = margin(b = 25)),
    
    # Increased caption size and adjusted spacing for multi-line block text
    plot.caption = element_text(size = 11, color = "grey30", face = "plain", hjust = 0, lineheight = 1.3, margin = margin(t = 20)),
    
    # Legend layout panel assignment
    legend.position = "right", 
    legend.background = element_blank(),
    legend.margin = margin(l = 10, r = 10),
    
    plot.margin = margin(15, 15, 15, 15)
  ) +
  labs(
    title = "Spatial Heterogeneity of Congestion Pricing Impacts",
    subtitle = "Calibrated Causal Estimates Clipped with Shoreline Separation",
    caption = caption_text
  )

# Save Plot
ggsave("../../outputs/NYC_Causal_Impact.png", plot = p_final, width = 14, height = 15, dpi = 300, bg = "white")


# Clean Environment
essentials <- c("cf_final", "X_map_df", "hex_grid", "cbd_polygon", "x_center", "x_scale", "calib_factor")
rm(list = setdiff(ls(), essentials))
gc()

message("Post-model environment cleared. Dynamic policy statistics added to the final figure caption.")