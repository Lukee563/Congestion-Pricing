# generate_maps.R
# Produces collision and speed CATE maps with unified color convention:
#   Red  = worse outcome (more accidents / slower speeds)
#   Blue = better outcome (fewer accidents / faster speeds)
# Run after LLCF models with hex_grid in environment
library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(scales)
library(tigris)
library(purrr)

options(tigris_use_cache = TRUE)
# Shared spatial setup (built once, used by both maps)
message("[", Sys.time(), "] Building shared spatial layers...")

nyc_counties <- counties(state = "NY", cb = TRUE) %>%
  filter(COUNTYFP %in% c("061", "047", "081", "005")) %>%
  st_transform(st_crs(hex_grid))

nyc_water <- map_df(c("061", "047", "081", "005"), ~area_water("NY", .x, year = 2022)) %>%
  st_transform(st_crs(hex_grid)) %>%
  st_union()

nyc_land      <- st_difference(st_union(nyc_counties), nyc_water)
nyc_shoreline <- st_cast(nyc_land, "MULTILINESTRING")

# Shared map builder
build_map <- function(output_csv,
                      max_val      = NULL,   # NULL → data-driven symmetric limit
                      low_color,             # color for negative CATE
                      high_color,            # color for positive CATE
                      legend_label,
                      title,
                      subtitle,
                      outcome_label,         # e.g. "accidents/week" or "mph"
                      ate_val) {
  
  model_output <- read.csv(output_csv)
  
  map_sf <<- hex_grid %>%
    left_join(model_output, by = "hex_id") %>%
    mutate(sig_95 = abs(t_stat) > 1.96) %>%      # honest 95% flag, caption only
    st_filter(nyc_land, .predicate = st_intersects)
  
  # Data-driven symmetric color limit: 98th pct of |CATE|, so a few outlier
  # hexes don't stretch the scale and wash the whole map to neutral.
  if (is.null(max_val)) {
    max_val <- as.numeric(quantile(abs(map_sf$adj_cate), 0.98, na.rm = TRUE))
  }
  
  cordon_boundary <- map_sf %>%
    filter(in_zone == 1) %>%
    st_union() %>%
    st_cast("MULTILINESTRING")
  
  # Caption metrics (zone-only); Sig counts use the real |t| > 1.96 threshold
  zone <- map_sf %>% filter(in_zone == 1)
  pos_count     <- sum(zone$adj_cate > 0, na.rm = TRUE)
  sig_pos_count <- sum(zone$adj_cate > 0 & zone$sig_95, na.rm = TRUE)
  neg_count     <- sum(zone$adj_cate < 0, na.rm = TRUE)
  sig_neg_count <- sum(zone$adj_cate < 0 & zone$sig_95, na.rm = TRUE)
  mean_cate     <- mean(zone$adj_cate, na.rm = TRUE)
  
  ggplot(map_sf) +
    # base layer: every land hex in a neutral fill (covers missing-data hexes)
    geom_sf(fill = "#fcfcfc", color = "grey93", linewidth = 0.05) +
    # data layer: ALL hexes with an estimate, colored by CATE (no sig filter)
    geom_sf(data = filter(map_sf, !is.na(adj_cate)),
            aes(fill = adj_cate), color = NA) +
    geom_sf(data = nyc_shoreline, color = "grey65", linewidth = 0.35) +
    geom_sf(data = cordon_boundary, color = "white", linewidth = 1.2, alpha = 0.8) +
    geom_sf(data = cordon_boundary, color = "black", linewidth = 0.6) +
    scale_fill_gradient2(
      low      = low_color,
      mid      = "#f7f7f7",
      high     = high_color,
      midpoint = 0,
      limits   = c(-max_val, max_val),
      oob      = scales::squish,
      na.value = "#fcfcfc",
      breaks   = seq(-max_val, max_val, length.out = 5),
      labels   = label_number(accuracy = 0.01),
      name     = legend_label,
      guide    = guide_colorbar(
        direction      = "vertical",
        barheight      = unit(7, "cm"),
        barwidth       = unit(0.5, "cm"),
        title.position = "top",
        frame.colour   = "black",
        ticks.colour   = "black"
      )
    ) +
    theme_minimal() +
    theme(
      panel.background  = element_rect(fill = "#eaeef1", color = NA),
      panel.grid        = element_blank(),
      axis.text         = element_blank(),
      axis.title        = element_blank(),
      text              = element_text(family = "serif"),
      plot.title        = element_text(size = 22, face = "bold", margin = margin(t = 20, b = 10)),
      plot.subtitle     = element_text(size = 13, color = "grey20", margin = margin(b = 25)),
      plot.caption      = element_text(size = 11, color = "grey30", hjust = 0,
                                       lineheight = 1.3, margin = margin(t = 20)),
      legend.position   = "right",
      legend.background = element_blank(),
      legend.margin     = margin(l = 10, r = 10),
      plot.margin       = margin(15, 15, 15, 15)
    ) +
    labs(title = title, subtitle = subtitle)
}

# 1. Collision map
# Color convention: blue = fewer accidents (better), red = more accidents (worse)
message("[", Sys.time(), "] Generating collision CATE map...")

collision_ate <- tryCatch(
  { read.csv("../../outputs/model_output.csv")$cate %>% mean(na.rm = TRUE) },
  error = function(e) NA
)

collision_ate_val <- if (file.exists("../../outputs/collision_ate.rds")) {
  readRDS("../../outputs/collision_ate.rds")
} else collision_ate

p_collision <- build_map(
  output_csv    = "../../outputs/model_output.csv",
  max_val       = NULL,            # data-driven; set to 0.15–0.20 to fix across maps
  low_color     = "#2166AC",       # blue = fewer accidents
  high_color    = "#B2182B",       # red  = more accidents
  legend_label  = "Adj. CATE\n(Accidents/Week)",
  title         = "Spatial Heterogeneity of Congestion Pricing Impacts",
  subtitle      = "Causal Estimates — Motor Vehicle Collisions | All Hexes (No Significance Filter)",
  outcome_label = "accidents/week",
  ate_val       = collision_ate_val
)

ggsave("../../outputs/NYC_Causal_Impact.png", plot = p_collision,
       width = 14, height = 15, dpi = 300, bg = "white")
message("Collision map saved.")

# 2. Speed map 
# Color convention: blue = faster speeds (better), red = slower speeds (worse)
message("[", Sys.time(), "] Generating speed CATE map...")

speed_ate_val <- -2.805   # from speed model run: ATE (Treated Zone)

p_speed <- build_map(
  output_csv    = "../../outputs/speed_model_output.csv",
  max_val       = 15,
  low_color     = "#053061",  # blue  = slower speeds
  high_color    = "#67001f",   # red = faster speeds
  legend_label  = "Adj. CATE\n(mph)",
  title         = "Spatial Heterogeneity of Congestion Pricing on Vehicle Speed",
  subtitle      = "Calibrated Causal Estimates — Average Speed (mph) | Significant Hexes Only (95% CI)",
  outcome_label = "mph",
  ate_val       = speed_ate_val
)

ggsave("../../outputs/NYC_Speed_Impact.png", plot = p_speed,
       width = 14, height = 15, dpi = 300, bg = "white")
message("Speed map saved.")

message("[", Sys.time(), "] Both maps complete.")