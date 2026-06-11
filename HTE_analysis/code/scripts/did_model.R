# DiD_analysis.R
# Difference-in-Differences: Motor Vehicle Collisions
# Treatment: CBD Tolling Zone (in_zone == 1)
# Control: Outer Boroughs (in_zone == 0), excluding Staten Island

library(readr)
library(dplyr)
library(ggplot2)
library(fixest)

POLICY_DATE <- as.Date("2025-01-05")
OUTPUT_DIR  <- "../../outputs/"

# Load 
data <- read_csv("../../data/cleaned/model_data_final.csv", show_col_types = FALSE) |>
  mutate(week = as.Date(week))

# 1. DiD regression
did_model <- feols(
  y_count ~ treatment | hex_id + week,
  data = data,
  cluster   = ~hex_id
)

summary(did_model)

# 2. Aggregate to zone-week 
did_agg <- data |>
  group_by(week, in_zone) |>
  summarise(total_collisions = sum(y_count, na.rm = TRUE), .groups = "drop") |>
  mutate(
    group    = ifelse(in_zone == 1, "Treatment (CBD Zone)", "Control (Outer Boroughs)"),
    rel_week = as.numeric(difftime(week, POLICY_DATE, units = "weeks"))
  ) |>
  filter(week < max(week))

# Normalize to pre-treatment mean = 100
pre_means <- did_agg |>
  filter(rel_week < 0) |>
  group_by(group) |>
  summarise(pre_mean = mean(total_collisions), .groups = "drop")

did_agg <- did_agg |>
  left_join(pre_means, by = "group") |>
  mutate(indexed = (total_collisions / pre_mean) * 100)

# 3. Parallel trends plot (indexed)
p_trends <- ggplot(did_agg, aes(x = week, y = indexed, color = group)) +
  geom_hline(yintercept = 100, linetype = "dotted", color = "grey50") +
  geom_line(linewidth = 0.8) +
  geom_vline(xintercept = POLICY_DATE, linetype = "dashed", color = "black") +
  annotate("text", x = POLICY_DATE + 14,
           y = max(did_agg$indexed) * 0.98,
           label = "Jan 5, 2025\n(Congestion Pricing)",
           hjust = 0, size = 3.5, family = "serif") +
  scale_color_manual(values = c(
    "Control (Outer Boroughs)" = "steelblue",
    "Treatment (CBD Zone)"     = "brown2"
  )) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(
    title   = "Weekly Motor Vehicle Collisions: Treatment vs Control",
    subtitle = "Indexed to pre-treatment mean = 100",
    x       = NULL,
    y       = "Collision Index (Pre-Treatment Mean = 100)",
    color   = NULL,
    caption = paste0("DiD: ", round(coef(did_model)["treatment"], 3),
                     " collisions/hex/week (SE: ",
                     round(se(did_model)["treatment"], 3), ") | Zone + Week FE")
  ) +
  theme_minimal(base_family = "serif") +
  theme(
    plot.title       = element_text(face = "bold", size = 15),
    plot.subtitle    = element_text(color = "grey30", size = 11),
    plot.caption     = element_text(color = "grey40", size = 9, hjust = 0),
    legend.position  = "bottom",
    axis.text.x      = element_text(angle = 30, hjust = 1),
    panel.grid.minor = element_blank()
  )

ggsave(paste0(OUTPUT_DIR, "parallel_trends.png"), plot = p_trends,
       width = 12, height = 6, dpi = 300, bg = "white")
message("Parallel trends saved.")

# 4. Summary
message("\n DiD Summary")
message(sprintf("  Treatment effect : %.4f collisions/hex/week", coef(did_model)["treatment"]))
message(sprintf("  SE               : %.4f", se(did_model)["treatment"]))
message(sprintf("  t-stat           : %.4f", coef(did_model)["treatment"] / se(did_model)["treatment"]))
message(sprintf("  Pre-period weeks : %d", n_distinct(data$week[data$post == 0])))
message(sprintf("  Post-period weeks: %d", n_distinct(data$week[data$post == 1])))