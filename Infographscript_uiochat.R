# PlantGrowth infographic — variation of plant weight with treatment 1 and 2
# Uses ONLY tidyverse + base R

library(tidyverse)   # ggplot2, dplyr
library(grid)        # base R: multi-panel canvas

# ---- arguments ----
args <- commandArgs(trailingOnly = TRUE)
output_file <- if (length(args) >= 1) args[1] else "infographic_plant_growth.png"
base_size   <- if (length(args) >= 2) as.numeric(args[2]) else 16
if (is.na(base_size)) {
  stop("Text size must be a number, got '", args[2], "'.", call. = FALSE)
}

# ---- data ----
plant_growth <- datasets::PlantGrowth
group_colours <- c(ctrl = "green", trt1 = "orange", trt2 = "purple")

# ---- computations ----
group_stats <- plant_growth %>%                 # <- assignment, NOT <=
  group_by(group) %>%
  summarise(
    mean_w = mean(weight),
    se     = sd(weight) / sqrt(n()),
    lo     = mean_w - 1.96 * se,
    hi     = mean_w + 1.96 * se,
    .groups = "drop"
  )

# ---- panel ----
plant_plot <- ggplot(group_stats,               # <- the real object name
                     aes(x = group, y = mean_w, fill = group)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.15, linewidth = 0.8) +
  geom_text(aes(label = sprintf("%.2f g", mean_w)),
            vjust = -1.2, fontface = "bold") +
  scale_fill_manual(values = group_colours) +
  labs(x = NULL, y = "Mean weight (g)",
       subtitle = "Group means with 95% CI") +
  theme_minimal(base_size = base_size)          # <- base_size now actually used

# ---- header text: plain assignments, ONE set of names ----
title_text    <- "Does treatment application affect plant growth?"
subtitle_text <- "Dried weight of 30 plants in 3 groups"
caption_text  <- "Data: datasets::PlantGrowth (Dobson 1983)"

# ---- canvas: header + one panel ----
png(output_file, width = 1600, height = 1200, res = 150)
grid.newpage()
pushViewport(viewport(layout = grid.layout(
  nrow = 2, ncol = 1,
  heights = unit(c(1, 3), "null")
)))

pushViewport(viewport(layout.pos.row = 1))
grid.text(title_text,    x = 0.03, y = 0.70, just = c("left", "centre"),
          gp = gpar(fontsize = base_size * 1.6, fontface = "bold"))
grid.text(subtitle_text, x = 0.03, y = 0.25, just = c("left", "centre"),
          gp = gpar(fontsize = base_size * 0.9, col = "grey30"))
grid.text(caption_text,  x = 0.03, y = 0.00, just = c("left", "bottom"),
          gp = gpar(fontsize = base_size * 0.65, col = "grey40"))
popViewport()

print(plant_plot, vp = viewport(layout.pos.row = 2))   # <- the panel that exists

dev.off()
message("Saved: ", output_file)
