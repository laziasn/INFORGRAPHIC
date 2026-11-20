# PlantGrowth inforgraphic
# Variation of plantweight with treatment 1 and 2
#
#
library(ggplot2)
# wrting arguments
#
args <- commandArgs(trailingOnly = TRUE)
output_file <- if(length(args) >= 1) args[1] else "inforgraphic_plant_grwoth.png"

#selcting plant grwoth data

plant_growth <- datasets::PlantGrowth

#assigning colours to the 3 groups
group_colours <- c(
  ctrl = "green",
  trt1 = "orange",
  trt2 = "purple"
  
)

#generating a ggplot

plant_plot <- ggplot(plant_growth,
       aes(x = group,
           y = weight, 
           colour = group))  +
  
  geom_boxplot(alpha = 0.3, show.legend = FALSE) +
  geom_jitter(width = 0.1, size = 3, alpha = 0.7) +
  
  labs(
    x = "Treatment group",
    y = " Dried weight (g)",
    title = "Difference in plant growth") +
theme_minimal(base_size = 16) + 

#specifying the colours of the plot
scale_colour_manual(values  = group_colours)
#saving the files

ggsave(
  filename = output_file,
  plot = plant_plot,
  width = 1600,
  height = 1200,
  units = "px",
  dpi = 150
)
  
