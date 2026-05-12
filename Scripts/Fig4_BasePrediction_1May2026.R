library(terra)
library(tidyterra)
library(ggplot2)
library(patchwork)
library(paletteer)

#  Data Processing & Standardization ---
# Define global categories and colors for consistency
cls3   <- c("Forest", "Rangeland", "Cropland", "Urban")
cats3  <- data.frame(ID = 1:4, LandCover = cls3)
lc_pal <- c('green4', "yellow3", 'chocolate3', "darkred")

# Load and factorize rasters
actual   <- rast("../Data/Preprocessed/lu2018_4cls_90m.tif") #reference 2018 map
actual   <- ifel(actual == 0, NA, actual)
levels(actual) <- cats3

prj2018  <- rast("../Data/LCM_Outputs/landcov_predict_stQuo_2020_15.rst") #predicted 2018 map
levels(prj2018) <- cats3

prj2018s <- rast("../Data/LCM_Outputs/landcov_predict_stQuo_2020_15_soft.rst") #2018 soft predicted map
extRast  <- rast("../Data/ComplementaryFiles/extentRaster.tif")
prj2018s <- mask(prj2018s, extRast)

narok    <- sf::st_read("../Data/ComplementaryFiles/narok_county_utm.shp") #boundary for plotting

# Shared Theme Definition ---
# Create a base theme to avoid repetition 
theme_map_pro <- function() {
  theme_void() + # Cleanest base for maps
    theme(
      plot.title = element_text(face = "bold", size = 16, margin = margin(b = 10)),
      legend.position = "bottom",
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      plot.margin = margin(10, 10, 10, 10)
    )
}

# map b legend scale
min_val = 0
max_val = 1
breaks1 = seq(min_val, max_val, by=(max_val-min_val)/5)

# Individual Map Components ---

# A: Reference Map
p1 <- ggplot() +
  geom_spatraster(data = actual) +
  geom_spatvector(data = narok, fill = NA, color = "grey20", linewidth = 0.4) +
  scale_fill_manual(values = lc_pal, name = " ", guide = 'none', na.translate = FALSE) +
  labs(title = "") +
  theme_map_pro()

# B: Hard Prediction
p2 <- ggplot() +
  geom_spatraster(data = prj2018) +
  geom_spatvector(data = narok, fill = NA, color = "grey20", linewidth = 0.4) +
  scale_fill_manual(values = lc_pal, guide = guide_legend(reverse = FALSE,  ncol=1),na.translate = FALSE) +
  labs(title = "") +
  theme_map_pro()+
  theme(legend.text=element_text(size=20),
        legend.position = "inside",
        legend.position.inside = c(0, 0.2),#c(-0.2, 0.2)
        legend.title = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.background=element_blank(),
        legend.justification="left",
        legend.key.height= unit(0.25, 'cm'), 
        legend.key.width= unit(1, 'cm')) 

# C: Soft Prediction
p3 <- ggplot() +
  geom_spatraster(data = prj2018s) +
  geom_spatvector(data = narok, fill = NA, color = "grey20", linewidth = 0.4) +
  scale_fill_gradientn(colours = c('darkgreen', rev(paletteer_c("ggthemes::Red-Green-Gold Diverging", 30)), 'brown4'),#c(rev(paletteer_c("viridis::viridis",100))),
                       na.value = "white",
                       breaks = breaks1,
                       limits = c(min_val, max_val))+
  labs(title = "") +
  theme_map_pro()+
  theme(legend.text=element_text(size=20),
        legend.direction="horizontal",
        legend.position = c(0.01, 0.1),
        legend.title = element_blank(),
        legend.key = element_rect(colour = "transparent", fill = "white"),
        legend.justification="left",
        legend.margin=margin(0,0,0,0),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(1.3, 'cm')) 

# Final Composition & Export ---

# Combine plots: a and c share a legend, b has its own

library(cowplot)
library(ggplot2)
library(grid)

# First a simple wrapper function for labeling
wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")

my_label1 <- "(a) 2018 Hard prediction"
my_label2 <- "(b) 2018 Soft prediction"
my_label3 <- "(c) 2018 Reference map"

# build grids
ff3=plot_grid(p2, p3, p1, align = "h", nrow = 1)+
  
  annotate("text",x=0.08, y=0.72, size=8,label=wrapper(my_label1, width = 15)) +
  annotate("text",x=0.4, y=0.72, size=8,label=wrapper(my_label2, width = 20)) +
  annotate("text",x=0.75, y=0.72, size=8,label=wrapper(my_label3, width = 20)) +
  
  # # Add some space around the edges  
  theme(plot.margin = unit(c(0.4,0,0.4,0), "cm"))

# turn off clipping
gt <- ggplot_gtable(ggplot_build(ff3))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

ff3
# 
# ggsave(file="../Figures/Fig4_BaseProjections_May2026.tiff", ff3,
#        units='px',width=8500,height=4000, dpi=600,compression='lzw')


