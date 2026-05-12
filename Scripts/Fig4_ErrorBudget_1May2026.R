#' @title Land Use Change Validation: Spatial Metrics and Budget Analysis
#' @author [Kotikot et al]
#' @date May 2026
#' @description This script performs a categorical validation of a land cover 
#' projection model. It compares a 2010 baseline, a 2018 observed map, and 
#' a 2020 projection to categorize persistence, hits, misses, wrong hits, 
#' and false alarms.

################################################################################
# Setup & Data Loading
################################################################################
library(tidyverse)
library(terra)
library(sf)
library(tidyterra)
library(cowplot)

# Define Plot Theme
theme_vld <- function() {
  theme_bw() +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.border = element_blank(),
      plot.background = element_blank(),
      legend.background = element_blank(),
      legend.key = element_rect(fill = "transparent")
    )
}

# Load Data
narok <- st_read("../Data/ComplementaryFiles/narok_county_utm.shp") #boundary for plotting
luProj <- terra::rast("../Data/LCM_Outputs/landcov_predict_stQuo_2020_15.rst") %>% classify(cbind(0, NA)) #2018 prediction
lu2018 <- terra::rast("../Data/Preprocessed/lu2018_4cls_90m.tif") %>% classify(cbind(0, NA)) #2018 reference
lu2010 <- terra::rast("../Data/Preprocessed/lu2010_4cls_90m.tif") %>% classify(cbind(0, NA)) #2010 reference

################################################################################
# Validation Logic (Vectorized Math)
################################################################################
# Create a unique 3-digit ID: 2010 (hundreds), 2018 (tens), Proj (ones)
lulcSum <- (lu2010 * 100) + (lu2018 * 10) + luProj

# Define Categories
# 1: Persistence Correct (start == mid == end)
# 2: Hits (start != mid & mid == end)
# 3: Misses (start == mid & mid != end)
# 4: Wrong Hits (all different)
# 5: False Alarms (start != mid & mid == end but was persistence) 
# Note: Adjusted logic below to match specific categorizations

all_raster <- classify(lulcSum, matrix(c(
  111, 1, 222, 1, 333, 1, 444, 1  # Correct Rejections
), ncol=2, byrow=TRUE))

# Use logical rasters for complex categories 
s <- lu2010; m <- lu2018; e <- luProj

all_raster <- ifel(s == m & m != e, 5, all_raster) # False Alarms
all_raster <- ifel(s != m & m == e, 2, all_raster) # Hits (Correct change) 2
all_raster <- ifel(s != m & m != e & s == e, 3, all_raster) # Misses
all_raster <- ifel(s != m & m != e & s != e, 4, all_raster) # Wrong Hits

cls2 <- c("Persistence simulated correctly (correct rejections)",
          "Change simulated correctly (hits)",
          "Change simulated as persistence (mises)",
          "Change simulated as change to wrong category (wrong hits)",
          "Persistence simulated as change (false alarms)")

levels(all_raster) <- data.frame(ID=1:5, LandCover=cls2)

################################################################################
# Visualization
################################################################################
vld_colors <- c('grey', "green", "yellow2", 'red3', 'orange')

# Map Plot - Spatial error categories
vld_map <- ggplot() +
  geom_spatraster(data = all_raster) +
  geom_sf(data = narok, fill = NA, color = 'grey30', linewidth=0.5) +
  scale_fill_manual(values = vld_colors, na.translate = F) +
  theme_vld() +
  theme(legend.position = "none")
#--------------------------------------------------------------------------------
# Extract Legend from a dummy plot
vld_legend <- get_legend(
  vld_map + theme(legend.position = "inside",
                  legend.position.inside = c(-0.4, 0.6)) + labs(fill = NULL)
)

vld_legend <- ggplot() +
  geom_spatraster(data = all_raster) +
  theme_bw() +
  labs(fill = NA) +
  scale_fill_manual(values = c('grey', "green", "yellow", 'red', 'orange' ), name = "", 
                    guide = guide_legend(reverse = FALSE,  ncol=1),
                    na.translate = F) +
  geom_spatvector(data = narok, fill = NA, color = 'grey30', linewidth=0.5) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size=15, color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color="black", size=13, hjust = 0.5),
        legend.text=element_text(size=12),
        legend.position = "inside",
        legend.position.inside = c(-0.4, 0.6),
        legend.title = element_blank(),
        legend.justification="left",
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.background=element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.key.height= unit(0.25, 'cm'), #Set legend height
        legend.key.width= unit(1, 'cm'), #Set legend width
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank())

vld_legend

grobs2 <- ggplotGrob(vld_legend)$grobs
Legend <- grobs2[[which(sapply(grobs2, function(x) x$name) == "guide-box")]]

#--------------------------------------------------------------------------------
#Generate and plot the error budget
#--------------------------------------------------------------------------------
stkUn <- terra::unique(all_raster, as.raster=TRUE)
stkUnProps <- freq(stkUn)

stkUnProps$prps <- stkUnProps$count/sum(stkUnProps$count)
stkUnProps$prps2 <- round(stkUnProps$count/sum(stkUnProps$count)*100, digits = 5)
stkUnProps$percs <- round(stkUnProps$prps2, digits = 0)

tab <- stkUnProps

tab$comb <- 'lulc3'
tab$codes <- factor(c('a','e','d','b','c'), levels = c('a','e','d','b','c'))
tab$classes2 <- factor(tab$value,
                       levels = c("Persistence simulated correctly (correct rejections)",     
                                  "Change simulated as persistence (mises)",                  
                                  "Change simulated as change to wrong category (wrong hits)",
                                  "Persistence simulated as change (false alarms)",           
                                  "Change simulated correctly (hits)"))
tab$perc3 <- round(tab$percs, digits=0)
tab$perc3[1] <- tab$perc3[1]-1

#Plot the error budget 
budget <- ggplot(tab, aes(fill=codes, y=percs, x=comb)) + 
  geom_bar(position="stack", stat="identity", width = 0.5) +
  scale_fill_manual(values = c('grey', "green", "yellow", 'red', 'orange'),
                    name = element_blank(),
                    guide = 'none')+
  ylab('Percentage of entire landscape') +
  scale_y_continuous(breaks = c(0, 8, 9, 17, 21, 100), limits = c(0,100)) +
  theme(axis.text.y=element_text(size=15, color="black"), #Axis setting,
        axis.text.x = element_blank(),
        axis.title.y=element_text(size=15, color="black"), #Axis setting
        axis.title.x.bottom = element_blank(), 
        strip.background = element_blank(), #Remove background color
        strip.text = element_text(size=15, color="black"), #Subplot names
        panel.grid.major.y = element_line(color = "grey60",
                                          size = 0.5,
                                          linetype = 2), #Remove major grid lines
        legend.text=element_text(size=12), #Set legend text size
        legend.title = element_blank(), #Set legend title size
        legend.key.height= unit(0.55, 'cm'), #Set legend height
        legend.key.width= unit(0.75, 'cm'), #Set legend width
        legend.position = 'top',
        panel.border = element_rect(colour = NA, fill = NA),
        plot.background = element_rect(fill = 'white', colour = NA), #Plot background
        panel.background = element_rect(colour = 'white', fill='white'), #Panel background
        plot.title = element_text(size=12, hjust = 0.5))
budget

################################################################################
# Final Assembly
################################################################################
# Combine plots (Budget + Legend)


right_col <- plot_grid(budget, Legend, ncol = 1, rel_heights = c(3, 1))

# Combine Map + Right Column
final_plot <- plot_grid(vld_map, right_col, ncol = 2, rel_widths = c(2, 1),
                        axis = "rlbt", align = "h") +
  draw_plot_label(label = c("(a)", "(b)"), x = c(0, 0.65), y = c(0.98, 0.98), size = 15)

final_plot

# ggsave(file="Fig4_ValidationPlots_7May2026.tiff", final_plot,
#        units='px',width=6500,height=4500, dpi=600,compression='lzw')


