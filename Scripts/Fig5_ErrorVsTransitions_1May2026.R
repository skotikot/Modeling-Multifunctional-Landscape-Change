#' @title Land Use Change Validation: Error Categories vs. Transitions
#' @description Analyzes model errors (Mises, False Alarms, Wrong Hits) against 
#' specific land cover transitions and generates maps and charts.
#' @author [Kotikot et al]
#' @date May 2026

# Setup & Dependencies -------------------------------------------------
library(tidyverse)
library(terra)
library(sf)
library(tidyterra)
library(cowplot)

# Standardized Theme for Maps
theme_narok_map <- function() {
  theme_minimal() +
    theme(
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(face = "bold", size = 13, hjust = 0.5),
      legend.text = element_text(size = 11),
      legend.title = element_text(size = 12, face = "bold"),
      legend.background = element_blank()
    )
}

# Data Acquisition  --------------------------------

# Load Rasters and mask zeros to NA
narok <- st_read("../Data/ComplementaryFiles/narok_county_utm.shp") #boundary for plotting
luProj <- terra::rast("../Data/LCM_Outputs/landcov_predict_stQuo_2020_15.rst") %>% classify(cbind(0, NA)) #2018 prediction
lu2018 <- terra::rast("../Data/Preprocessed/lu2018_4cls_90m.tif") %>% classify(cbind(0, NA)) #2018 reference
lu2010 <- terra::rast("../Data/Preprocessed/lu2010_4cls_90m.tif") %>% classify(cbind(0, NA)) #2010 reference

# Define Categories using vectorized logic (s=start, m=middle, e=end/proj)
s <- lu2010; m <- lu2018; e <- luProj
lulcSum <- (s * 100) + (m * 10) + e

# 1: Correct Rejection, 2: Hits, 3: Misses, 4: Wrong Hits, 5: False Alarms
all_raster <- classify(lulcSum, matrix(c(111,1, 222,1, 333,1, 444,1), ncol=2, byrow=TRUE))
all_raster <- ifel(s == m & m != e, 5, all_raster)
all_raster <- ifel(s != m & m == e, 2, all_raster) 
all_raster <- ifel(s != m & m != e & s == e, 3, all_raster) 
all_raster <- ifel(s != m & m != e & s != e, 4, all_raster) 

cls2 <- c("Persistence simulated correctly (correct rejections)",
          "Change simulated correctly (hits)",
          "Change simulated as persistence (misses)",
          "Change simulated as change to wrong category (wrong hits)",
          "Persistence simulated as change (false alarms)")

levels(all_raster) <- data.frame(ID=1:5, LandCover=cls2)

# Transitions and Statistics -------------------------------------

# Generate Transitions Raster (2010 reference to 2020 Projected)
cls_lc <- c("Forest", "Rangeland", "Cropland", "Urban")
levels(lu2010) <- levels(luProj) <- data.frame(ID=1:4, LandCover=cls_lc)

#Mask extents to ensure matching 
lu2010_ <- mask(lu2010, luProj)
luProj_ <- mask(luProj, lu2010_)

# Identify unique transitions
stkUn <- terra::unique(c(lu2010_, luProj_), as.raster=TRUE)

# Frequency and Percentage Analysis
val_dcrop <- terra::crosstab(c(all_raster, stkUn), long = TRUE)
colnames(val_dcrop) <- c("ErrorType", "Transition", "Freq")

tab_stats <- val_dcrop %>%
  mutate(perc = (Freq / sum(Freq)) * 100) %>%
  # Filter to focus on errors in specific transition classes (we are not interested in correct predictions)
  filter(!ErrorType %in% c('Change simulated correctly (hits)',
                           'Persistence simulated correctly (correct rejections)')) %>%
  filter(!Transition %in% c("Cropland_Forest", "Urban_Urban", 'Rangeland_Forest',
                            'Rangeland_Urban', 'Cropland_Urban')) 

# Rename transitions
trans_lookup <- c(
  "Cropland_Cropland"      = "Cropland to Cropland",
  "Forest_Forest"        = "Forest to Forest",
  "Rangeland_Rangeland"    = "Rangeland to Rangeland",
  "Cropland_Rangeland"           = "Cropland to Rangeland",
  "Forest_Cropland"                = "Forest to Cropland",
  "Forest_Cropland" = "Forest to Cropland",
  "Forest_Rangeland"= "Forest to Rangeland",
  "Rangeland_Cropland"               = "Rangeland to Cropland"
)

tab_stats <- tab_stats %>% 
  mutate(
    Transition = recode(Transition, !!!trans_lookup) )

# Reorder as desired for ploting
tab_stats$nms <- c( 'c','c','c','c', 'a','a','a', 'b','b','b','b')


# Visualization: Bar Chart (Panel A) -----------------------------------

p_bar <- ggplot(data=tab_stats, aes(x = Transition, y = perc, fill = nms)) + 
  geom_bar(position="stack", stat="identity", width=0.93) +
  coord_flip() +

  scale_fill_manual(values = c( "yellow", 'orange', 'red', "green" ),
                    name = element_blank(),
                    label = c("Misses", "False alarms","Wrong hits", "Hits" ))+#,
  ylab('Percent of Landscape') +
  scale_y_continuous(n.breaks=5) +
  
  xlab('') +
  #Set theme parameters
  theme(aspect.ratio = 1/1,
        axis.text.y=element_text(size=12, color="black"), 
        axis.text.x = element_text(size=12, color="black",
                                   margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y=element_text(size=12, color="black"), 
        axis.title.x.bottom = element_text(size=12, color="black"), 
        strip.background = element_blank(), 
        strip.text = element_text(size=12, color="black"), 
        panel.grid.major.y = element_line(color = "grey60",
                                          size = 0.5,
                                          linetype = 2), 
        panel.grid.major.x = element_line(color = "grey60",
                                          size = 0.5,
                                          linetype = 2), 
        legend.text=element_text(size=12), 
        legend.position  = c(1.0, 0.7),
        legend.justification="left",
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.background=element_blank(),
        legend.title = element_blank(), 
        legend.key.height= unit(0.55, 'cm'), 
        legend.key.width= unit(0.75, 'cm'), 
        panel.border = element_rect(colour = 'black', fill = NA),
        plot.background = element_rect(fill = 'white', colour = NA), 
        panel.background = element_rect(colour = 'white', fill='white'), 
        plot.title = element_text(size=12, hjust = 0.5))

# Visualization: Spatial Map (Panel B) ---------------------------------

spChange <- terra::unique(c(all_raster, stkUn), as.raster=TRUE)
#levels(spChange) # check to see what levels need reclassification or grouping to NA
m <- matrix(c(
  -Inf, 19, NA,
  19,   20, 1,   # 
  20,   21, 2,   # 
  21,   22, 3,   #
  22,   23, NA,
  23,   31, 8,   # 
  31,   32, 4,   # 
  32,   33, 5,   # 
  33,   34, NA,  # 
  34,   35, 6,   # 
  35,   37, NA,
  37,   38, 7,   # 
  38,  Inf, NA
), ncol=3, byrow=TRUE)

# Classify 
spChange3 <- classify(spChange, m)

# Set Categories (errors vs modeled transitions)
cls_names <- c("Misses (Forest to Forest)",
               "Misses (Rangeland to Rangeland)",
               "Misses (Cropland to Cropland)",
               "False alarms (Forest to Rangeland)",
               "False alarms (Forest to Cropland)",
               "False alarms (Rangeland to Cropland)",
               "False alarms (Cropland to Rangeland)",
               "Wrong hits")

# Create the levels data frame
cats <- data.frame(ID=1:8, change=cls_names)
levels(spChange3) <- cats

# Error mapping palette
vld_colors <- c(
  'yellowgreen', 'yellow3','#FFFF00',  # Mises
  '#fed98e','#fe9929','#d95f0e','#993404', # False Alarms
  "red" # Wrong Hits
)

# Generate spatial map
p_map <- ggplot() +
  geom_spatraster(data = spChange3) +
  geom_spatvector(data = narok, fill = NA, color = "grey30", linewidth = 0.4) +
  theme_bw() +
  labs(fill = NA) +
  scale_fill_manual(values = vld_colors,
                    name = 'Error (Predicted transitions)',
                    guide = guide_legend(reverse = FALSE,  ncol=1),
                    na.translate = F) +
  theme(strip.background = element_blank(),
        strip.text = element_text(size=15, color="black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(face = "bold", color="black", size=13, hjust = 0.5),
        legend.position = 'none',
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),
        plot.background = element_blank(),
        panel.background = element_blank())

# Extract Legend from a dummy plot
p_for_legend <- ggplot() +
  tidyterra::geom_spatraster(data = spChange3) +
  scale_fill_manual(
    values = vld_colors,
    name = 'Error (Predicted transitions)',
    na.translate = FALSE
  ) +
  theme_bw() +
  theme(
    legend.text = element_text(size=12),
    legend.title = element_text(size=15),
    legend.key.height= unit(0.25, 'cm'), #Set legend height
    legend.key.width= unit(1, 'cm'), #Set legend width
    legend.position  = c(0.8, 0.7)  )

#Extract the legend
#shared_legend <- cowplot::get_legend(p_for_legend)

grobs4 <- ggplotGrob(p_for_legend)$grobs
shared_legend <- grobs4[[which(sapply(grobs4, function(x) x$name) == "guide-box")]]

# Final Assembly -------------------------------------------------------

# Combine plots using cowplot
p1 = plot_grid(p_bar, shared_legend, align = "v", nrow = 2, rel_heights = c(2,1),
               axis = "rlbt")

p2 = plot_grid(p1, p_map, align = "h", nrow = 1, rel_widths = c(1,2),
              axis = "rlbt")+
  
  annotate("text",x=0.08, y=0.9, size=7,label="(a)") +
  annotate("text",x=0.65, y=0.9, size=7,label="(b)") 

# turn off clipping
gt <- ggplot_gtable(ggplot_build(p2))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

p2

ggsave(file="Fig5_Errors_transitions_7May2025.tiff", p2,
       units='px',width=6500,height=3000, dpi=600,compression='lzw')



