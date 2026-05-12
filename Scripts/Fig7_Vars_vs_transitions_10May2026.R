#' @title Land Use Transition Drivers Visualization
#' @description This script processes geospatial data to analyze how physical and 
#' policy factors (Slope, Fragmentation, Temperature, and Land Tenure, etc) influence 
#' land use transitions.
#' @author [Kotikot t al.]
#' @date May 2026
#' 
# Load necessary libraries
library(tidyverse) # Data manipulation and visualization
library(terra)      # Spatial data analysis for rasters
library(sf)         # Vector data handling
library(cowplot)    # Plot arrangement and themes

# Setup & Lookups ------------------------------------------------------

# Mapping transition IDs to names
TRANS_MAP <- c(
  "0" = "Forest to Forest",   "1" = "Forest to Rangeland", "2" = "Forest to Cropland",
  "3" = "Rangeland to Forest", "4" = "Rangeland Persistence", "5" = "Rangeland to Cropland",
  "6" = "Rangeland to Urban",  "7" = "Cropland to Forest",   "8" = "Cropland to Rangeland",
  "9" = "Cropland Persistence", "10" = "Cropland to Urban",  "11" = "Urban Persistence"
)

# Data Loading & Pre-processing ----------------------------------------

# Load extent raster to ensure all datasets are masked to the same study area boundary
extRast <- terra::rast("../Data/ComplementaryFiles/extentRaster.tif")

slope <- terra::rast("../Data/Preprocessed/slp.tif")
frag <- terra::rast("../Data/Preprocessed/ForFrag_spa5_2000.tif")
temp <- terra::mask(terra::rast("../Data/Preprocessed/temp_annAvg09.tif"), extRast)
polcy <- terra::mask(terra::rast("../Data/Preprocessed/policy.tif"), extRast)

# Load Land Use / Land Cover (LULC) rasters: 2010 baseline and a predicted scenario
luProj  <- terra::classify(terra::rast("../Data/LCM_Outputs/landcov_predict_stQuo_2020_15.rst"), cbind(0, NA))
lu2010  <- terra::classify(terra::rast("../Data/Preprocessed/lu2010_4cls_90m.tif"), cbind(0, NA))

# Define land cover classes for categorical raster levels
cls3 <- c("Forest",
          "Rangeland",
          "Cropland",
          "Urban")
cats3 <- data.frame(ID=1:4, LandCover=cls3)

levels(luProj) <- cats3
levels(lu2010) <- cats3

# Generate transition raster by identifying unique combinations of 2010 vs Predicted classes
lu2010_m <- terra::mask(lu2010, luProj)
luProj_m <- terra::mask(luProj, lu2010)
stk_un   <- terra::unique(c(lu2010_m, luProj_m), as.raster = TRUE)
stk_un2 <- terra::concats(lu2010_m, luProj_m)

# 3. Helper Function for crosstabulation 

#' Calculate transition percentages per factor class
#' @param factor_rast The driver raster (e.g., slope)
#' @param trans_rast The raster representing LULC changes
#' @param target_tos Specific transitions to filter for
#' 
get_transition_df <- function(factor_rast, trans_rast,  target_tos) {
  
  # Cross-tabulate factor values against transitions to get pixel counts (Freq)
  ct <- terra::crosstab(c(factor_rast, trans_rast), long = TRUE)
  colnames(ct) <- c("From", "To", "Freq")
  
  df <- ct %>%
    # Calculate percentage based on pixel frequency (equivalent to area percentage)
    mutate(landA = sum(Freq)) %>%
    mutate(perc = (Freq / landA) * 100) %>%
    filter(To %in% target_tos) %>%
    mutate(ers = recode(as.character(To), !!!TRANS_MAP))

  return(df)
}

# 4. Generate Individual Panels -------------------------------------------------------------------

## Panel A: Slope Analysis ----
df_slope <- get_transition_df(slope, stk_un, target_tos = c("Forest_Rangeland", "Cropland_Rangeland"))

slope_p <- ggplot(data=df_slope, aes(x=From, y=perc, fill=ers)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c('#bebada','#ccebc5','#fdb462','#fb8072','#80b1d3',
                               '#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5', '#ffed6f'),
                    guide = 'none',
                    name = element_blank())+#,
  ylab('Percentage of landscape area') +
  scale_y_continuous(n.breaks=5) +
  scale_x_continuous(n.breaks=10) +
    xlab('Slope (%)') +
  theme(axis.text.y=element_text(size=15, color="black"), #Axis setting,
        axis.text.x = element_text(size=15, color="black"),
        axis.title.y=element_text(size=15, color="black"), #Axis setting
        axis.title.x.bottom = element_text(size=15, color="black"), 
        strip.background = element_blank(), #Remove background color
        strip.text = element_text(size=15, color="black"), #Subplot names
        panel.grid.major.y = element_line(color = "grey60",
                                          size = 0.5,
                                          linetype = 2), #Remove major grid lines
        panel.grid.major.x = element_line(color = "grey60",
                                          size = 0.5,
                                          linetype = 2), #Remove major grid lines
        legend.text=element_text(size=12), #Set legend text size
        legend.title = element_blank(), #Set legend title size
        legend.key.height= unit(0.55, 'cm'), #Set legend height
        legend.key.width= unit(0.75, 'cm'), #Set legend width
        legend.position = 'top',
        panel.border = element_rect(colour = 'black', fill = NA),
        plot.background = element_rect(fill = 'white', colour = NA), #Plot background
        panel.background = element_rect(colour = 'white', fill='white'), #Panel background
        plot.title = element_text(size=12, hjust = 0.5))
#slope_p
#-------------------------------------------------------------------------------------------------
## Panel B: Forest Fragmentation ----
# Categorize fragmentation into Margin, Edge, Perforation, and Core
df_frag <- get_transition_df(frag, stk_un, 
                             target_tos = c('Forest_Rangeland', 'Forest_Cropland', 'Forest_Forest'))
df_frag1 <- df_frag %>% 
  mutate(from2 = if_else(From == "Margin", 1, 0),
         from2 = if_else(From == "Edge", 2, from2),
         from2 = if_else(From == "Perforation", 3, from2),
         from2 = if_else(From == "Core", 4, from2),
         ers2 = if_else(To == 'Forest_Forest', "c", To),
         ers2 = if_else(To == 'Forest_Rangeland', "b", ers2),
         ers2 = if_else(To == 'Forest_Cropland', "a", ers2))
df_frag1$from2 <- as.factor(df_frag1$from2)

frag_p <- ggplot(data=df_frag1, aes(x=from2, y=perc, fill=ers2)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c('#fdb462', '#ccebc5','grey60',  'grey70','#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3',
                               '#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5', '#ffed6f'),
                    guide = 'none',
                    name = element_blank())+#,
  ylab('Percentage of landscape area') +
  scale_y_continuous(n.breaks=5) +
  scale_x_discrete(breaks=c("1","2","3","4"),
                   labels=c("Margin", "Edge", "Perforation", "Core"))+
    xlab('Forest fragmentation level') +
  #Set theme parameters
  theme(axis.text.y=element_text(size=15, color="black"), #Axis setting,
        axis.text.x = element_text(size=15, color="black"),
        axis.title.y=element_text(size=15, color="black"), #Axis setting
        axis.title.x.bottom = element_text(size=15, color="black"), 
        strip.background = element_blank(), #Remove background color
        strip.text = element_text(size=15, color="black"), #Subplot names
        panel.grid.major.y = element_line(color = "grey60",
                                          size = 0.5,
                                          linetype = 2), #Remove major grid lines
        panel.grid.major.x = element_line(color = "grey60",
                                          size = 0.5,
                                          linetype = 2), #Remove major grid lines
        legend.text=element_text(size=12), #Set legend text size
        legend.title = element_blank(), #Set legend title size
        legend.key.height= unit(0.55, 'cm'), #Set legend height
        legend.key.width= unit(0.75, 'cm'), #Set legend width
        legend.position = 'top',
        panel.border = element_rect(colour = 'black', fill = NA),
        #panel.border = element_rect(colour = NA, fill = NA),
        plot.background = element_rect(fill = 'white', colour = NA), #Plot background
        panel.background = element_rect(colour = 'white', fill='white'), #Panel background
        plot.title = element_text(size=12, hjust = 0.5))
#frag_p
#-------------------------------------------------------------------------------------------------
## Panel C: Temperature ----
df_temp <- get_transition_df(temp, stk_un, target_tos = c('Cropland_Rangeland', 'Cropland_Cropland')) %>% 
  mutate(ers2 = if_else(To == 'Cropland_Rangeland', "a", To),
         ers2 = if_else(To == 'Cropland_Cropland', "b", ers2))

temps <- ggplot(data=df_temp, aes(x=From, y=perc, fill=ers2)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values = c('#bebada', 'grey60',  'grey70','#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3',
                               '#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5', '#ffed6f'),
                    guide = 'none',
                    name = element_blank())+#,
  ylab('Percentage of landscape area') +
  scale_y_continuous(n.breaks=5) +
  scale_x_continuous(n.breaks=13) +
    xlab('Annual average temperature (Degree C)') +
  theme(axis.text.y=element_text(size=15, color="black"), #Axis setting,
        axis.text.x = element_text(size=15, color="black"),
        axis.title.y=element_text(size=15, color="black"), #Axis setting
        axis.title.x.bottom = element_text(size=15, color="black"), 
        strip.background = element_blank(), #Remove background color
        strip.text = element_text(size=15, color="black"), #Subplot names
        panel.grid.major.y = element_line(color = "grey60",
                                          size = 0.5,
                                          linetype = 2), #Remove major grid lines
        panel.grid.major.x = element_line(color = "grey60",
                                          size = 0.5,
                                          linetype = 2), #Remove major grid lines
        legend.text=element_text(size=12), #Set legend text size
        legend.title = element_blank(), #Set legend title size
        legend.key.height= unit(0.55, 'cm'), #Set legend height
        legend.key.width= unit(0.75, 'cm'), #Set legend width
        legend.position = 'top',
        panel.border = element_rect(colour = 'black', fill = NA),
        plot.background = element_rect(fill = 'white', colour = NA), #Plot background
        panel.background = element_rect(colour = 'white', fill='white'), #Panel background
        plot.title = element_text(size=12, hjust = 0.5))
#temps
#-------------------------------------------------------------------------------------------------
## Panel D: Policy / Tenure ----
allTrans <- c("Forest_Forest",   "Forest_Rangeland",    "Forest_Cropland" ,  "Rangeland_Forest", "Rangeland_Rangeland" ,
"Rangeland_Cropland" ,   "Rangeland_Urban" ,   "Cropland_Forest", "Cropland_Rangeland",  "Cropland_Cropland", 
"Cropland_Urban" ,    "Urban_Urban")

# Filter and group data by land tenure policy (Private vs Conservancy etc)
df_polcy <- get_transition_df(polcy, stk_un, 
                                target_tos = allTrans)%>%  
  mutate(polc = if_else(From == 1, "Private", 'polc'),
         polc = if_else(From == 2, "Group ranch (GR)", polc),
         polc = if_else(From == 3, "Historical GR", polc),
         polc = if_else(From == 4, "Conservancy", polc),
         polc = if_else(From == 5, "PA Forest", polc),
         polc = if_else(From == 6, "PA MMGR", polc))

# Calculate proportion relative to the specific policy area, not the whole landscape
aa <- df_polcy %>% 
  group_by(From) %>% 
  summarise(pxCnt = sum(Freq))

df_polcy$org <- c(rep(aa$pxCnt[1], 12), rep(aa$pxCnt[2], 10),
                  rep(aa$pxCnt[3], 12),  rep(aa$pxCnt[4], 6),
                  rep(aa$pxCnt[5], 6),  rep(aa$pxCnt[6], 7))

df_polcy$prps <- round((df_polcy$Freq/df_polcy$org)*100, digits = 4)
  
df_polcy1 <- df_polcy %>% 
  dplyr::filter(polc != "PA Forest" & polc != "PA MMGR") 

df_polcy2 <- df_polcy1 %>% 
  dplyr::filter(ers == 'Cropland_Rangeland' | ers == 'Cropland_Rangeland' | ers == 
                  'Forest_Cropland' | ers ==  'Forest_Rangeland' | ers ==  'Rangeland_Cropland' )

polcP <- ggplot(data=df_polcy2, aes(x=polc, y=prps, fill=ers)) + 
  geom_bar(position="stack", stat="identity", width=0.93) +
  coord_flip() + # Flip for better readability of tenure labels
  scale_fill_manual(values = c('#bebada','#fdb462','#ccebc5','#fb8072','#80b1d3',
                               '#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5', '#ffed6f'),
                    guide = guide_legend(reverse = FALSE, ncol=4),
                    name = element_blank())+#,
  ylab('Percent of policy land area') +
  scale_y_continuous(n.breaks=5) +
  
  xlab('') +

  theme(#aspect.ratio = 1/8,
    axis.text.y=element_text(size=15, color="black"), #Axis setting,
    axis.text.x = element_text(size=15, color="black", angle = 0,
                               margin = margin(t = 10, r = 0, b = 0, l = 0)),
    axis.title.y=element_text(size=15, color="black"), #Axis setting
    axis.title.x.bottom = element_text(size=12, color="black"), 
    strip.background = element_blank(), #Remove background color
    strip.text = element_text(size=15, color="black"), #Subplot names
    panel.grid.major.y = element_line(color = "grey60",
                                      size = 0.1,
                                      linetype = 2), #Remove major grid lines
    panel.grid.major.x = element_line(color = "grey60",
                                      size = 0.1,
                                      linetype = 2), #Remove major grid lines
    legend.text=element_text(size=15), #Set legend text size
    legend.justification="left",
    legend.key = element_rect(colour = "transparent", fill = "transparent"),
    legend.background=element_blank(),
    legend.title = element_blank(), #Set legend title size
    legend.key.height= unit(0.55, 'cm'), #Set legend height
    legend.key.width= unit(0.75, 'cm'), #Set legend width
    legend.position = "none",
    panel.border = element_rect(colour = 'black', fill = NA),
    plot.background = element_rect(fill = 'white', colour = NA), #Plot background
    panel.background = element_rect(colour = 'white', fill='white'), #Panel background
    plot.title = element_text(size=12, hjust = 0.5),
    plot.margin = margin(t = 0,  # Top margin
                         r = 0,  # Right margin
                         b = 0,  # Bottom margin
                         l = 0)) # Left margin)
#polcP

#---------------------------------------------------------------------------------------
polcP_legend <- ggplot(data=df_polcy2, aes(x=polc, y=prps, fill=ers)) + 
  geom_bar(position="stack", stat="identity", width=0.93) +
  coord_flip() +
  scale_fill_manual(values = c('#bebada','#fdb462','#ccebc5','#fb8072','#80b1d3',
                               '#fdb462','#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5', '#ffed6f'),
                    guide = guide_legend(reverse = FALSE, ncol=4),
                    name = element_blank())+#,
  scale_y_continuous(n.breaks=10) +
  
  xlab('') +
  #Set theme parameters
  theme(aspect.ratio = 1/10,
        axis.text.y=element_text(size=12, color="black", face='bold'), #Axis setting,
        axis.text.x = element_text(size=12, color="black", face='bold',
                                   margin = margin(t = 10, r = 0, b = 0, l = 0)),
        axis.title.y=element_text(size=12, color="black", face='bold'), #Axis setting
        axis.title.x.bottom = element_text(size=12, color="black", face='bold'), 
        strip.background = element_blank(), #Remove background color
        strip.text = element_text(size=12, color="black", face='bold'), #Subplot names
        panel.grid.major.y = element_line(color = "grey60",
                                          size = 0.5,
                                          linetype = 2), #Remove major grid lines
        panel.grid.major.x = element_line(color = "grey60",
                                          size = 0.5,
                                          linetype = 2), #Remove major grid lines
        legend.text=element_text(size=15), #Set legend text size
        legend.position  = c(0.89, 0.99),

        legend.justification="right",
        legend.key = element_rect(colour = "transparent", fill = "transparent"),
        legend.background=element_blank(),
        legend.title = element_blank(), #Set legend title size
        legend.key.height= unit(0.55, 'cm'), #Set legend height
        legend.key.width= unit(0.75, 'cm'), #Set legend width
        panel.border = element_rect(colour = 'black', fill = NA),
        plot.background = element_rect(fill = 'white', colour = NA), #Plot background
        panel.background = element_rect(colour = 'white', fill='white'), #Panel background
        plot.title = element_text(size=12, hjust = 0.5))
#polcP_legend

# Final Assembly -------------------------------------------------------
grobs2 <- ggplotGrob(polcP_legend)$grobs
shared_legend <- grobs2[[which(sapply(grobs2, function(x) x$name) == "guide-box")]]

# Combine all panels into a 2x2 grid
polls = plot_grid(polcP, frag_p, temps, slope_p, align = "h", nrow = 2)

# Add sub-plot labels (a, b, c, d) and the shared legend
final_plot = plot_grid(polls, shared_legend, align = "v", nrow = 2)+
  annotate("text",x=0.06, y=0.98, size=7,label= "(a)") +
  annotate("text",x=0.57, y=0.98, size=7,label= "(b)") +
  annotate("text",x=0.06, y=0.73, size=7,label= "(c)") +
  annotate("text",x=0.58, y=0.73, size=7,label= "(d)") +

  theme(plot.margin = unit(c(0.4,0,0.4,0), "cm"))

gt <- ggplot_gtable(ggplot_build(final_plot))
gt$layout$clip[gt$layout$name == "panel"] <- "off"

final_plot

# Save as high-resolution TIFF
ggsave(file="Fig7_Vars_vs_transitions_7May2026.tiff", final_plot,
       units='px',width=7500,height=7900, dpi=600,compression='lzw')


