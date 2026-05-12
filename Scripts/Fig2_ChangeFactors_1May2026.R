
# Load necessary libraries
library(terra)
library(ggplot2)
library(tidyterra) # Best for plotting spatRasters with ggplot2
library(patchwork)
library(sf)

# Load support files
extRast <- terra::rast("../Data/ComplementaryFiles/extentRaster.tif")
narok <- st_read("../Data/ComplementaryFiles/narok_county_utm.shp", quiet = TRUE)

# Load PROCEESSED LULC CHANGE FACTORS
dist2_cropland2000 <- rast("../Data/Preprocessed/dist2_cropland2000.tif") 
dist2_forest2000 <- rast("../Data/Preprocessed/dist2_forest2000.tif")   
dist2_ncropland2000 <- rast("../Data/Preprocessed/dist2_ncropland2000.tif")
dist2_nforest2000 <- rast("../Data/Preprocessed/dist2_nforest2000.tif")  
dist2_nrange2000 <- rast("../Data/Preprocessed/dist2_nrange2000.tif")   
dist2_nurban2000 <- rast("../Data/Preprocessed/dist2_nurban2000.tif")   
dist2_range2000 <- rast("../Data/Preprocessed/dist2_range2000.tif")    
dist2_urban2000 <- rast("../Data/Preprocessed/dist2_urban2000.tif")    
elev <- rast("../Data/Preprocessed/elev.tif")               
evi_0310mn <- rast("../Data/Preprocessed/evi_0310mn.tif")         
evi_0310pek <- rast("../Data/Preprocessed/evi_0310pek.tif")        
evi_0310sos <- rast("../Data/Preprocessed/evi_0310sos.tif")        
ForFrag_spa3_2000 <- rast("../Data/Preprocessed/ForFrag_spa3_2000.tif")  
ForFrag_spa5_2000 <- rast("../Data/Preprocessed/ForFrag_spa5_2000.tif")  
mnNDVI00_09 <- rast("../Data/Preprocessed/mnNDVI00_09.tif")        
pden1999 <- rast("../Data/Preprocessed/pden1999.tif")           
pden2009 <- rast("../Data/Preprocessed/pden2009.tif")           
policy <- rast("../Data/Preprocessed/policy.tif")             
prec_annAvg09 <- rast("../Data/Preprocessed/prec_annAvg09.tif")      
slp <- rast("../Data/Preprocessed/slp.tif")                
temp_annAvg09 <- rast("../Data/Preprocessed/temp_annAvg09.tif")

#--------------------------------------------------------------------------------

#reclassify the categorical variables 'forest fragmentation' for better plotting
reclass_matrix <- matrix(c(
  1, 4,
  2, 3,
  3, 2,
  4, 1
), ncol = 2, byrow = TRUE)

# Apply the matrix
ForFrag_spa5_2000 <- classify(ForFrag_spa5_2000, reclass_matrix)

# Re-assign category names - fragmentation
levels(ForFrag_spa5_2000) <- data.frame(
  ID = c(1, 2, 3, 4),
  category = c("Core", "Perforation", "Edge", "Margin")
)
#--------------------------------------------------------------------------------
# Re-assign category names - policy map
levels(policy) <- data.frame(
  ID = c(1, 2, 3, 4, 5, 6),
  category = c("PR", "GR", "HGR", "CON", "PA", "PA_CA")
)

#--------------------------------------------------------------------------------
# create a list of the files
allFiles <- c(prec_annAvg09, temp_annAvg09, elev, slp,
              pden2009, evi_0310sos, evi_0310pek, mnNDVI00_09,
              dist2_cropland2000, dist2_forest2000, dist2_range2000, dist2_urban2000,
              dist2_ncropland2000, dist2_nforest2000, dist2_nrange2000, dist2_nurban2000,
              policy, ForFrag_spa5_2000)

#Mask to study extent 
all_rasters_combined <- mask(crop(allFiles, extRast), extRast)

#name files in the list
names(all_rasters_combined) <- c("Rainfall" ,
"Temperature",
"Elevation" ,
"Slope" ,
"Population density" ,
"SOS EVI" ,
"Peak EVI" ,
"Mean NDVI" ,
"Dist. to. crop." ,
"Dist. to. for." ,
"Dist. to. range" ,
"Dist. to. urban" ,
"Dist. to. new crop." ,
"Dist. to. new for.",
"Dist. to. new range",
"Dist. to. new urban",
"Policy",
"Forest fragmentation")

# Define titles (adjust names to match raster layers)
legend_titles <- c(
  "Rainfall" = "mm/year",
  "Temperature" = expression(paste(degree, "C")),
  "Elevation" = "meters",
  "Slope" = "Degrees",
  "Population density" = "Count/km2",
  "SOS EVI" = "Value",
  "Peak EVI" = "Value",
  "Mean NDVI" = "Value",
  "Dist. to. crop." = "m",
  "Dist. to. for." = "m",
  "Dist. to. range" = "m",
  "Dist. to. urban" = "m",
  "Dist. to. new crop." = "m",
  "Dist. to. new for." = "m",
  "Dist. to. new range" = "m",
  "Dist. to. new urban" = "m",
  "Policy" = "",
  "Forest fragmentation" = "")

# Create a function that contains specific styling to plot numeric variables
plot_variable <- function(raster_layer, layer_name) {
  
  # Look up the legend title; default to "Value" if not in our list
  leg_title <- if(layer_name %in% names(legend_titles)) legend_titles[layer_name] else "Value"
  
  ggplot() +
    geom_spatraster(data = raster_layer) +
    geom_spatvector(data = narok, fill = NA, color = 'grey10', linewidth = 0.5) +
    scale_fill_viridis_c(
      name = leg_title, # You can also use layer_name if units differ
      option = "plasma",
      direction = -1,
      na.value = "transparent",
      guide = guide_colorbar(
        title.position = "top",
        barwidth = 0.4,
        barheight = 4, # Smaller for grid layout
        frame.colour = "black"
      )
    ) +
    theme_minimal() +
    labs(title = layer_name) +
    theme(
      legend.position = "right",
      legend.title = element_text(size = 10, face = "plain"),
      legend.text = element_text(size = 8),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 12, face = "plain"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.border = element_rect(colour = "transparent", fill = NA, size = 0.5)
    )
}

# to plot categorical variables
plot_categorical_alt <- function(raster_layer, layer_name) {
  
  ggplot() +
    geom_spatraster(data = raster_layer) +
    geom_spatvector(data = narok, fill = NA, color = 'grey10', linewidth = 0.5) +
    scale_fill_brewer(
      name = NULL,
      palette = "Set3",
      na.translate = FALSE
    ) +
    theme_minimal() +
    labs(title = layer_name) +
    theme(
      legend.position = "right",
      # --- ADJUST KEY SIZE HERE ---
      legend.key.height = unit(0.4, "cm"), # Adjust vertical height of the boxes
      legend.key.width = unit(0.3, "cm"),  # Adjust horizontal width of the boxes
      legend.text = element_text(size = 7), # Ensure text stays proportional
      # ----------------------------
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 10, face = "plain"))
}

#--------------------------------------------------------------------------

#Generate the continuous plots
all_plots <- lapply(names(all_rasters_combined[[1:16]]), function(x) plot_variable(all_rasters_combined[[1:16]][[x]], x))

# Generate the 2 categorical plots
cat_plot1 <- plot_categorical_alt(all_rasters_combined[[17]], "Policy")
cat_plot2 <- plot_categorical_alt(all_rasters_combined[[18]], "Forest fragmentation")

# 3. Combine them into a single list
final_plot_list <- c(all_plots, list(cat_plot1, cat_plot2))
#--------------------------------------------------------------------------
final_grid <- wrap_plots(final_plot_list, ncol = 4)

print(final_grid)

# ggsave(file="../Figures/Fig2_ChangeFactors_May2026.tiff", final_grid,
#        units='px',width=4000, height=4000, dpi=400,compression='lzw')
