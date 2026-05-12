#' @title Variable Importance and Spatial Transition Visualization
#' @description Processes LCM (Land Change Modeler) outputs to visualize driver 
#' importance across four transition types and maps their spatial distribution.
#' @author [Kotikot at al.]
#' @date May 2026

# Load Required Libraries
library(sf)         # Vector data handling (Shapefiles)
library(terra)      # Modern raster analysis
library(tidytext)   # For reorder_within() functionality in facets
library(tidyverse)  # Data manipulation and ggplot2
library(cowplot)    # Plot arrangement and composition
library(tidyterra)  # Specialized ggplot layers for spatrasters/vectors

# 1. Configuration & Lookups ----------------------------------------------
# Mapping original variable names to labels
VAR_LOOKUP <- c(
  "temp_annAvg09_b1"      = "Temperature",
  "evi_0310pek_b1"        = "EVI-Peak",
  "dist2_range2000_b1"    = "dist-Rangeland",
  "cr_policyEL"           = "Policy",
  "slp_b1"                = "Slope",
  "dist2_cropland2000_b1" = "dist-Cropland",
  "dist2_ncropland2000_b1"= "dist_newCropland",
  "elev_b1"               = "Elevation",
  "fc_policyEL"           = "Policy",
  "fc_Frag5EL"            = "Forest-Fragmentation",
  "prec_annAvg09_b1"      = "Rainfall",
  "pden2009_b1"           = "Population Density",
  "fr_Frag5EL"            = "Forest-Fragmentation",
  "fr_policyEL"           = "Policy",
  "evi_0310sos_b1"        = "EVI-SOS",
  "rc_policyEL"           = "Policy",
  "mnNDVI_00_09_b1"       = "NDVI-Mean"
)

# Panel indexing and label mapping for the facet wrap
PANEL_LOOKUP <- c("RC" = "A", "FC" = "B", "FR" = "C", "CR" = "D")
LABELS_LOOKUP <- c("A" = "RC", "B" = "FC", "C" = "FR", "D" = "CR")

# Data Processing (Variable Importance) --------------------------------

# Read the LCM importance summary CSV
fl <- read_csv("../Data/LCM_Outputs/LCM_output_Variable_Importance_summary.csv", show_col_types = FALSE)

# Clean and transform importance data
fl_clean <- fl %>% 
  # Filter out baseline "all" variables and irrelevant minor transitions
  filter(!Vars == "all", !trans %in% c("CF", "CU", "RU", "RF")) %>% 
  mutate(
    nms = recode(Vars, !!!VAR_LOOKUP),
    pans = recode(trans, !!!PANEL_LOOKUP)
  ) %>% 
  group_by(pans) %>% 
  arrange(desc(change)) %>% 
  ungroup()

# Visualization (Variable Importance) ----------------------------------
varImp <- ggplot(fl_clean, aes(y = as.numeric(change), x = reorder_within(nms, change, pans))) +
  geom_col(color = "grey30", fill = "gray30") +
  coord_flip() + # Horizontal bars for better text readability
  # Facet by transition type; scales="free" allows unique variable sets per panel
  facet_wrap(~pans, scales = "free", labeller = as_labeller(LABELS_LOOKUP)) +
  scale_x_reordered() + # Works with tidytext::reorder_within to maintain order per facet
  labs(x = NULL, y = "Change in model skill measure (%)") +
  theme_bw(base_size = 20) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

# Spatial Data Processing ----------------------------------------------

# Load county boundary
narok <- st_read("../Data/ComplementaryFiles/narok_county_utm.shp", quiet = TRUE)

# Define file paths for specific transition rasters
trans_files <- c("rc_transition.rst", "fc_transition.rst", "fr_transition.rst", "cr_transition.rst")
trans_path <- "../Data/LCM_Outputs//"

# Load rasters and apply offsets to create unique IDs for the final mosaic
# This ensures each transition has a distinct integer value for classification
rc_r <- terra::rast(paste0(trans_path, "rc_transition.rst"))
fc_r <- terra::rast(paste0(trans_path, "fc_transition.rst")) + 1
fr_r <- terra::rast(paste0(trans_path, "fr_transition.rst")) + 2
cr_r <- terra::rast(paste0(trans_path, "cr_transition.rst")) + 3

# Combine minor "Other" transitions into a single layer
oth_files <- c("rf_transition.rst", "cf_transition.rst", "ru_transition.rst", "cu_transition.rst")
oth_r <- terra::mosaic(terra::sprc(lapply(paste0(trans_path, oth_files), terra::rast))) + 4

# Create final mosaic and define categorical levels for the legend
mp <- terra::mosaic(rc_r, fc_r, fr_r, cr_r, oth_r)
levels(mp) <- data.frame(
  ID = 1:5, 
  LandCover = c("Rangeland to Cropland (RC 11%)",
                "Forest to Cropland (FC 9.5%)",
                "Forest to Rangeland (FR 7.3%)",
                "Cropland to Rangeland (CR 7.3%)",
                "Others (RF, CF, RU, CU (2.5%)"))


# Combined Plotting ----------------------------------------------------

# Generate spatial map using tidyterra extensions
spatial_plot <- ggplot() +
  tidyterra::geom_spatraster(data = mp) +
  # Add county boundary outline
  geom_spatvector(data = narok, fill = NA, color = 'grey10', linewidth = 0.5) +
  scale_fill_manual(
    values = c('darkorchid', "chartreuse3", "brown2", "deepskyblue3", "grey40"),
    na.translate = FALSE, # Strictly prevents NA from appearing in legend
    drop = TRUE,           # Drops levels that aren't present in the data
    name = ""
  ) +
  theme_void() + # Clean layout for maps (removes grid/axes)
  theme(legend.position = "bottom", legend.direction = "vertical",
        legend.text=element_text(size=15),
        # Transparency settings
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        legend.background = element_rect(fill = "transparent", colour = NA),
        legend.key = element_rect(fill = "transparent", colour = NA))

# Assemble Panel (a) and (b) into one figure using cowplot
final_fig <- plot_grid(
  spatial_plot, varImp, 
  labels = c("(a)", "(b)"), 
  label_size = 24, 
  rel_widths = c(1, 1.5) # Spatial map given less horizontal space than 4-panel graph
)

# 6. Export ---------------------------------------------------------------
# ggsave("../Figures/Fig7_VarImportance_May2026.tiff", final_fig,
#        width = 12, height = 7, dpi = 600, compression = 'lzw')


