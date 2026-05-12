# Modeling-Multifunctional-Landscape-Change
This repository contains the R-based analytical workflow and visualization scripts for the manuscript: "Modeling Multifunctional Landscape Change: Diagnosing Spatial Trade-offs and Drivers in Arid and Semi-Arid Systems". The codebase facilitates the reproduction of summarized outputs from land use/land cover (LULC) transitions and model validation presented in the study.
Table of Contents
Overview

Data Availability

Repository Structure

Software Requirements

Usage

Overview
The provided code performs categorical validation of land cover projection models, analyzes driver importance (e.g., policy, slope, and fragmentation), and generates publication-quality figures representing LULC transitions and error budgets.

Data Availability
The geospatial data required to run these scripts are archived on Zenodo (DOI: [Insert DOI]). The scripts expect a specific directory structure (see Repository Structure) to locate preprocessed rasters and model outputs.

Repository Structure
To ensure the scripts execute correctly, organize your local directory as follows:

Plaintext
├── Scripts/               # Contains the R scripts in this repository
├── Data/
│   ├── Preprocessed/      # .tif rasters (e.g., lu2010_4cls_90m.tif, slp.tif)
│   ├── LCM_Outputs/       # Land Change Modeler results (.rst files)
│   └── ComplementaryFiles/# Study area boundaries (.shp) and extent rasters
└── Figures/               # Output directory for generated plots
Script Descriptions
Kotikot_EcoInf_Fig1_ChangeFactors_1May2026.R: Visualizes LULC change factors, including distance to various land classes and topographic variables.

Kotikot_EcoInf_Fig3_BasePrediction_1May2026.R: Generates maps for reference (2018), hard prediction, and soft prediction scenarios.

Kotikot_EcoInf_Fig4_ErrorBudget_1May2026.R: Performs categorical validation comparing 2010 baseline, 2018 observed, and 2020 projection to calculate hits, misses, and false alarms.

Kotikot_EcoInf_Fig5_ErrorVsTransitions_1May2026.R: Analyzes model error categories specifically against transition types.

Kotikot_EcoInf2026_VarImportance_1May2026-.R: Visualizes the importance of drivers like Temperature, Slope, and Policy across transition models.

Kotikot_EcoInf_Fig7_Vars_vs_transitions_10May2026.R: Analyzes how physical and policy factors influence specific land use transitions.

Software Requirements
The analysis was performed using R (version 4.x). The following packages are required:

Spatial Analysis: terra, sf, tidyterra

Data Manipulation: tidyverse (dplyr, readr, tidyr)

Visualization: ggplot2, patchwork, cowplot, paletteer, tidytext

You can install the necessary packages with:

R
install.packages(c("terra", "sf", "tidyterra", "tidyverse", "patchwork", "cowplot", "paletteer", "tidytext"))
Usage
Clone this repository to your local machine.

Download the data from the Zenodo link provided in the Data Availability section.

Ensure the Data/ folder is in the parent directory of your scripts (or update the file paths at the top of each script).

Run the scripts in RStudio to reproduce the manuscript figures.

Authors
Kotikot et al. (2026)
