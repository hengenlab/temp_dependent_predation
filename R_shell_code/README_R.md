# Project Overview

This set of scripts handles the spatial analysis and modeling components of the project fit models, and generate figures for a project on vertebrate richness and predator-prey performance across temperature gradients. The workflow combines spatial raster operations (in bash and R), model fitting (including spatial Bayesian models), and experimental behavioral analysis. Scripts are numbered to reflect their approximate order in the workflow.

A separate set of Python scripts handles video processing and thermal sensor data.

Shell and R scripts are numbered in roughly the order they appear in the manuscript. Scripts 1–6 handle spatial data and figures; scripts 7–11 handle experimental data, results, and figures.

Although scripts are numbered sequentially, they can be run alone in any order. See https://github.com/hengenlab/temp_dependent_predation for revisions and updates.

---

## Script Summaries

### 1_project_to_gpkg.sh
Reprojects shapefiles and GeoPackages of species distributions to a cylindrical equal-area projection and saves them as GeoPackage (.gpkg). Non-native, vagrants, extinct, marine-only are excluded. Assumes raw spatial data are stored in a folder called `rangepolygons_raw`. All distribution data are stored in `Predation_Data/Richness_Distributions/`.

### 2_rasterize_richness.sh
Rasterizes equal-area `.gpkg` files of species ranges at three resolutions (standard, 2x, 4x) and sums them into richness rasters. Uses GDAL for rasterization and cleanup. Intermediate raster tiles are deleted after summing. Species counts are printed to a CSV.

### 3_spatial_functions.R
Defines functions for plotting spatial distributions. Also loads a multipolygon of ocean and lakes to mask out aquatic habitat in analysis.

### 4_compile_spatial.R
Loads richness rasters, constructs derived richness layers (e.g. endotherm:ectotherm ratio), and adds abiotic covariates like elevation, precipitation, and NPP. Outputs include a raster stack (`vert_ras`) and a dataframe version (`vert_df`) for modeling. Abiotic data are loaded from the subfolder `Spatial_data`, distributions from `Richness_Distributions`.

### 5_spatial_model.R
Uses spatial data generated in script 4 to model the drivers of vertebrate diversity. Log and non-log predictors are assessed, and the relative contribution of predictor variables is determined. Linear and spatial BYM2 models that control for spatial autocorrelation are generated, with summary output tables. In addition, observed and modeled spatial outputs are plotted (supplemental figure).

### 6_spatial_figures.R
Plots main spatial figures (Figures 1–3), as well as a supplemental shrew diversity analysis.

### 7_experim_functions.R
Houses helper functions used across experimental scripts, including outlier detection and quantifying contacts between predator and prey. Also includes custom plotting functions to generate experimental figures with mixed model regression fits (Figures 4–5).

### 8_experim_outliers.R
Cleans experimental DeepLabCut tracking data by removing implausible frame-wise jumps. Applies IQR-based outlier detection within and across trials. Plots raw and filtered data, including distributions of movement data from videos. Calculates 50th percentile and max values for velocity, acceleration, strike rate, etc. Summarizes cleaned DeepLabCut tracking data into per-trial outputs that are saved as CSVs for statistical analysis and plotting.

### 9_experim_results.R
Loads experimental datasets for analysis and saves as an R object: `capture_data.RData` for downstream analysis.

### 10_experim_stats.R
Loads `capture_data.RData` and fits linear mixed-effects models to assess the role of temperature and trial number, with mouse ID as a random effect. Regressions are generated and analyzed for initial and thermal trials. For mismatch experiments, `emmeans` is used to extract group contrasts between hot/cold conditions and whether temperature was matched or mismatched. Statistical outputs relevant to the main experimental figures are produced in table format; additional statistical outputs are also provided

### 11_experimental_figures.R
Plots key behavioral results from Figures 3–5 and supplemental experimental figures. Relies on outputs from scripts 8–10.

---
