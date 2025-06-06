library(tidyverse)
library(raster)
library(rasterVis)
library(ncdf4)
library(terra)
library(sp)

# Update paths
github_path = "/Users/jgradym/Documents/GitHub/mouse_capture"

# Load spatial plotting functions
source(file.path(github_path, "R_shell_code/3_spatial_functions.R"))

rich_path <- file.path(path, "/Richness_Distributions/richnessrasters")

# Get all raster file paths
rich_files <- list.files(rich_path, pattern = "\\.tif$", full.names = TRUE)

# Categorize into three groups (base = 48.25 X 48.25 km, 2x is 96.5 x 96.5 and 4x = 193 x 193)
rich_files_2x <- grep("2x\\.tif$", rich_files, value = TRUE)
rich_files_4x <- grep("4x\\.tif$", rich_files, value = TRUE)
rich_files_base <- setdiff(rich_files, c(rich_files_2x, rich_files_4x))

rich = brick(stack(rich_files_base))
rich_2x = brick(stack(rich_files_2x))
rich_4x = brick(stack(rich_files_4x))
names(rich)

# sum anura1 and anura2 if needed to get anura_richness, then drop anura1 and anura2
rich = dropLayer(rich, c("anura1_richness", "anura2_richness"))

anura_richness_2x = rich_2x[["anura1_richness_2x"]] + rich_2x[["anura2_richness_2x"]]
names(anura_richness_2x) =  "anura_richness_2x"
rich_2x = addLayer(rich_2x, anura_richness_2x)
rich_2x = dropLayer(rich_2x, c("anura1_richness_2x", "anura2_richness_2x"))
names(rich_2x)

anura_richness_4x = rich_4x[["anura1_richness_4x"]] + rich_4x[["anura2_richness_4x"]]
names(anura_richness_4x) =  "anura_richness_4x"
rich_4x = addLayer(rich_4x, anura_richness_4x)
rich_4x = dropLayer(rich_4x, c("anura1_richness_4x", "anura2_richness_4x"))
names(rich_4x)

# Return to rich_4x and 2x later (bottom of script)


#------ 1x Richness (48.25km x 48.25 km) -------
#-----Add Taxa ---
endo_rich = rich[["birds_richness"]] + rich[["mammals_richness"]]
amphibian_rich = rich[["anura_richness"]] + rich[["gymnophiona_richness"]] + rich[["caudata_richness"]]
ecto_rich = rich[["reptiles_richness"]] + amphibian_rich
#
#------Endos/Ectos
endo_ecto_rich = endo_rich/ecto_rich
endo_ecto_rich[is.infinite(values(endo_ecto_rich))] = NA # remove infinite values
values(endo_ecto_rich)[values(endo_ecto_rich) == 0] <- NA #remove 0 values

#----- Shrew Ratio Richness
soric_crocid_rich = rich[["soricinae_richness"]]/rich[["crocidurinae_richness"]]
soric_crocid_rich[is.infinite(values(soric_crocid_rich))] = NA
values(soric_crocid_rich)[values(soric_crocid_rich) == 0] <- NA

#-----Mammals/Reptiles
mammal_reptile_rich = rich[["mammals_richness"]]/rich[["reptiles_richness"]]
mammal_reptile_rich[is.infinite(values(mammal_reptile_rich))] = NA
values(mammal_reptile_rich)[values(mammal_reptile_rich) == 0] <- NA

rich = addLayer(rich, amphibian_rich, endo_rich, ecto_rich,  endo_ecto_rich, soric_crocid_rich, mammal_reptile_rich)
print(names(rich))  # Print all current layer names
sum(grepl("^layer\\.\\d+$", names(rich)))  # Count "layer.X" layers

# Count how many layers are currently in rich
num_layers <- length(names(rich))

# Count how many unnamed layers exist (layers named "layer.X")
num_new_layers <- sum(grepl("^layer\\.\\d+$", names(rich)))

# Get current layer names
layer_names <- names(rich)

# Rename only the newly added layers
layer_names[(num_layers - num_new_layers + 1):num_layers] <- c(
  "amphibian_rich", "endo_rich", "ecto_rich",  
  "endo_ecto_rich", "soric_crocid_rich", "mammal_reptile_rich"
)

# Apply the new names
names(rich) <- layer_names
names(rich) 

# Remove aquatic cells
ocean_mask <- rasterize(ocean_sp, raster48by48, field = 1, background = 0, getCover = TRUE)
ocean_mask[ocean_mask > 0.1] <- NA
vert_rich = mask(rich, ocean_mask)

#------------------------------------------------------------------------------
#------------------ Add Environmental Variables -------------------------------
#------------------------------------------------------------------------------

#----- Elevation

# Load high-res elevation (as terra object for fast projection)
# Access from Worldclim: https://www.worldclim.org/data/worldclim21.html
elev_highres <- rast(file.path(path, "Spatial_data/elevation/wc2.1_30s_elev.tif"))

# Fast projection high-res elevation to equal-area CRS
elev_projected <- terra::project(elev_highres, ceaproj, method = "bilinear")

# Convert for raster compatibility
elev_projected_raster <- raster(elev_projected)  

# Calculate aggregation factors to match vert_rich resolution
fact_x <- as.integer(res(vert_rich)[1] / res(elev_projected_raster)[1])
fact_y <- as.integer(res(vert_rich)[2] / res(elev_projected_raster)[2])

# Aggregate to get elevation summary statistics
elevation_min_agg <- aggregate(elev_projected_raster, fact = c(fact_x, fact_y), fun = min, na.rm = TRUE)
elevation_max_agg <- aggregate(elev_projected_raster, fact = c(fact_x, fact_y), fun = max, na.rm = TRUE)
elevation_sd_agg  <- aggregate(elev_projected_raster, fact = c(fact_x, fact_y), fun = sd,  na.rm = TRUE)
elevation_ea_agg = aggregate(elev_projected_raster, fact = c(fact_x, fact_y), fun = mean, na.rm = TRUE)

# Elevation range = max - min
elevation_range_agg <- elevation_max_agg - elevation_min_agg

# Resample back to match vert_rich grid
elevation_min    <- resample(elevation_min_agg, vert_rich)
elevation_max    <- resample(elevation_max_agg, vert_rich)
elevation_sd     <- resample(elevation_sd_agg, vert_rich)
elevation_range  <- resample(elevation_range_agg, vert_rich)
elevation        <- resample(elevation_ea_agg, vert_rich)


#---------- Add NPP: Net Primary Productivity (MODIS-derived)
# Source: https://neo.gsfc.nasa.gov/view.php?datasetId=MOD17A3H_Y_NPP
npp0 <- raster(file.path(path, "Spatial_data/npp/MOD17A3H_Y_NPP_2023-01-01_rgb_3600x1800.TIFF"))
plot(npp0)  # Max values of 255 should be NA

npp <- npp0
# Convert 255 to NA (common MODIS flag)
values(npp)[values(npp) == 255] <- NA  

# Reproject to match vert_rich resolution/extent
npp_ea <- projectRaster(npp, vert_rich)  
range(npp, na.rm = TRUE)
plot(npp_ea)

#------------ Load Temperature and precipitation (WorldClim 2.1, 2.5 min resolution) https://www.worldclim.org/data/worldclim21.html
precip0 <- raster(file.path(path, "Spatial_data/precipitation/wc2.1_2.5m_bio_12.tif")) # mean annual precipitation
temp0   <- raster(file.path(path, "Spatial_data/temperature/wc2.1_2.5m_bio_1.tif")) #mean annual temperature

#Equal area projection
temp_ea   <- projectRaster(temp0, vert_rich)
# Convert mm to cm/year
precip_ea <- projectRaster(precip0, vert_rich) / 10  

# Maximum monthly temperatures (multi-layer) 
max_temp_path <- file.path(path, "Spatial_data/temperature/wc2.1_2.5m_tmax")
max_temp0 <- stack(list.files(max_temp_path, pattern = ".tif$", full.names = TRUE))

# Calculate average and maximum temperature across months
max_temp_avg <- calc(max_temp0, fun = mean, na.rm = TRUE)
max_temp_max <- calc(max_temp0, fun = max, na.rm = T)  

# Project to match vert_rich
max_temp_avg_ea <- projectRaster(max_temp_avg, vert_rich)
max_temp_max_ea <- projectRaster(max_temp_max, vert_rich)

# Consistent names
names(temp_ea)         <- "temp"
names(precip_ea)       <- "precip"
names(npp_ea)          <- "npp"
names(max_temp_avg_ea) <- "max_temp_avg"
names(max_temp_max_ea) <- "max_temp_max"   # **Added name** to avoid unnamed layer
names(elevation_range) <- "elevation_range"
names(elevation_sd)    <- "elevation_sd"
names(elevation)    <- "elevation"

# Calculate 1/kT from mean temperature (for metabolic theory)
one_kT <- 1 / (8.617e-5 * (temp_ea + 273.15))
names(one_kT) <- "one_kT"
 
# Stack abiotic covariates into one RasterBrick
abiotic <- brick(temp_ea, one_kT, npp_ea, max_temp_avg_ea, max_temp_max_ea, precip_ea, elevation, elevation_range, elevation_sd)

# Combine with main biological raster
vert_rich2 <- addLayer(vert_rich, abiotic)
names(vert_rich2)


#------------------------------------------------------------------------------------------------------
# --------------------- Add climate projections -------------------------------------------------------
#--CMIP6/CESM2: Generate projected temperature and precipitation rasters for 2100 under different SSPs
# source: https://cds.climate.copernicus.eu/datasets/projections-cmip6?tab=download
#-------------------------------------------------------------------------------------------------------

# File paths for CMIP6 near-surface temperature projections

ssp1_2.6v0 <- file.path(path, "Spatial_data/CMIP6/CMIP6_near_surf_temp/CMIP6_SSP1_2.6_CESM2_2100_near_surf_temp/tas_Amon_CESM2_ssp126_r4i1p1f1_gn_21000115-21001215_v20200528.nc")
ssp2_4.5v0 <- file.path(path, "Spatial_data/CMIP6/CMIP6_near_surf_temp/CMIP6_SSP2_4.5_CESM2_2100_near_surf_temp/tas_Amon_CESM2_ssp245_r4i1p1f1_gn_21000115-21001215_v20200528.nc")
ssp5_8.5v0 <- file.path(path, "Spatial_data/CMIP6/CMIP6_near_surf_temp/CMIP6_SSP5_8.5_CESM2_2100_near_surf_temp/tas_Amon_CESM2_ssp585_r4i1p1f1_gn_21000115-21001215_v20200528.nc")

# Historical 1970–2000
historicalv0 <- file.path(path, "Spatial_data/CMIP6/CMIP6_near_surf_temp/CMIP6_Historical_CESM2_1970_2000_near_surf_temp/tas_Amon_CESM2_historical_r1i1p1f1_gn_19700115-20001215.nc")
# From 2014
historical2014 <- file.path(path, "Spatial_data/CMIP6/CMIP6_near_surf_temp/CMIP6_Historical_CESM2_2014_near_surf_temp/tas_Amon_CESM2_historical_r1i1p1f1_gn_20140115-20141215_v20190308.nc")

# Load historical temperature and convert to raster
historical_lat_lon <- nc_open(historicalv0)
lon <- ncvar_get(historical_lat_lon, "lon")
lat <- ncvar_get(historical_lat_lon, "lat", verbose = FALSE)
temperature <- ncvar_get(historical_lat_lon, "tas")
mean_temp <- apply(temperature, c(1, 2), FUN = mean, na.rm = TRUE)
mean_temp <- mean_temp - 273.15
historical_temp <- raster(mean_temp, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +datum=WGS84"))

# Open 2014 CMIP6 historical NetCDF
historical2014_lat_lon <- nc_open(historical2014)
lon_2014 <- ncvar_get(historical2014_lat_lon, "lon")
lat_2014 <- ncvar_get(historical2014_lat_lon, "lat", verbose = FALSE)
temperature_2014 <- ncvar_get(historical2014_lat_lon, "tas")
mean_temp_2014 <- apply(temperature_2014, c(1, 2), FUN = mean, na.rm = TRUE)
mean_temp_2014 <- mean_temp_2014 - 273.15
historical_temp_2014 <- raster(mean_temp_2014, xmn=min(lon_2014), xmx=max(lon_2014), ymn=min(lat_2014), ymx=max(lat_2014), crs=CRS("+proj=longlat +datum=WGS84"))
nc_close(historical2014_lat_lon)

# File paths for CMIP6 precipitation projections
ssp1_2.6v0_precip <- file.path(path, "Spatial_data/CMIP6/CMIP6_precip/CMIP6_CESM2_precip_SSP1-2.6/pr_Amon_CESM2_ssp126_r4i1p1f1_gn_21000115-21001215_v20200528.nc")
ssp2_4.5v0_precip <- file.path(path, "Spatial_data/CMIP6/CMIP6_precip/CMIP6_CESM2_precip_SSP2-4.5/pr_Amon_CESM2_ssp245_r4i1p1f1_gn_21000115-21001215_v20200528.nc")
ssp5.8.5v0_precip <- file.path(path, "Spatial_data/CMIP6/CMIP6_precip/CMIP6_CESM2_precip_SSP5-8.5/pr_Amon_CESM2_ssp585_r4i1p1f1_gn_21000115-21001215_v20200528.nc")
historical_precipv0 =  file.path(path, "Spatial_data/CMIP6/CMIP6_precip/CMIP6_CESM2_Historical_1970_2000_precip/pr_Amon_CESM2_historical_r1i1p1f1_gn_19700115-20001215.nc")

# Function to extract and project temperature or precipitation rasters from NetCDF files
color1 <- colorRampPalette(c("white","purple", "navy", "blue", "cyan","yellow","orange", "red", "black"))

process_nc_data <- function(file_path, var = "tas") {
  nc_data <- nc_open(file_path)
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = FALSE)
  variable_data <- ncvar_get(nc_data, var)
  mean_variable <- apply(variable_data, c(1, 2), FUN = mean, na.rm = TRUE)
  if (var %in% c("tas", "temperature")) {
    mean_variable <- mean_variable - 273.15  # Convert Kelvin to Celsius
  } else {
    mean_variable <- mean_variable * 3.154e+7 / 10  # Convert kg/m²/s to cm/year
  }
  raster_data <- raster(mean_variable, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +datum=WGS84"))
  raster_data <- flip(t(raster_data))
  extent(raster_data) <- c(xmin = 0, xmax = 360, ymin = -90, ymax = 90)
  raster_data <- raster::rotate(raster_data)
  raster_data <- projectRaster(raster_data, to = vert_rich, crs = "+proj=cea +lat_ts=30")
  nc_close(nc_data)
  plot_land(raster_data, color = color1(100))
  return(raster_data)
}

# Process future temperature projections
ssp1_2.6_temp <- process_nc_data(ssp1_2.6v0)
ssp2_4.5_temp <- process_nc_data(ssp2_4.5v0)
ssp5_8.5_temp <- process_nc_data(ssp5_8.5v0)
historical_temp <- process_nc_data(historicalv0)
historical_temp2014 <- process_nc_data(historical2014)

# Assign consistent names to all temperature rasters
names(ssp1_2.6_temp) <- "ssp1_2.6_temp"
names(ssp2_4.5_temp) <- "ssp2_4.5_temp"
names(ssp5_8.5_temp) <- "ssp5_8.5_temp"
names(historical_temp) <- "historical_temp"

# Process future precipitation projections
ssp1_2.6_precip <- process_nc_data(ssp1_2.6v0_precip, var = "pr")
ssp2_4.5_precip <- process_nc_data(ssp2_4.5v0_precip, var = "pr")
ssp5_8.5_precip <- process_nc_data(ssp5.8.5v0_precip, var = "pr")
historical_precip <- process_nc_data(historical_precipv0, var = "pr")

# Assign consistent names to all precipitation rasters
names(ssp1_2.6_precip) <- "ssp1_2.6_precip"
names(ssp2_4.5_precip) <- "ssp2_4.5_precip"
names(ssp5_8.5_precip) <- "ssp5_8.5_precip"
names(historical_precip) <- "historical_precip"


# Compute and store mean global temperatures for scenario comparison 
baseline_temp <- cellStats(historical_temp, mean, na.rm = TRUE)
ssp1_2.6_temp_increase <- cellStats(ssp1_2.6_temp, mean, na.rm = TRUE) - baseline_temp
ssp2_4.5_temp_increase <- cellStats(ssp2_4.5_temp, mean, na.rm = TRUE) - baseline_temp
ssp5_8.5_temp_increase <- cellStats(ssp5_8.5_temp, mean, na.rm = TRUE) - baseline_temp

# mean temp change from 2014
baseline_temp2014 <- cellStats(historical_temp2014, mean, na.rm = TRUE)
ssp1_2.6_temp_increase2014 <- cellStats(ssp1_2.6_temp, mean, na.rm = TRUE) - baseline_temp2014
ssp2_4.5_temp_increase2014 <- cellStats(ssp2_4.5_temp, mean, na.rm = TRUE) - baseline_temp2014
ssp5_8.5_temp_increase2014 <- cellStats(ssp5_8.5_temp, mean, na.rm = TRUE) - baseline_temp2014


# --- CMIP6 Scenario Projections: Uncorrected Delta Method ---
# Projected temperature and precipitation shifts are calculated as:
# ΔT = CMIP6 future temp - CMIP6 historical temp
# ΔP = CMIP6 future precip - CMIP6 historical precip
# Based on its accuracy and wide use, worldClim mean annual temperature (MAT) was used as the baseline for model fitting, based on climate data from 1970–2000. 
# For consistency, CMIP6 historical projections were also extracted for 1970–2000 to match this baseline period. 

# Create CMIP6 scenario raster stack (historical and future variables)
cmip <- brick(historical_temp, historical_precip, ssp1_2.6_temp, ssp2_4.5_temp, ssp5_8.5_temp, 
              ssp1_2.6_precip, ssp2_4.5_precip, ssp5_8.5_precip)

# Assign meaningful names to each layer
names(cmip) <- c(
  "historical_temp", "historical_precip",
  "ssp1_2.6_temp", "ssp2_4.5_temp", "ssp5_8.5_temp",
  "ssp1_2.6_precip", "ssp2_4.5_precip", "ssp5_8.5_precip"
)

# Add CMIP rasters to master dataset
vert_rich3 <- addLayer(vert_rich2, cmip)

# Convert to data frame for regression
data_df <- as_tibble(as.data.frame(vert_rich3, xy = TRUE))

# --- Project richness ratios under each CMIP scenario ---

# Function to project new richness ratio under ΔT and ΔP from CMIP6
predict_new_ratio <- function(starter_raster, projected_temp, projected_precip) {
  # Fit linear model using WorldClim data
  model <- lm(log(values(starter_raster)) ~ temp + precip, data = data_df)
  
  # Extract slope coefficients
  slope_temp <- coef(model)["temp"]
  slope_precip <- coef(model)["precip"]
  
  # Compute deltas
  delta_temp <- projected_temp - historical_temp
  delta_precip <- projected_precip - historical_precip
  
  # Predict log change and back-transform
  log_change <- delta_temp * slope_temp + delta_precip * slope_precip
  new_log_ratio <- log(starter_raster) + log_change
  new_ratio <- exp(new_log_ratio)
  
  # Clean up invalid values
  #new_ratio[is.infinite(values(new_ratio)) | values(new_ratio) == 0 | is.na(values(new_ratio))] <- NA
  
  return(new_ratio)
}

# --- Project mammal:reptile richness ratios under CMIP6 scenarios ---

# Predict future ratios using fitted model and CMIP6 deltas
mammal_reptile_1_2.6_ratio <- predict_new_ratio(mammal_reptile_rich,  ssp1_2.6_temp, ssp1_2.6_precip)
mammal_reptile_2_4.5_ratio <- predict_new_ratio(mammal_reptile_rich, ssp2_4.5_temp, ssp2_4.5_precip)
mammal_reptile_5_8.5_ratio <- predict_new_ratio(mammal_reptile_rich, ssp5_8.5_temp, ssp5_8.5_precip)

# Rename projected output layers
names(mammal_reptile_1_2.6_ratio) <- "mammal_reptile_1_2.6_ratio"
names(mammal_reptile_2_4.5_ratio) <- "mammal_reptile_2_4.5_ratio"
names(mammal_reptile_5_8.5_ratio) <- "mammal_reptile_5_8.5_ratio"

# --- Combine with master raster and crop spatial extent ---

vert_rich4 <- addLayer(vert_rich3, 
                       mammal_reptile_1_2.6_ratio, 
                       mammal_reptile_2_4.5_ratio, 
                       mammal_reptile_5_8.5_ratio)

# Define spatial crop extent (equal-area projection)
adjusted_ext <- extent(-17367530, 17372470, -6500000, 7342230)
vert_ras <- crop(vert_rich4, adjusted_ext)

# --- Log-transform richness and abiotic variables ---

log_precip <- log(vert_ras$precip)
log_npp <- log(vert_ras$npp)
log_endo_ecto_rich <- log(vert_ras$endo_ecto_rich)
log_mammal_reptile_rich <- log(vert_ras$mammal_reptile_rich)
log_squamate_rich <- log(vert_ras$squamates_richness)
log_turtle_rich <- log(vert_ras$turtles_richness)
log_crocodile_rich <- log(vert_ras$crocodile_richness)
log_salamander_rich <- log(vert_ras$caudata_richness)
log_frog_rich <- log(vert_ras$anura_richness)
log_caecilian_rich <- log(vert_ras$gymnophiona_richness)
log_mammal_rich <- log(vert_ras$mammals_richness)
log_bird_rich <- log(vert_ras$birds_richness)
log_soric_crocid_rich <- log(vert_ras$soric_crocid_rich)
log_endo_rich <- log(vert_ras$endo_rich)
log_ecto_rich <- log(vert_ras$ecto_rich)
log_amphibian_rich <- log(vert_ras$amphibian_rich)
log_reptile_rich <- log(vert_ras$reptiles_richness)

# Add log layers to raster stack
vert_ras <- addLayer(vert_ras, 
                     log_precip, log_npp, log_endo_ecto_rich,
                     log_soric_crocid_rich, log_mammal_reptile_rich,
                     log_squamate_rich, log_turtle_rich, log_crocodile_rich, 
                     log_salamander_rich, log_frog_rich, log_caecilian_rich,
                     log_mammal_rich, log_bird_rich, 
                     log_endo_rich,log_ecto_rich,log_amphibian_rich, log_reptile_rich)



# --- Rename log layers ---
names(vert_ras)

correct_names <- c(names(vert_ras)[1:37],
                   "log_precip", "log_npp", "log_endo_ecto_rich", 
                   "log_soric_crocid_rich", "log_mammal_reptile_rich",
                   "log_squamate_rich", "log_turtle_rich", "log_crocodile_rich", 
                   "log_salamander_rich", "log_frog_rich", "log_caecilian_rich",
                    "log_mammal_rich", "log_bird_rich",
                   "log_endo_rich", "log_ecto_rich","log_amphibian_rich", "log_reptile_rich")

names(vert_ras) <- correct_names
vert_ras <- brick(vert_ras)
names(vert_ras)
# --- Clean data frame (remove 0s and Infs), inspect key layers ---

vert_df <- as_tibble(as.data.frame(vert_ras, xy = TRUE))
vert_df[] <- lapply(vert_df, function(x) ifelse(is.infinite(x) | x == 0, NA, x))

#--------- write output file --------

writeRaster(vert_ras, file.path(path, "Spatial_data/vert_ras.grd"), overwrite = TRUE)
write_csv(vert_df, file.path(path, "Spatial_data/vert_df.csv"))


#--- Calculate % change in mammal:reptile richness from present to future


# Define a function to calculate percentage, mean, and standard deviation for each raster
calculate_stats <- function(raster_data) {
  fraction_above_1 <- mean(values(raster_data) > 1, na.rm = TRUE)
  percentage_above_1 <- round(fraction_above_1 * 100, 1)  # Convert to percentage and round to 1 decimal places
  
  raster_mean <- round(cellStats(raster_data, stat = 'mean', na.rm = TRUE), 2)  # Round mean to 2 decimal places
  raster_sd <- round(cellStats(raster_data, stat = 'sd', na.rm = TRUE), 2)  # Round standard deviation to 2 decimal points
  
  raster_name <- names(raster_data)  # Extract the name of the raster
  
  return(data.frame(Raster = raster_name, 
                    Percentage_Endo_Dominant = percentage_above_1,
                    Mean = raster_mean, 
                    SD = raster_sd))
}

#mammals vs reptiles
raster_list <- list(mammal_reptile_rich, mammal_reptile_1_2.6_ratio, mammal_reptile_2_4.5_ratio, mammal_reptile_5_8.5_ratio)
mammal_reptile_results <- do.call(rbind, lapply(raster_list, calculate_stats))

ratio_results = as_tibble(mammal_reptile_results)
ratio_results
write_csv(ratio_results, file.path(path, "Spatial_data/mammal_reptile_ratio.csv"))

###############################################
# ------- 2x and 4x supplemental analysis - compare effect of richness resolution ------


# Amphibians
names(rich_2x)
amphibian_rich_2x = rich_2x[["anura_richness_2x"]] + rich_2x[["gymnophiona_richness_2x"]] + rich_2x[["caudata_richness_2x"]]

# Endotherms
endo_rich_2x = rich_2x[["birds_richness_2x"]] +
  rich_2x[["mammals_richness_2x"]]

# Ectotherms
ecto_rich_2x = rich_2x[["reptiles_richness_2x"]] + amphibian_rich_2x

# Ratio
endo_ecto_rich_2x = endo_rich_2x / ecto_rich_2x
endo_ecto_rich_2x[is.infinite(values(endo_ecto_rich_2x))] = NA
values(endo_ecto_rich_2x)[values(endo_ecto_rich_2x) == 0] <- NA

# Add and rename
rich_2x = addLayer(rich_2x, amphibian_rich_2x, endo_rich_2x, ecto_rich_2x, endo_ecto_rich_2x)
names(rich_2x)[(nlayers(rich_2x)-3):nlayers(rich_2x)] = c(
  "amphibian_rich", "endo_rich", "ecto_rich", "endo_ecto_rich"
)


#- 4x
# Amphibians
amphibian_rich_4x = rich_4x[["anura_richness_4x"]] +
  rich_4x[["gymnophiona_richness_4x"]] +
  rich_4x[["caudata_richness_4x"]]

# Endotherms
endo_rich_4x = rich_4x[["birds_richness_4x"]] +
  rich_4x[["mammals_richness_4x"]]

# Ectotherms
ecto_rich_4x = rich_4x[["reptiles_richness_4x"]] + amphibian_rich_4x

# Ratio
endo_ecto_rich_4x = endo_rich_4x / ecto_rich_4x
endo_ecto_rich_4x[is.infinite(values(endo_ecto_rich_4x))] = NA
values(endo_ecto_rich_4x)[values(endo_ecto_rich_4x) == 0] <- NA

# Add and rename
rich_4x = addLayer(rich_4x, amphibian_rich_4x, endo_rich_4x, ecto_rich_4x, endo_ecto_rich_4x)
names(rich_4x)[(nlayers(rich_4x)-3):nlayers(rich_4x)] = c(
  "amphibian_rich", "endo_rich", "ecto_rich", "endo_ecto_rich"
)

# Add abiotic layers
abiotic_2x = projectRaster(from = abiotic, to = rich_2x, method = "bilinear")
abiotic_4x = projectRaster(from = abiotic, to = rich_4x, method = "bilinear")

rich_2x = addLayer(rich_2x, abiotic_2x)
rich_4x = addLayer(rich_4x, abiotic_4x)

# Remove aquatic habitat
rich_2x = mask(rich_2x, ocean_plot, inverse = T)
rich_4x = mask(rich_4x, ocean_plot, inverse = T)
names(rich_2x)

# Convert to dataframe
vert_2x_df= as.data.frame(rich_2x)
vert_4x_df = as.data.frame(rich_4x)

# Regress and compare slopes - very similar
lm1x = lm(log(endo_ecto_rich) ~ one_kT + log(npp) + log(precip) + elevation_range, data = vert_df)
summary(lm1x)

lm2x = lm(log(endo_ecto_rich) ~ one_kT + log(npp) + log(precip) + elevation_range, data = vert_2x_df)
summary(lm2x)

lm4x = lm(log(endo_ecto_rich) ~ one_kT + log(npp) + log(precip) + elevation_range, data = vert_4x_df)
summary(lm4x)
