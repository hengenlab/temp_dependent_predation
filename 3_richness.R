library(tidyverse)
library(raster)
library(sf)
library(raster)
library(sf)
library(lobstr)
library(rasterVis)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(raster)
library(sf)
library(tidyverse)
source("/Users/jgradym/Documents/GitHub/mouse_capture/plot_land.R")
col1 = rev(c('#7f0000','#f46d43','#fdae61','#fee08b','#ffffbf','#e6f598','#abdda4','#41b6c4', '#3288bd'))
col2 = rev(c('#7f0000', '#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4', '#08306b'))

#change path
path = "/Users/jgradym/Desktop/Predation_Data"

source("/Users/jgradym/Documents/GitHub/mouse_capture/plot_land.R")
#-------------------------------------------------------------
#---- Generate Aquatic Polygons for plotting and masking -----
#-------------------------------------------------------------
ceaproj = "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

cont <- st_read('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Science/Enviro_data/Continents/eurasia.shp')[1] %>%
  st_transform(ceaproj) %>%
  st_simplify(dTolerance = 10000)
cont$SP_ID <- NULL
plot(cont)

# Lakes and Ocean source: https://www.naturalearthdata.com/downloads/110m-physical-vectors/
# lakes
lakes <- st_read('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Science/Enviro_data/ne_110m_lakes/ne_110m_lakes.shp')[7] %>%
  st_transform(ceaproj) %>%
  st_simplify(dTolerance = 10000) %>% #1 km res
  st_union() %>%
  st_cast("MULTIPOLYGON") %>%
  st_set_crs(ceaproj)
lakes$FID <- NULL
plot(lakes)
obj_size(lakes) 

# caspian sea 
#https://earthworks.stanford.edu/catalog/stanford-zb452vm0926
caspian <- st_read('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Science/Enviro_data/caspian/XCA_adm0.shp')[1] %>%
  st_transform(ceaproj) %>%
  st_simplify( dTolerance = 10000) # 1 km resol
plot(caspian)
caspian$ID_0 <- NULL
obj_size(caspian)
plot(caspian)

# Ocean
ocean <- st_read('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Science/Enviro_data/ne_110m_ocean/ne_110m_ocean.shp')[1] %>%st_transform(ceaproj)
ocean$scalerank <- NULL
plot(ocean)


#combine
ocean_plot <- rbind(lakes, caspian, ocean)
plot(ocean_plot)

# Define folder path
folder_path <- "/Users/jgradym/Desktop/Predation_Data/Richness_Distributions/richnessrasters"

# Get all raster file paths
rich_files <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)

# Categorize into three groups
rich_files_2x <- grep("2x\\.tif$", rich_files, value = TRUE)
rich_files_4x <- grep("4x\\.tif$", rich_files, value = TRUE)
rich_files_base <- setdiff(rich_files, c(rich_files_2x, rich_files_4x))

# Check the grouping
print(length(rich_files_2x))  # Should match expected number
print(length(rich_files_4x))  
print(length(rich_files_base))

rich_2x = brick(stack(rich_files_2x))
rich_4x = brick(stack(rich_files_4x))
rich = brick(stack(rich_files_base))
names(rich)
#-----Endotherms ---
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

rich = addLayer(rich, endo_rich, ecto_rich, amphibian_rich, endo_ecto_rich, soric_crocid_rich, mammal_reptile_rich)
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
  "endo_rich", "ecto_rich", "amphibian_rich", 
  "endo_ecto_rich", "soric_crocid_rich", "mammal_reptile_rich"
)

# Apply the new names
names(rich) <- layer_names

# Remove aquatic cells
vert_rich = mask(rich, ocean_plot, inverse = T)
vert_rich <- dropLayer(vert_rich, c("anura1_richness", "anura2_richness"))
names(vert_rich)

####### Add enviro variables ##################
#Environmental Variables
library(terra)
#https://www.worldclim.org/data/worldclim21.html
elevation0 = raster("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Richness_Distributions/Environment/elevation/wc2.1_30s_elev.tif")
elevation = projectRaster(elevation0, vert_rich)

# Define the target CRS
ceaproj <- "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

elevation0 = rast("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Richness_Distributions/Environment/elevation/wc2.1_30s_elev.tif")
elevation1 = raster("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Richness_Distributions/Environment/elevation/wc2.1_30s_elev.tif")

elevation_ea = projectRaster(elevation1, vert_rich)
names(elevation_ea) = "elevation"

# Reproject the high-resolution elevation raster using terra
elevation_proj0 <- terra::project(elevation0, ceaproj, method = "bilinear")
elevation_proj = raster(elevation_proj0)
# Create the larger equal-area raster (vert_rich)
extent_proj <- ext(elevation_proj)

# Resample the reprojected elevation raster to match the resolution of vert_rich
elevation_resampled <- resample(elevation_proj, vert_rich, method="bilinear")

# Calculate the factor by which to aggregate the high-resolution raster
fact_x <- res(vert_rich)[1] / res(elevation_proj)[1]
fact_y <- res(vert_rich)[2] / res(elevation_proj)[2]
fact_x <- as.integer(fact_x)
fact_y <- as.integer(fact_y)

# Aggregate the high-resolution raster to match the coarse grid for min and max values
elevation_min_agg <- raster::aggregate(elevation_proj, fact=c(fact_x, fact_y), fun=min, na.rm=TRUE)
elevation_max_agg <- raster::aggregate(elevation_proj, fact=c(fact_x, fact_y), fun=max, na.rm=TRUE)

# Calculate standard deviation and range
elevation_sd0 <- aggregate(elevation_proj, fact=c(fact_x, fact_y), fun=sd, na.rm=TRUE)
elevation_range0 <- elevation_max_agg - elevation_min_agg

# Adjust the range raster to match the extent and resolution of vert_rich
elevation_range <- resample(elevation_range0, vert_rich)
elevation_sd <- resample(elevation_sd0, vert_rich)
plot(elevation_sd)
plot(elevation_range)
plot_land(elevation_range)

#NPP #https://neo.gsfc.nasa.gov/view.php?datasetId=MOD17A3H_Y_NPP
npp0 = raster("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Richness_Distributions/Environment/NPP/MOD17A3H_Y_NPP_2023-01-01_rgb_3600x1800.TIFF") 
plot(npp0) #max value should be NA
#global(npp0, max, na.rm = T)
npp = npp0
values(npp)[values(npp) == 255] <- NA
npp_ea = projectRaster(npp, vert_rich)
range(npp, na.rm = T)
plot(npp_ea)
plot(npp0)

precip0 = raster("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Richness_Distributions/Environment/worldclim_data/wc2.1_2.5m/wc2.1_2.5m_bio_12.tif")
temp0 = raster("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Richness_Distributions/Environment/precip/wc2.1_2.5m_bio/wc2.1_2.5m_bio_1.tif")
temp_ea = projectRaster(temp0, vert_rich)
precip_ea = projectRaster(precip0, vert_rich)/10
plot(precip_ea)
plot_land_log(precip_ea, z_limits = c(1, 13), col = rev(br_ramp))

plot_land(npp_ea)

# compare to temp
#library(geodata)
#temp <- raster::getData("worldclim", var = "bio", res= 2.5)[[1]]/10
#str(temp)
#temp2 <- worldclim_global("bio", path = "~/Downloads/worldclim_data/temp.tif", res = 2.5)[["bio1"]] / 10
#identical(temp, temp2)
path ="/Users/jgradym/Downloads/wc2.1_2.5m_tmax/"
max_temp0 <- stack(list.files(path, pattern = ".tif$", full.names = TRUE))
# Calculate the average and maximum values across the raster stack
max_temp_avg <- calc(max_temp0, fun = mean, na.rm = TRUE)
max_temp_max <- calc(max_temp0, fun = max, na.rm = TRUE)

# Project the results to the desired CRS and resolution
max_temp_avg_ea <- projectRaster(max_temp_avg, vert_rich)
max_temp_max_ea <- projectRaster(max_temp_max, vert_rich)

plot_land(max_temp_avg_ea, z_limits = c(10, 40))
plot_land(max_temp_max_ea,  z_limits = c(10, 50))
plot(max_temp_avg_ea)
names(max_temp_avg_ea) = "max_temp_avg"
#temp_ea = projectRaster(temp, raster110by110)

one_kT = 1/(8.617e-5*(temp_ea+ 273.15))
names(one_kT) = "one_kT"
names(temp_ea) = "temp"
names(precip_ea) = "precip"
names(npp_ea) = "npp"
names(max_temp_avg) = "max_temp_avg"
names(elevation_range) = "elevation_range"
names(elevation_sd) = "elevation_sd"
# add to rasters
vert_rich
abiotic = brick(temp_ea, one_kT,  npp_ea, max_temp_avg_ea, precip_ea, elevation_ea, elevation_range, elevation_sd)
names(abiotic)
names(abiotic)[which(names(abiotic) == "wc2.1_30s_elev")] <- "elevation"
vert_rich2 =  addLayer(vert_rich, abiotic)
names(vert_rich2)
##############---------CMPI6--------------################
library(sf)
library(tidyverse)
library(raster)
library(ncdf4)
library(terra)
library(stars)
library(sp)
library(geodata)

#https://cds.climate.copernicus.eu/cdsapp#!/dataset/projections-cmip6?tab=form # monthly, surface temp, 2100
#ssp2_4.5v0 <- nc_open("/Users/jgradym/Downloads/CMIP6_SSP2_4.5_CESM2_2100/ts_Amon_CESM2_ssp245_r4i1p1f1_gn_21000115-21001215_v20200528.nc")
#ssp2_4.5v0 = file.path("/Users/jgradym/Downloads/CMIP6_SSP2_4.5_CESM2_2100/ts_Amon_CESM2_ssp245_r4i1p1f1_gn_21000115-21001215_v20200528.nc")
ssp1_2.6v0 = file.path("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/CMIP6/CMIP6_near_surf_temp/CMIP6_SSP1_2.6_CESM2_2100_near_surf_temp/tas_Amon_CESM2_ssp126_r4i1p1f1_gn_21000115-21001215_v20200528.nc")
ssp2_4.5v0 = file.path("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/CMIP6/CMIP6_near_surf_temp/CMIP6_SSP2_4.5_CESM2_2100_near_surf_temp/tas_Amon_CESM2_ssp245_r4i1p1f1_gn_21000115-21001215_v20200528.nc")
ssp4_6.0v0 = file.path("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/CMIP6/CMIP6_near_surf_temp/CMIP6_SSP4_6.0_CANESM5_2100_near_surf_tempe/tas_Amon_CanESM5_ssp460_r1i1p1f1_gn_21000116-21001216_v20190429.nc")
ssp5.8.5v0 = file.path("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/CMIP6/CMIP6_near_surf_temp/CMIP6_SSP5_8.5_CESM2_2100_near_surf_temp/tas_Amon_CESM2_ssp585_r4i1p1f1_gn_21000115-21001215_v20200528.nc")
historicalv0 = file.path("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/CMIP6/CMIP6_near_surf_temp/CMIP6_Historical_CESM2_2014_near_surf_temp/tas_Amon_CESM2_historical_r1i1p1f1_gn_20140115-20141215_v20190308.nc")

historical_lat_lon = nc_open(historicalv0)
lon <- ncvar_get(historical_lat_lon, "lon")
lat <- ncvar_get(historical_lat_lon, "lat", verbose = FALSE)
temperature <- ncvar_get(historical_lat_lon, "tas")  # Assuming 'ts' is the temperature variable
# Compute mean temperature and convert to Celsius
mean_temp <- apply(temperature, c(1, 2), FUN = mean, na.rm = TRUE)
mean_temp <- mean_temp - 273.15  # Convert Kelvin to Celsius
historical_temp <- raster(mean_temp, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +datum=WGS84"))

# precipitation

ssp1_2.6v0_precip = file.path("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/CMIP6/CMIP6_precip/precip_SSP1-2.6/pr_Amon_CESM2_ssp126_r4i1p1f1_gn_21000115-21001215_v20200528.nc")
ssp2_4.5v0_precip = file.path("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/CMIP6/CMIP6_precip/precip_SSP2-4.5/pr_Amon_CESM2_ssp245_r4i1p1f1_gn_21000115-21001215_v20200528.nc")
ssp4_6.0v0_precip = file.path("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/CMIP6/CMIP6_precip/precip_SSP4-6.0/pr_Amon_CanESM5_ssp460_r1i1p1f1_gn_21000116-21001216_v20190429.nc")
ssp5.8.5v0_precip = file.path("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/CMIP6/CMIP6_precip/precip_SSP5-8.5/pr_Amon_CESM2_ssp585_r4i1p1f1_gn_21000115-21001215_v20200528.nc")
nc_open(ssp1_2.6v0_precip)
nc_open(ssp5.8.5v0 )

#file_path = ssp5.8.5v0 
## Define the function to process the NetCDF data, create a raster, and plot it
color1 <- colorRampPalette(c("white","purple", "navy", "blue", "cyan","yellow","orange", "red", "black"))

# function to process data
# Function to process NetCDF data for a specified variable
process_nc_data <- function(file_path, var = "tas") { # 'tas' is the default variable name for temperature
  # Open the NetCDF file
  nc_data <- nc_open(file_path)
  
  # Extract longitude, latitude, and the specified variable data
  lon <- ncvar_get(nc_data, "lon")
  lat <- ncvar_get(nc_data, "lat", verbose = FALSE)
  variable_data <- ncvar_get(nc_data, var)
  
  # Compute mean of the variable data
  mean_variable <- apply(variable_data, c(1, 2), FUN = mean, na.rm = TRUE)
  
  # Convert Kelvin to Celsius if the variable is temperature ('tas' or 'temperature')
  if (var %in% c("tas", "temperature")) {
    mean_variable <- mean_variable - 273.15  # Convert Kelvin to Celsius
  } else {
    mean_variable <- mean_variable * 3.154e+7 / 10  # Convert kg/m²/s (or mm/s) to cm/year
  }
  
  # Create a raster from the mean variable data
  raster_data <- raster(mean_variable, xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=CRS("+proj=longlat +datum=WGS84"))
  
  # Transform the raster by transposing and flipping
  raster_data <- flip(t(raster_data))  
  
  # Set new extent if needed
  extent(raster_data) <- c(xmin = 0, xmax = 360, ymin = -90, ymax = 90)
  
  # Rotate the raster
  raster_data <- raster::rotate(raster_data)
  
  # Project raster to the desired CRS and crop to the extent
  ea <- "+proj=cea +lat_ts=30"
  raster_data <- projectRaster(raster_data, to = vert_rich, crs = ea)
  #ext <- extent(-17367530, 17372470, -7325770, 7342230)
  #raster_data <- crop(raster_data, ext)
  
  # Close the NetCDF file to free up resources
  nc_close(nc_data)
  
  # Plot the transformed raster
  plot_land(raster_data, color = color1(100))
  
  # Return the transformed raster
  return(raster_data)
}


ssp1_2.6_temp = process_nc_data(ssp1_2.6v0)#looks good
ssp2_4.5_temp = process_nc_data(ssp2_4.5v0)#looks good
ssp4_6.0_temp = process_nc_data(ssp4_6.0v0) #lower resolution
ssp5_8.5_temp = process_nc_data(ssp5.8.5v0) #looks good
historical_temp = process_nc_data(historicalv0) #looks good

#average temp
cellStats(historical_temp, mean, na.rm = T) #[1] 14.83689
cellStats(ssp1_2.6_temp, mean, na.rm = T) #[1]  16.29937
cellStats(ssp4_6.0_temp, mean, na.rm = T) #[1] 19.14282
cellStats(ssp5_8.5_temp, mean, na.rm = T) #[1] 20.2779

#Deviation 
baseline_temp = cellStats(historical_temp, mean, na.rm = T)
ssp1_2.6_temp_increase = cellStats(ssp1_2.6_temp, mean, na.rm = T) - baseline_temp # 1.4C
ssp2_4.5_temp_increase = cellStats(ssp2_4.5_temp, mean, na.rm = T) - baseline_temp  # 2.6 C
ssp4_6.0_temp_increase = cellStats(ssp4_6.0_temp, mean, na.rm = T) - baseline_temp #4.3 C
ssp5_8.5_temp_increase = cellStats(ssp5_8.5_temp, mean, na.rm = T) - baseline_temp #5.4 C


names(ssp1_2.6_temp) = "ssp1_2.6_temp"
names(ssp2_4.5_temp) = "ssp2_4.5_temp"
names(ssp4_6.0_temp) = "ssp4_6.0_temp"
names(ssp5_8.5_temp) = "ssp5_8.5_temp"
names(historical_temp) = "historical_temp"

ssp1_2.6_precip = process_nc_data(ssp1_2.6v0_precip, var = "pr")#looks good
ssp2_4.5_precip = process_nc_data(ssp2_4.5v0_precip, var = "pr")#looks good
ssp4_6.0_precip = process_nc_data(ssp4_6.0v0_precip, var = "pr") #lower resolution
ssp5_8.5_precip = process_nc_data(ssp5.8.5v0_precip, var = "pr") #looks good

names(ssp1_2.6_precip) = "ssp1_2.6_precip"
names(ssp2_4.5_precip) = "ssp2_4.5_precip"
names(ssp4_6.0_precip) = "ssp4_6.0_precip"
names(ssp5_8.5_precip) = "ssp5_8.5_precip"
#Temp Checks
# current temp

mat  <- raster("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Science/Enviro_data/worldclim/bioclim_0.5/wc2.1_30s_bio_1.tif")
mat_ea = projectRaster(mat, to = vert_rich)
cellStats(mat_ea, mean, na.rm = T)

max_temp  <- raster("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Science/Enviro_data/worldclim/bioclim_0.5/wc0.5/bio5_36.bil")/10
max_temp_ea = projectRaster(max_temp, to = vert_rich)

# add to raster brick
cmip = brick(historical_temp, ssp1_2.6_temp, ssp2_4.5_temp, ssp4_6.0_temp, ssp5_8.5_temp,ssp1_2.6_precip, ssp2_4.5_precip, ssp4_6.0_precip, ssp5_8.5_precip  )

vert_rich3 = addLayer(vert_rich2, cmip)
data_df = as_tibble(as.data.frame(vert_rich3, xy = T))

# Project new ratio
predict_new_ratio_temp <- function(starter_raster, projection_raster) {
  # Load your constant rasters
  #current_temp_raster <- raster('path_to_current_temp_raster.tif')
  # Fit the linear model using the provided data frame
  
  model <- lm(log(endo_reptile_rich) ~ temp, data = data_df)
  
  # Extract the intercept and slope from the model
  intercept <- coef(model)[1]
  slope <- coef(model)[2]
  
  # Calculate temperature deviation
  temp_dev <- projection_raster - historical_temp
  
  # Predict the change in log ratio using the linear model
  log_ratio_change <- temp_dev * slope
  
  # Calculate the new log ratio
  original_log_ratio <- log(starter_raster)
  new_log_ratio <- original_log_ratio + log_ratio_change
  
  # Convert the new log ratio back to the actual ratio
  new_ratio <- exp(new_log_ratio)
  
  return(new_ratio)
}


# Project new ratio
predict_new_ratio <- function( starter_raster, projected_temp, projected_precip) {
  # Fixed historical rasters
  historical_temp <- historical_temp
  historical_precip <- precip_ea
  
  # Extract the response variable from the starter raster
  response <- values(starter_raster)
  
  # Fit the linear model using the provided data frame
  model <- lm(log(response) ~ temp + precip, data = data_df)
  
  # Extract the intercepts and slopes from the model
  intercept <- coef(model)[1]
  slope_temp <- coef(model)["temp"]
  slope_precip <- coef(model)["precip"]
  
  # Calculate temperature and precipitation deviations
  temp_dev <- projected_temp - historical_temp
  precip_dev <- projected_precip - historical_precip
  
  # Predict the change in log ratio using the linear model
  log_ratio_change <- (temp_dev * slope_temp) + (precip_dev * slope_precip)
  
  # Calculate the new log ratio
  original_log_ratio <- log(starter_raster)
  new_log_ratio <- original_log_ratio + log_ratio_change
  
  # Convert the new log ratio back to the actual ratio
  new_ratio <- exp(new_log_ratio)
  
  # Ensure the output is in the format defined by vert_rich
  #new_ratio_projected <- projectRaster(new_ratio, to = vert_rich)
  
  #return(new_ratio_projected)
  return(new_ratio)
}


# mammal/reptile
mammal_reptile_1_2.6_ratio = predict_new_ratio(mammal_reptile_rich,  ssp1_2.6_temp, ssp1_2.6_precip)
mammal_reptile_2_4.5_ratio = predict_new_ratio(mammal_reptile_rich, ssp2_4.5_temp, ssp2_4.5_precip)
mammal_reptile_5_8.5_ratio =  predict_new_ratio(mammal_reptile_rich, ssp5_8.5_temp, ssp5_8.5_precip)
names(mammal_reptile_1_2.6_ratio) = "mammal_reptile_1_2.6_ratio"
names(mammal_reptile_2_4.5_ratio) = "mammal_reptile_2_4.5_ratio"
names(mammal_reptile_5_8.5_ratio) = "mammal_reptile_5_8.5_ratio"

plot_center(mammal_reptile_5_8.5_ratio)


#combined dataset
vert_rich4 = addLayer(vert_rich3, mammal_reptile_1_2.6_ratio, mammal_reptile_2_4.5_ratio, mammal_reptile_5_8.5_ratio )

adjusted_ext <- extent(-17367530, 17372470, -6500000, 7342230) # Example ymin set to -6500000
vert_ras <- crop(vert_rich4, adjusted_ext)
names(vert_ras)

# Add log rasters
# Add logged columns to vert_ras
log_precip <- log(vert_ras$precip)
log_npp <- log(vert_ras$npp)
log_endo_ecto_rich <- log(vert_ras$endo_ecto_rich)
log_mammal_reptile_rich <- log(vert_ras$mammal_reptile_rich)
log_squamate_rich <- log(vert_ras$squamates_richness)
log_turtle_rich <- log(vert_ras$turtles_richness)
log_salamander_rich <- log(vert_ras$caudata_richness)
log_frog_rich <- log(vert_ras$anura_richness)
log_mammal_rich <- log(vert_ras$mammals_richness)
log_bird_rich <- log(vert_ras$birds_richness)
log_soric_crocid_rich <- log(vert_ras$soric_crocid_rich)

vert_ras = addLayer(vert_ras, log_precip, log_npp,log_endo_ecto_rich,log_soric_crocid_rich, log_mammal_reptile_rich,
                    log_squamate_rich, log_turtle_rich, log_salamander_rich, log_frog_rich, log_mammal_rich,
                    log_bird_rich)
correct_names <- c(names(vert_ras)[1:39],  # Keep first 39 names
                   "log_precip", "log_npp", "log_endo_ecto_rich", "log_soric_crocid_rich", 
                   "log_mammal_reptile_rich", "log_squamate_rich", "log_turtle_rich", 
                   "log_salamander_rich", "log_frog_rich", "log_mammal_rich", "log_bird_rich")

correct_names <- c(names(vert_ras)[1:39],  # Keep first 39 names
                   "log_precip", "log_npp", "log_endo_ecto_rich", "log_soric_crocid_rich", 
                   "log_mammal_reptile_rich", "log_squamate_rich", "log_turtle_rich", 
                   "log_salamander_rich", "log_frog_rich", "log_mammal_rich", "log_bird_rich")

names(vert_ras) <- correct_names
vert_ras <- brick(vert_ras)

names(vert_ras)
# Add logged columns
vert_df = as_tibble(as.data.frame(vert_ras, xy = T))
vert_df[] <- lapply(vert_df, function(x) ifelse(is.infinite(x) | x == 0, NA, x))

plot_land(vert_ras[["elevation"]])
plot_center(vert_ras[["mammal_reptile_rich"]])
plot_center(vert_ras[["mammal_reptile_1_2.6_ratio"]], col = br_ramp2)
plot_center(vert_ras[["mammal_reptile_2_4.5_ratio"]], col = br_ramp2)
plot_center(vert_ras[["mammal_reptile_5_8.5_ratio"]], col = br_ramp2)

#--------- write output file --------


writeRaster(vert_ras, "/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Mouse/Richness/vert_ras.grd", overwrite = T)
write_csv(vert_df, "/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Mouse/Richness/vert_df.csv")

#calculate change in 


#----------descriptive stats --------------
# Define a function to calculate percentage, mean, and standard deviation for each raster
calculate_stats <- function(raster_data) {
  fraction_above_1 <- sum(values(raster_data) > 1, na.rm = TRUE) / sum(!is.na(values(raster_data)))
  percentage_above_1 <- round(fraction_above_1 * 100, 1)  # Convert to percentage and round to 1 decimal places
  
  raster_mean <- round(cellStats(raster_data, stat = 'mean', na.rm = TRUE), 2)  # Round mean to 2 decimal places
  raster_sd <- round(cellStats(raster_data, stat = 'sd', na.rm = TRUE), 2)  # Round standard deviation to 2 decimal points
  
  raster_name <- names(raster_data)  # Extract the name of the raster
  
  return(data.frame(Raster = raster_name, 
                    Percentage_Above_1 = percentage_above_1,
                    Mean = raster_mean, 
                    SD = raster_sd))
}

#mammals vs reptiles
raster_list <- list(mammal_reptile_rich, mammal_reptile_1_2.6_ratio, mammal_reptile_2_4.5_ratio, mammal_reptile_5_8.5_ratio)
mammal_reptile_results <- do.call(rbind, lapply(raster_list, calculate_stats))
print(mammal_reptile_results)

ratio_results = as_tibble(mammal_reptile_results)
ratio_results
write_csv(ratio_results, "/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Mouse/Richness/ratio_results.csv")

