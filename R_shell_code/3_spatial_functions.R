# Required libraries
library(raster)
library(rasterVis)
library(lattice)
library(latticeExtra)
library(sf)  # for reading ocean shapefiles
library(sp)  # 
library(ggplot2)

#-----------------------------------------------------
# File path to project root (used for accessing all input files)
# Update to computer
path <- "/Users/jgradym/Desktop/Predation_Data"

#-----------------------------------------------------
# Plot theme (for ggplot, if needed)
#-----------------------------------------------------
theme_plot <- theme(
  panel.grid = element_blank(), 
  aspect.ratio = .75,
  axis.text = element_text(size = 18, color = "black"), 
  axis.ticks.length = unit(0.2, "cm"),
  axis.title = element_text(size = 18),
  axis.title.y = element_text(margin = margin(r = 10)),
  axis.title.x = element_text(margin = margin(t = 10)),
  axis.title.x.top = element_text(margin = margin(b = 5)),
  plot.title = element_text(size = 18, face = "plain", hjust = 10),
  panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
  panel.background = element_blank(),
  strip.background = element_blank(),
  legend.position = "none",
  text = element_text(family = 'Helvetica')
)

#-----------------------------------------------------
# Color palette: Extended ColorBrewer2 RdBu diverging palette
#-----------------------------------------------------
col_br <- rev(c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7',
                '#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061'))
col_br2 = c(
  "#08306B", "#0A306D", "#0C3070", "#0F3172", "#113175", "#143177", "#16327A", "#19327C", "#1B327F", "#1E3381",
  "#203384", "#233487", "#253489", "#28348C", "#2A358E", "#2D3591", "#2F3593", "#333996", "#37409A", "#3B489E",
  "#3F4FA1", "#4356A5", "#475DA9", "#4B64AC", "#4F6CB0", "#5373B3", "#577AB7", "#5B81BB", "#5F88BE", "#6390C2",
  "#6797C6", "#6B9EC9", "#6FA5CD", "#74ADD1", "#7BB1D3", "#83B5D5", "#8BBAD7", "#93BEDA", "#9BC3DC", "#A3C7DE",
  "#ABCCE1", "#B3D0E3", "#BBD5E5", "#C3D9E8", "#CBDEEA", "#D3E2EC", "#DBE7EE", "#E3EBF1", "#EBF0F3", "#F3F4F5",
  "#F6F2F1", "#F6EAE6", "#F6E2DB", "#F6D9D0", "#F6D1C5", "#F6C8BA", "#F5C0B0", "#F5B8A5", "#F5AF9A", "#F5A78F",
  "#F59F84", "#F49679", "#F48E6E", "#F48663", "#F47D58", "#F4754D", "#F36C42", "#EF6641", "#EA5F3F", "#E5593D",
  "#E0523B", "#DC4B3A", "#D74538", "#D23E36", "#CD3834", "#C83133", "#C42A31", "#BF242F", "#BA1D2D", "#B5172C",
  "#B0102A", "#AC0928", "#A70326", "#A30025", "#9F0023", "#9B0022", "#970020", "#94001F", "#90001D", "#8C001C",
  "#88001A", "#850019", "#810017", "#7D0016", "#790014", "#760013", "#720011", "#6E0010", "#6A000E", "#67000D"
)

#-----------------------------------------------------
# Global equal-area projection raster template (48km resolution)
#-----------------------------------------------------
res_48 <- c(48250, 48250)
raster48by48 <- raster(xmn = -17367530, xmx = 17372470, ymn = -7325770, ymx = 7342230)
res(raster48by48) <- res_48
ceaproj <- "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
crs(raster48by48) <- ceaproj 

#---------------------------------------------------------------------------------------------------------
# Load polygon of for plotting and polygons of ocean, seas and lakes for masking data aquatic habitat
#--------------------------------------------------------------------------


# Lakes and Ocean source: https://www.naturalearthdata.com/downloads/110m-physical-vectors/
# lakes
lakes <- st_read(file.path(path,'Spatial_data/lakes/ne_110m_lakes.shp'))[7] %>%
  st_transform(ceaproj) %>%
  st_simplify(dTolerance = 10000) %>% #1 km res
  st_union() %>%
  st_cast("MULTIPOLYGON") %>%
  st_set_crs(ceaproj)
lakes$FID <- NULL

# caspian sea 
#https://earthworks.stanford.edu/catalog/stanford-zb452vm0926
caspian <- st_read(file.path(path,'Spatial_data/caspian/XCA_adm0.shp'))[1] %>%
  st_transform(ceaproj) %>%
  st_simplify( dTolerance = 10000) # 1 km resol
caspian$ID_0 <- NULL

# Ocean
ocean <- st_read(file.path(path,'Spatial_data/ocean/ne_110m_ocean.shp'))[1] %>% st_transform(ceaproj)
ocean$scalerank <- NULL

#combine
ocean_plot <- rbind(lakes, caspian, ocean)

# sp version
ocean_sp <- as(ocean_plot, "Spatial")

# Adjust to standard global equal-area raster extent
adjusted_ext <- extent(-17367530, 17372470, -6500000, 7342230)
ocean_sp <- crop(ocean_sp, adjusted_ext)

#-----------------------------------------------------
# Create ocean mask raster (0 = land, NA = ocean)
#-----------------------------------------------------
ocean_mask <- rasterize(ocean_sp, raster48by48, field = 1, background = 0, getCover = TRUE)
ocean_mask[ocean_mask > 0.1] <- NA

#-----------------------------------------------------
# Plot a raster with raw (untransformed) values
# Common for absolute quantities like temperature or richness
# Includes a border for eventually clipping of Antarctica, where no richness data is used
#-----------------------------------------------------
plot_land <- function(raster, color = col_br, cuts = 99, 
                      z_limits = NULL, num_labels = 6, maxpixels = ncell(raster)) {
  
  # Remove zero values (often undefined or uninformative in raw scale)
  raster[raster == 0] <- NA
  
  # Set value limits for color scaling if not specified
  if (is.null(z_limits)) {
    z_limits <- range(raster[], na.rm = TRUE)
  }
  
  # Define tick positions for continuous color scale
  at_values <- seq(z_limits[1], z_limits[2], length.out = cuts)
  at_values <- at_values[at_values >= z_limits[1] & at_values <= z_limits[2]]
  
  # Create evenly spaced axis tick marks and rounded labels
  pretty_indices <- pretty(z_limits, n = num_labels)
  pretty_indices <- pretty_indices[pretty_indices >= z_limits[1] & pretty_indices <= z_limits[2]]
  pretty_labels <- round(pretty_indices, 2)
  
  # Configure colorbar
  colorkey <- list(at = at_values, labels = list(at = pretty_indices, labels = pretty_labels))
  color_ramp <- colorRampPalette(color)(length(at_values) - 1)
  
  # Build plot
  raster_plot <- levelplot(raster, col.regions = color_ramp, at = at_values, colorkey = colorkey, 
                           maxpixels = maxpixels, xlab = NULL, ylab = NULL, 
                           scales = list(x = list(draw = FALSE), y = list(draw = FALSE)), 
                           par.settings = list(layout.heights = list(top.padding = 0, bottom.padding = 0)),
                           margin = FALSE)
  
  # Add land/ocean boundary (used to clip out Antarctica)
  raster_plot <- raster_plot + latticeExtra::layer(sp.polygons(ocean_sp, col = "black", fill = NA, lwd = 0.5))
  
  # Display
  print(raster_plot)
}

#-----------------------------------------------------
# Plot a raster on a log2 scale, using default continuous palette
# Typically used for environmental variables (e.g., NPP, precipitation)
plot_land_log <- function(raster, color = col_br, breaks = 99, z_limits = NULL, labels = 6, maxpixels = ncell(raster)) {
  
  # Transform to log2 scale
  log_raster <- log2(raster)
  
  # Set z-axis limits if not specified
  if (is.null(z_limits)) { 
    z_limits <- range(log_raster[], na.rm = TRUE) 
  }
  
  # Define tick positions and color gradient
  at_values <- seq(z_limits[1], z_limits[2], length.out = breaks)
  pretty_indices <- pretty(at_values, n = labels)
  
  # Create human-readable labels from log2 values
  pretty_labels <- sapply(pretty_indices, function(x) {
    if (x >= 1) {
      as.character(round(2^x, 0))               # e.g., log2(8) = 3 -> "8"
    } else {
      fraction <- as.integer(2^(abs(x)))        # e.g., log2(0.5) = -1 -> "1/2"
      if (fraction == 1) "1" else paste0("1/", fraction)
    }
  })
  
  # Set up color scale and key
  colorkey <- list(at = at_values, labels = list(at = pretty_indices, labels = pretty_labels))
  color_ramp <- colorRampPalette(color)(length(at_values))
  
  # Create plot
  raster_plot <- levelplot(log_raster, col.regions = color_ramp, at = at_values, colorkey = colorkey, 
                           maxpixels = maxpixels, xlab = NULL, ylab = NULL, 
                           scales = list(x = list(draw = FALSE), y = list(draw = FALSE)), 
                           par.settings = list(layout.heights = list(top.padding = 0, bottom.padding = 0)),
                           margin = FALSE)
  
  # Add ocean boundaries
  raster_plot <- raster_plot + latticeExtra::layer(sp.polygons(ocean_sp, col = "black", fill = NA, lwd = 0.5))
  
  # Display
  print(raster_plot)
}
plot_land_log <- function(raster, color = col_br, breaks = 99, z_limits = NULL, num_labels = 6, maxpixels = ncell(raster)) {
  
  # Transform to log2 scale
  log_raster <- log2(raster)
  
  # Set z-axis limits if not specified
  if (is.null(z_limits)) { 
    z_limits <- range(log_raster[], na.rm = TRUE) 
  }
  
  # Define tick positions and color gradient
  at_values <- seq(z_limits[1], z_limits[2], length.out = breaks)
  pretty_indices <- pretty(at_values, n = num_labels)
  
  # Create human-readable labels from log2 values
  pretty_labels <- sapply(pretty_indices, function(x) {
    if (x >= 1) {
      as.character(round(2^x, 0))               # e.g., log2(8) = 3 -> "8"
    } else {
      fraction <- as.integer(2^(abs(x)))        # e.g., log2(0.5) = -1 -> "1/2"
      if (fraction == 1) "1" else paste0("1/", fraction)
    }
  })
  
  # Set up color scale and key
  colorkey <- list(at = at_values, labels = list(at = pretty_indices, labels = pretty_labels))
  color_ramp <- colorRampPalette(color)(length(at_values))
  
  # Create plot
  raster_plot <- levelplot(log_raster, col.regions = color_ramp, at = at_values, colorkey = colorkey, 
                           maxpixels = maxpixels, xlab = NULL, ylab = NULL, 
                           scales = list(x = list(draw = FALSE), y = list(draw = FALSE)), 
                           par.settings = list(layout.heights = list(top.padding = 0, bottom.padding = 0)),
                           margin = FALSE)
  
  # Add ocean boundaries
  raster_plot <- raster_plot + latticeExtra::layer(sp.polygons(ocean_sp, col = "black", fill = NA, lwd = 0.5))
  
  # Display
  print(raster_plot)
}

plot_land_log <- function(raster, color = col_br, breaks = 99, z_limits = NULL, num_labels = 6, maxpixels = ncell(raster)) {
  
  # Transform to log2 scale
  log_raster <- log2(raster)
  
  # Replace non-finite values with NA (handles zeros or negative)
  log_raster[!is.finite(log_raster[])] <- NA
  
  # Set z-axis limits if not specified
  if (is.null(z_limits)) { 
    z_limits <- range(log_raster[], na.rm = TRUE) 
  }
  
  # Define tick positions and color gradient
  at_values <- seq(z_limits[1], z_limits[2], length.out = breaks)
  pretty_indices <- pretty(at_values, n = num_labels)
  
  # Create human-readable labels from log2 values
  pretty_labels <- sapply(pretty_indices, function(x) {
    if (x >= 1) {
      as.character(round(2^x, 0))               # e.g., log2(8) = 3 -> "8"
    } else {
      fraction <- as.integer(2^(abs(x)))        # e.g., log2(0.5) = -1 -> "1/2"
      if (fraction == 1) "1" else paste0("1/", fraction)
    }
  })
  
  # Set up color scale and key
  colorkey <- list(at = at_values, labels = list(at = pretty_indices, labels = pretty_labels))
  color_ramp <- colorRampPalette(color)(length(at_values))
  
  # Create plot
  raster_plot <- levelplot(log_raster, col.regions = color_ramp, at = at_values, colorkey = colorkey, 
                           maxpixels = maxpixels, xlab = NULL, ylab = NULL, 
                           scales = list(x = list(draw = FALSE), y = list(draw = FALSE)), 
                           par.settings = list(layout.heights = list(top.padding = 0, bottom.padding = 0)),
                           margin = FALSE)
  
  # Add ocean boundaries
  raster_plot <- raster_plot + latticeExtra::layer(sp.polygons(ocean_sp, col = "black", fill = NA, lwd = 0.5))
  
  # Display
  print(raster_plot)
}

#-----------------------------------------------------
# Function: plot_center
# Plot a raster on a log2 scale, centered at 1:1 ratio (i.e., log2 = 0)
# Useful for highlighting fold changes, e.g., richness ratios

plot_center <- function(raster, color = col_br2, breaks = 99, z_limits = NULL, labels = 6, maxpixels = ncell(raster)) {
  
  # Transform values to log2 scale and remove invalid values
  log_raster <- log2(raster)
   log_raster[!is.finite(log_raster[])] <- NA
  
  # Set plotting limits if not provided
  if (is.null(z_limits)) { 
    z_limits <- range(log_raster[], na.rm = TRUE) 
  }
  
  # Generate symmetric color breaks around 0 (i.e., log2 ratio = 1)
  max_abs_limit <- max(abs(z_limits))
  at_values <- seq(-max_abs_limit, max_abs_limit, length.out = breaks)
  color_ramp <- colorRampPalette(color)(length(at_values))
  # Create readable labels for log scale
  pretty_indices <- pretty(at_values, n = labels)
  pretty_labels <- sapply(pretty_indices, function(x) {
    if (x >= 0) {
      as.character(round(2^x, 0))          # e.g., log2(2) = 1 -> label "2"
    } else {
      paste0("1/", as.integer(2^abs(x)))    # e.g., log2(0.5) = -1 -> label "1/2"
    }
  })
  colorkey <- list(at = at_values, labels = list(at = pretty_indices, labels = pretty_labels))
  # Generate the levelplot
  raster_plot <- levelplot(log_raster, col.regions = color_ramp, at = at_values, colorkey = colorkey, 
                           maxpixels = maxpixels, xlab = NULL, ylab = NULL, 
                           scales = list(x = list(draw = FALSE), y = list(draw = FALSE)), 
                           par.settings = list(layout.heights = list(top.padding = 0, bottom.padding = 0)),
                           margin = FALSE)
  # Add ocean outline (from global variable ocean_sp)
  raster_plot <- raster_plot + latticeExtra::layer(sp.polygons(ocean_sp, col = "black", fill = NA, lwd = 0.5))
  # Display the plot
  print(raster_plot)
}


#For overlaying two rasters
plot_land_overlay <- function(raster1, raster2, color1, color2, breaks = 99, 
                              z_limits1 = NULL, z_limits2 = NULL, labels = 6, 
                              maxpixels = ncell(raster1), alpha_top = .75) {  
  library(rasterVis)
  library(latticeExtra)
  
  # Replace 0s with NA
  raster1[raster1 == 0] <- NA
  raster2[raster2 == 0] <- NA
  
  # Log-transform and clean both rasters
  log_raster1 <- log2(raster1)
  log_raster2 <- log2(raster2)
  log_raster1[!is.finite(log_raster1[])] <- NA
  log_raster2[!is.finite(log_raster2[])] <- NA
  
  # Define limits separately for each raster
  if (is.null(z_limits1)) z_limits1 <- range(log_raster1[], na.rm = TRUE)
  if (is.null(z_limits2)) z_limits2 <- range(log_raster2[], na.rm = TRUE)
  
  at_values1 <- seq(z_limits1[1], z_limits1[2], length.out = breaks)
  at_values2 <- seq(z_limits2[1], z_limits2[2], length.out = breaks)
  
  valid_at_values1 <- at_values1[is.finite(at_values1)]
  valid_at_values2 <- at_values2[is.finite(at_values2)]
  
  # Define color scales with transparency for raster1
  color_ramp1 <- adjustcolor(colorRampPalette(color1)(length(valid_at_values1)), alpha.f = alpha_top)
  color_ramp2 <- colorRampPalette(color2)(length(valid_at_values2))  # Keep second layer fully visible
  
  # Base plot: Plot raster2 first (so it appears below)
  p1 <- levelplot(log_raster2, col.regions = color_ramp2, at = valid_at_values2, colorkey = FALSE, 
                  maxpixels = maxpixels, xlab = NULL, ylab = NULL, 
                  scales = list(x = list(draw = FALSE), y = list(draw = FALSE)), 
                  par.settings = list(layout.heights = list(top.padding = 0, bottom.padding = 0)), 
                  margin = FALSE)
  
  # Overlay raster1 with transparency
  p2 <- levelplot(log_raster1, col.regions = color_ramp1, at = valid_at_values1, colorkey = FALSE, 
                  maxpixels = maxpixels, xlab = NULL, ylab = NULL, 
                  scales = list(x = list(draw = FALSE), y = list(draw = FALSE)), 
                  par.settings = list(layout.heights = list(top.padding = 0, bottom.padding = 0)), 
                  margin = FALSE)
  
  # Combine layers (maintaining transparency for true overlap)
  combined_plot <- p1 + as.layer(p2)
  
  # Overlay ocean boundaries (assumes global ocean_sp is set)
  combined_plot <- combined_plot + latticeExtra::layer(sp.polygons(ocean_sp, col = "black", fill = NA, lwd = 0.5))
  
  print(combined_plot)
}
