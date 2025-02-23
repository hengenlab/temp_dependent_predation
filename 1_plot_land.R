library(rasterVis)
library(lattice)
library(latticeExtra)
library(RColorBrewer)
library(raster)
library(sf)
library(tidyverse)
theme_plot <- theme(panel.grid = element_blank(), 
                    aspect.ratio = .75,
                    axis.text = element_text(size = 18, color = "black"), 
                    axis.ticks.length=unit(0.2,"cm"),
                    axis.title = element_text(size = 18),
                    axis.title.y = element_text(margin = margin(r = 10)),
                    axis.title.x = element_text(margin = margin(t = 10)),
                    axis.title.x.top = element_text(margin = margin(b = 5)),
                    plot.title = element_text(size = 18, face = "plain", hjust = 10),
                    panel.border = element_rect(colour = "black", fill=NA, size=1),
                    panel.background = element_blank(),
                    strip.background = element_blank(),
                    legend.position = "none",
                    text = element_text(family = 'Helvetica'))

col2 = rev(c('#7f0000', '#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4', '#08306b'))
col_br = rev(c('#67001f','#b2182b','#d6604d','#f4a582','#fddbc7','#f7f7f7','#d1e5f0','#92c5de','#4393c3','#2166ac','#053061'))

# colors
endo_ecto_col <- rev(c("#a50026", "#f46d43", "white", "#74add1", "#313695"))#ffffbf
#endo_ecto_col <- rev(c("#a50026", "#f46d43", "#ffffbf", "#74add1", "#313695"))
br_ramp <- (colorRampPalette(endo_ecto_col)(100))
endo_ecto_col2 <- rev(c("#67000d", "#a50026", "#f46d43", "#f7f7f7", "#74add1", "#313695", "#08306b"))
endo_ecto_col3 <- rev(c("#67000d", "#b2182b", "#d6604d","#f4a582" ,"#fddbc7", "#f7f7f7","#d1e5f0", "#92c5de", "#4393c3","#2166ac", "#08306b"))
endo_ecto_col4 <- rev(c("#67000d", "#b2182b", "#d6604d","#f4a582" , "#f7f7f7","#d1e5f0",  "#4393c3","#2166ac", "#08306b"))

br_ramp <- (colorRampPalette(endo_ecto_col)(100))
br_ramp2 <- (colorRampPalette(endo_ecto_col2)(100))
br_ramp3 <- (colorRampPalette(endo_ecto_col3)(100))
br_ramp4 <- (colorRampPalette(endo_ecto_col4)(100))
br_ramp5 <- (colorRampPalette(col2)(100))


ea <- c("+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs") #global equal area projection
res_48 <- c(48250, 48250)
raster48by48  <- raster(xmn = -17367530, xmx = 17372470 , ymn = -7325770, ymx = 7342230 ) #extents found from global diversity datasets when projected to equal area
res(raster48by48) = res_48
crs(raster48by48) = ea
raster48by48

# Ensure necessary objects are defined
ocean_plot = st_read("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Richness_Distributions/Environment/ocean_simp.gpkg")
ocean_complex = st_read("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Richness_Distributions/Environment/ocean2.gpkg")
plot(ocean_plot)
adjusted_ext <- extent(-17367530, 17372470, -6500000, 7342230)

# Crop the SpatialPolygonsDataFrame to the new extent

ocean_sp = as(ocean_plot, Class = "Spatial")
ocean_sp <- crop(ocean_sp, adjusted_ext)
ocean_sp_complex = as(ocean_complex, Class = "Spatial")
clear_ocean <- list("sp.polygons", ocean_sp, fill= NA, lwd = 0.5,first = F)
clear_ocean_complex <- list("sp.polygons", ocean_sp_complex, fill= NA, lwd = 0.5,first = F)
lobstr::obj_size(clear_ocean)
#clear_ocean2 <- ms_simplify(ocean_plot, keep = 0.05) 
#clear_ocean2 =  as(clear_ocean2, Class = "Spatial")
#clear_ocean2 =  list("sp.polygons", clear_ocean2, fill= NA, lwd = 0.5,first = F)

col_blue2 <- c("white", "lightblue3")

prop_ocean <- raster::rasterize(ocean_plot,  field = 1, background = 0,raster48by48, getCover = TRUE)
ocean_mask <- prop_ocean
plot(ocean_mask)
ocean_mask[ocean_mask > 0.1] <- NA
plot(ocean_mask)
################ using raster #######################


#---------- plot_land ---------
plot_land <- function(raster, color = col_br, cuts = 99, 
                      z_limits = NULL, num_labels = 6, maxpixels = ncell(raster)) {
  raster[raster == 0] <- NA  # Replace 0 with NA
  
  # Determine z_limits from the raster if not provided
  if (is.null(z_limits)) {
    z_limits <- range(raster[], na.rm = TRUE)
  }
  
  # Generate evenly spaced color breaks
  at_values <- seq(z_limits[1], z_limits[2], length.out = cuts)
  
  # Ensure `at_values` are finite and within z_limits
  at_values <- at_values[at_values >= z_limits[1] & at_values <= z_limits[2]]
  
  # Generate reasonable legend tick marks
  pretty_indices <- pretty(z_limits, n = num_labels)
  pretty_indices <- pretty_indices[pretty_indices >= z_limits[1] & pretty_indices <= z_limits[2]]
  
  # Round labels if necessary
  pretty_labels <- round(pretty_indices, 2)
  
  # Construct the color key
  colorkey <- list(at = at_values, labels = list(at = pretty_indices, labels = pretty_labels))
  
  # Create the color ramp
  color_ramp <- colorRampPalette(color)(length(at_values) - 1)
  
  # Create the raster plot
  raster_plot <- levelplot(raster, col.regions = color_ramp, at = at_values, colorkey = colorkey, 
                           maxpixels = maxpixels, xlab = NULL, ylab = NULL, 
                           scales = list(x = list(draw = FALSE), y = list(draw = FALSE)), 
                           par.settings = list(layout.heights = list(top.padding = 0, bottom.padding = 0)),
                           margin = FALSE)
  
  # Add ocean mask as a polygon layer (if `ocean_sp` exists)
  if (exists("ocean_sp")) {
    raster_plot <- raster_plot + latticeExtra::layer(sp.polygons(ocean_sp, col = "black", fill = NA, lwd = 0.5))
  }
  
  # Print the final plot
  print(raster_plot)
}


plot_land2 <- function(raster, color = col_br, cuts = 99, 
                      z_limits = NULL, num_labels = 6, maxpixels = ncell(raster)) {
  raster[raster == 0] <- NA # Replace 0 with NA
  
  # Determine the range of the raster
  if (is.null(z_limits)) {z_limits <- range(raster[], na.rm = TRUE)} 
  at_values <- seq(z_limits[1], z_limits[2], length.out = cuts) # Generate 'at' values
  valid_at_values <- at_values[is.finite(at_values)] # Filter valid 'at' values
  
  # Ensure 1 is included in the pretty_indices
  if (!1 %in% valid_at_values) {
    valid_at_values <- sort(c(valid_at_values, 1)) # Add 1 if it's not in valid_at_values
  }
  
  pretty_indices <- pretty(valid_at_values, n = num_labels)
  pretty_labels <- round(pretty_indices, 0) 
  #if (!1 %in% pretty_indices) {
    #pretty_indices <- sort(c(pretty_indices, 1)) # Ensure 1 is in pretty_indices
    #pretty_labels <- sort(c(pretty_labels, 1)) # Ensure 1 is in pretty_labels
#  }
  
  colorkey <- list(at = valid_at_values, labels = list(at = pretty_indices, labels = pretty_labels))
  
  # Create the color ramp and colorkey
  color_ramp <- colorRampPalette(color)(length(valid_at_values))
  
  # Create the raster plot without x and y labels, adjusting margins to remove unwanted bars
  raster_plot <- levelplot(raster, col.regions = color_ramp, at = valid_at_values, colorkey = colorkey, 
                           maxpixels = maxpixels, xlab = NULL, ylab = NULL, 
                           scales = list(x = list(draw = FALSE), y = list(draw = FALSE)), 
                           par.settings = list(layout.heights = list(top.padding = 0, bottom.padding = 0)),
                           margin = FALSE)
  
  raster_plot <- raster_plot + latticeExtra::layer(sp.polygons(ocean_sp, col = "black", fill = NA, lwd = 0.5)) # Adjust `lwd` for thinner lines
  # }
  
  print(raster_plot) # Print the combined plot
}

#------------ plot_land_log ----------------------
plot_land_log <- function(raster, color = col_br, breaks = 99, z_limits = NULL, labels = 6, maxpixels = ncell(raster)) {
  raster[raster == 0] <- NA # Replace 0 with NA
  log_raster <- log2(raster) # Apply log2 transformation
  
  # Determine the range of the transformed raster
  if (is.null(z_limits)) {z_limits <- range(log_raster[], na.rm = TRUE)} 
  at_values <- seq(z_limits[1], z_limits[2], length.out = breaks) # Generate 'at' values
  valid_at_values <- at_values[is.finite(at_values)] # Filter valid 'at' values
  
  # Create the color ramp and colorkey
  color_ramp <- colorRampPalette(color)(length(valid_at_values))
  
  # Generate labels with fractions for values < 1 and keep as is for values >= 1
  pretty_indices <- pretty(valid_at_values, n = labels)
  pretty_labels <- sapply(pretty_indices, function(x) {
    if (x >= 1) {
      return(as.character(round(2^x, 0))) # Convert log scale values back to original scale
    } else {
      fraction <- as.integer(2^(abs(x)))
      if (fraction == 1) {
        return("1") # Show "1" instead of "1/1"
      } else {
        return(paste0("1/", fraction)) # Format fractions for values < 1
      }
    }
  })
  
  # Reverse the order for colorkey to ensure proper legend ordering
  pretty_indices <- rev(pretty_indices)
  pretty_labels <- rev(pretty_labels)
  valid_at_values <- rev(valid_at_values)
  
  colorkey <- list(at = valid_at_values, labels = list(at = pretty_indices, labels = pretty_labels))
  
  # Create the raster plot without x and y labels, adjusting margins to remove unwanted bars
  raster_plot <- levelplot(log_raster, col.regions = color_ramp, at = valid_at_values, colorkey = colorkey, 
                           maxpixels = maxpixels, xlab = NULL, ylab = NULL, 
                           scales = list(x = list(draw = FALSE), y = list(draw = FALSE)), 
                           par.settings = list(layout.heights = list(top.padding = 0, bottom.padding = 0)),
                           margin = FALSE)
  
  # Include the polygon layer directly in the function
  # Add the polygon layer
  ocean_sp <- clear_ocean[[2]] # Assuming clear_ocean is preloaded or defined elsewhere
  raster_plot <- raster_plot + latticeExtra::layer(sp.polygons(ocean_sp, col = "black", fill = NA, lwd = 0.5)) # Adjust `lwd` for thinner lines
  
  print(raster_plot) # Print the combined plot
}

#---------- plot_center ---------
plot_center <- function(raster, color = col_br, breaks = 99, z_limits = NULL, labels = 6, maxpixels = ncell(raster)) {
  raster[raster == 0] <- NA # Replace 0 with NA
  log_raster <- log2(raster) # Apply log2 transformation
  
  # Determine the actual range of the log-transformed raster
  if (is.null(z_limits)) {
    z_limits <- range(log_raster[], na.rm = TRUE)
  }
  
  # Calculate the midpoint of the color ramp (centered on 0)
  max_abs_limit <- max(abs(z_limits)) # Symmetrically extend range
  at_values_full <- seq(-max_abs_limit, max_abs_limit, length.out = breaks) # Full range for color ramp
  
  # Restrict the at_values to the actual data range but keep the color ramp centered on 0
  at_values <- at_values_full[at_values_full >= z_limits[1] & at_values_full <= z_limits[2]]
  
  # Create the color ramp using the predefined br_ramp
  color_ramp <- colorRampPalette(color)(length(at_values_full))
  color_ramp <- color_ramp[at_values_full >= z_limits[1] & at_values_full <= z_limits[2]]
  
  # Generate labels with fractions for values < 1 and keep as is for values >= 1
  pretty_indices <- pretty(at_values, n = labels)
  pretty_labels <- sapply(pretty_indices, function(x) {
    if (x >= 0) {
      return(as.character(round(2^x, 0))) # Convert log scale values back to original scale
    } else {
      fraction <- as.integer(2^(abs(x)))
      return(paste0("1/", fraction)) # Format fractions for values < 1
    }
  })
  
  colorkey <- list(at = at_values, labels = list(at = pretty_indices, labels = pretty_labels))
  
  # Create the raster plot without x and y labels, adjusting margins to remove unwanted bars
  raster_plot <- levelplot(log_raster, col.regions = color_ramp, at = at_values, colorkey = colorkey, 
                           maxpixels = maxpixels, xlab = NULL, ylab = NULL, 
                           scales = list(x = list(draw = FALSE), y = list(draw = FALSE)), 
                           par.settings = list(layout.heights = list(top.padding = 0, bottom.padding = 0)),
                           margin = FALSE)
  
  # Include the polygon layer directly in the function
  ocean_sp <- clear_ocean[[2]] # Assuming clear_ocean is preloaded or defined elsewhere
  raster_plot <- raster_plot + latticeExtra::layer(sp.polygons(ocean_sp, col = "black", fill = NA, lwd = 0.5)) # Adjust `lwd` for thinner lines
  
  print(raster_plot) # Print the combined plot
}

####################################################################
########################## using ggplot ###########################
plot_log_g <- function(raster, color = col_br, breaks = 99, z_limits = NULL, labels = 6, maxpixels = ncell(raster)) {
  # Replace 0 with NA and apply log2 transformation
  raster[raster == 0] <- NA 
  log_raster <- log2(raster)
  
  # Determine the range of the transformed raster
  if (is.null(z_limits)) {
    z_limits <- range(log_raster[], na.rm = TRUE)
  }
  at_values <- seq(z_limits[1], z_limits[2], length.out = breaks)
  
  # Create the color ramp
  color_ramp <- colorRampPalette(color)(length(at_values))
  
  # Generate labels with fractions for values < 1
  pretty_indices <- pretty(at_values, n = labels)
  pretty_labels <- sapply(pretty_indices, function(x) {
    if (x >= 1) {
      return(as.character(round(2^x, 0)))
    } else {
      fraction <- as.integer(2^(abs(x)))
      if (fraction == 1) {
        return("1")
      } else {
        return(paste0("1/", fraction))
      }
    }
  })
  
  # Convert raster to data frame for ggplot
  raster_df <- as.data.frame(log_raster, xy = TRUE)
  names(raster_df)[3] <- "value"
  
  # Get the plot bounds
  x_range <- range(raster_df$x, na.rm = TRUE)
  y_range <- range(raster_df$y, na.rm = TRUE)
  
   # Define frame 
  frame_data <- data.frame(
    x = c(x_range[1] - 20000, x_range[2] + 20000, x_range[2] + 20000, x_range[1] - 20000, x_range[1] - 20000),
    y = c(y_range[1] - 10000,  y_range[1] - 10000, y_range[2] + 20000, y_range[2] + 20000, y_range[1] - 10000)
  )
  
  # Plot using ggplot2 with the specified polygons and customized legend
  ggplot(raster_df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_gradientn(
      colors = color_ramp, 
      breaks = pretty_indices, 
      labels = pretty_labels, 
      limits = z_limits, 
      na.value = "transparent",
      guide = guide_colorbar(
        barwidth = 1.25, 
        barheight = 16.65,  # Updated height
        title = NULL,  # Remove the legend title ("value")
        ticks = TRUE,  # Enable ticks
        ticks.colour = "black",  # Set ticks to black
        ticks.linewidth = .25,  # Set tick thickness
        ticks.length = .5,  # Set tick thickness
        frame.colour = "black",  # Add a black border around the legend
        frame.linewidth = 0.25  # Updated thickness of the frame
      )
    ) +
    theme_void() +
    geom_polygon(data = clear_ocean[[2]], aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.18) +
    geom_polygon(data = frame_data, aes(x = x, y = y), color = "black", fill = NA, size = .5) +  # Thinner frame with size = 0.2
    coord_equal(xlim = x_range, ylim = y_range) +  # Set plot limits to match the clipped extent
    theme(
      legend.position = "right",  
      legend.justification = "left",  
      legend.box.margin = margin(0, 55, 0, 0),  # Updated margin
      legend.text = element_text(size = 10)  # Increase font size of legend labels
    )
}

#plot_log_g(vert_ras2[["endo_ecto_rich"]])

#-------------------- plot_log_left ------------------------------
plot_log_g_left <- function(raster, color = col_br, breaks = 99, z_limits = NULL, labels = 6, maxpixels = ncell(raster), leg_position = "left") {
  # Replace 0 with NA and apply log2 transformation
  raster[raster == 0] <- NA 
  log_raster <- log2(raster)
  
  # Determine the range of the transformed raster
  if (is.null(z_limits)) {
    z_limits <- range(log_raster[], na.rm = TRUE)
  }
  at_values <- seq(z_limits[1], z_limits[2], length.out = breaks)
  
  # Create the color ramp
  color_ramp <- colorRampPalette(color)(length(at_values))
  
  # Generate labels with fractions for values < 1
  pretty_indices <- pretty(at_values, n = labels)
  pretty_labels <- sapply(pretty_indices, function(x) {
    if (x >= 1) {
      return(as.character(round(2^x, 0)))
    } else {
      fraction <- as.integer(2^(abs(x)))
      if (fraction == 1) {
        return("1")
      } else {
        return(paste0("1/", fraction))
      }
    }
  })
  
  # Convert raster to data frame for ggplot
  raster_df <- as.data.frame(log_raster, xy = TRUE)
  names(raster_df)[3] <- "value"
  
  # Get the plot bounds
  x_range <- range(raster_df$x, na.rm = TRUE)
  y_range <- range(raster_df$y, na.rm = TRUE)
  
  # Define the coordinates of the rectangle frame (optional)
  #frame_data <- data.frame(
  # x = c(x_range[1], x_range[2], x_range[2], x_range[1], x_range[1]),
  #  y = c(y_range[1], y_range[1], y_range[2], y_range[2], y_range[1])
  #)
  frame_data <- data.frame(
    x = c(x_range[1] - 20000, x_range[2] + 20000, x_range[2] + 20000, x_range[1] - 20000, x_range[1] - 20000),
    y = c(y_range[1] - 10000,  y_range[1] - 10000, y_range[2] + 20000, y_range[2] + 20000, y_range[1] - 10000)
  )
  # Plot using ggplot2 with the specified polygons and customized legend
  ggplot(raster_df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_gradientn(
      colors = color_ramp, 
      breaks = pretty_indices, 
      labels = pretty_labels, 
      limits = z_limits, 
      na.value = "transparent",
      guide = guide_colorbar(
        barwidth = 1.25, 
        barheight = 16.5,  # Updated height
        title = NULL,  # Remove the legend title ("value")
        ticks = TRUE,  # Enable ticks
        label.position = "left",
        ticks.colour = "black",  # Set ticks to black
        ticks.linewidth = .25,  # Set tick thickness
        ticks.length = .5,  # Set tick thickness
        frame.colour = "black",  # Add a black border around the legend
        frame.linewidth = 0.25  # Updated thickness of the frame
      )
    ) +
    theme_void() +
    geom_polygon(data = clear_ocean[[2]], aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.18) +
    geom_polygon(data = frame_data, aes(x = x, y = y), color = "black", fill = NA, size = .5) +  # Thinner frame with size = 0.2
    coord_equal(xlim = x_range, ylim = y_range) +  # Set plot limits to match the clipped extent
    theme(
      legend.position = leg_position,  # Use the leg_position argument
      legend.justification = "left",  
      legend.box.margin = margin(0, -30, 0, 0),  # Updated margin
      legend.text = element_text(size = 12),  # Increase font size of legend labels
      plot.margin = margin(t = 5, r = 5, b = 5, l = 50)  # S
      
    )
}
#plot_log_g_left(vert_ras2[["endo_ecto_rich"]])
#-------------------- plot_center_g ------------------------------

plot_center_g <- function(raster, color = col_br, breaks = 99, z_limits = NULL, labels = 6, title = NULL, maxpixels = ncell(raster)) {
  # Replace 0 with NA and apply log2 transformation
  raster[raster == 0] <- NA 
  log_raster <- log2(raster)
  
  # Determine the actual range of the log-transformed raster
  if (is.null(z_limits)) {
    z_limits <- range(log_raster[], na.rm = TRUE)
  }
  
  # Calculate the midpoint of the color ramp (centered on 0)
  max_abs_limit <- max(abs(z_limits))  # Symmetrically extend range
  at_values_full <- seq(-max_abs_limit, max_abs_limit, length.out = breaks)  # Full range for color ramp
  
  # Restrict the at_values to the actual data range but keep the color ramp centered on 0
  at_values <- at_values_full[at_values_full >= z_limits[1] & at_values_full <= z_limits[2]]
  
  # Create the color ramp using the predefined color palette
  color_ramp <- colorRampPalette(color)(length(at_values_full))
  color_ramp <- color_ramp[at_values_full >= z_limits[1] & at_values_full <= z_limits[2]]
  
  # Generate labels with fractions for values < 1 and keep as is for values >= 1
  pretty_indices <- pretty(at_values, n = labels)
  pretty_labels <- sapply(pretty_indices, function(x) {
    if (x >= 0) {
      return(as.character(round(2^x, 0)))  # Convert log scale values back to original scale
    } else {
      fraction <- as.integer(2^(abs(x)))
      return(paste0("1/", fraction))  # Format fractions for values < 1
    }
  })
  
  # Convert raster to data frame for ggplot
  raster_df <- as.data.frame(log_raster, xy = TRUE)
  names(raster_df)[3] <- "value"
  
  # Get the plot bounds
  x_range <- range(raster_df$x, na.rm = TRUE)
  y_range <- range(raster_df$y, na.rm = TRUE)
  
  # Define the frame)
  frame_data <- data.frame(
    x = c(x_range[1] - 20000, x_range[2] + 20000, x_range[2] + 20000, x_range[1] - 20000, x_range[1] - 20000),
    y = c(y_range[1] - 10000,  y_range[1] - 10000, y_range[2] + 20000, y_range[2] + 20000, y_range[1] - 10000)
  )
  
  # Plot using ggplot2 with the specified polygons and customized legend
  ggplot(raster_df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_gradientn(
      colors = color_ramp, 
      values = scales::rescale(at_values_full),  # Center the color scale
      breaks = pretty_indices, 
      labels = pretty_labels, 
      limits = z_limits, 
      na.value = "transparent",
      guide = guide_colorbar(
        barwidth = 1.25, 
        barheight = 15.5,  # Updated height
        title = NULL,  # Remove the legend title ("value")
        ticks = TRUE,  # Enable ticks
        ticks.colour = "black",  # Set ticks to black
        ticks.linewidth = 0.35,  # Set tick thickness
        ticks.length = unit(0.65, "cm"),  # Set tick length
        frame.colour = "black",  # Add a black border around the legend
        frame.linewidth = 0.25  # Updated thickness of the frame
      )
    ) +
    theme_void() +
    geom_polygon(data = clear_ocean[[2]], aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.18) +
    geom_polygon(data = frame_data, aes(x = x, y = y), color = "black", fill = NA, size = 0.35) +  # Thinner frame with size = 0.2
    coord_equal(xlim = x_range, ylim = y_range) +  # Set plot limits to match the clipped extent
    ggtitle(title) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 18),  # Center the title
      legend.position = "right",  
      legend.justification = "left",  
      legend.box.margin = margin(0, 55, 0, 0),  # Updated margin
      legend.text = element_text(size = 14)  # Increase font size of legend labels
    )
}

plot_land_overlay <- function(raster1, raster2, color1, color2, breaks = 99, 
                              z_limits1 = NULL, z_limits2 = NULL, labels = 6, 
                              maxpixels = ncell(raster1), alpha_top = .75) {  
  library(rasterVis)
  library(latticeExtra)
  
  # Replace 0s with NA
  raster1[raster1 == 0] <- NA
  raster2[raster2 == 0] <- NA
  
  # Log-transform both rasters
  log_raster1 <- log2(raster1)
  log_raster2 <- log2(raster2)
  
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
  
  # Ensure polygon overlay is drawn last
  ocean_sp <- clear_ocean[[2]]  
  combined_plot <- combined_plot + layer(sp.polygons(ocean_sp, col = "black", fill = NA, lwd = 0.5))
  
  print(combined_plot)
}

#plot_center_g(vert_ras2[["mammal_reptile_rich"]], col = br_ramp2, title = "Current")

#------------- Plot g no log --------
########################## using ggplot ###########################
plot_g <- function(raster, color = col_br, breaks = 99, z_limits = NULL, labels = 6, maxpixels = ncell(raster)) {
  # Replace 0 with NA
  raster[raster == 0] <- NA 
  
  # Determine the range of the raster
  if (is.null(z_limits)) {
    z_limits <- range(raster[], na.rm = TRUE)
  }
  at_values <- seq(z_limits[1], z_limits[2], length.out = breaks)
  
  # Create the color ramp
  color_ramp <- colorRampPalette(color)(length(at_values))
  
  # Generate labels
  pretty_indices <- pretty(at_values, n = labels)
  pretty_labels <- round(pretty_indices, 0)
  
  # Convert raster to data frame for ggplot
  raster_df <- as.data.frame(raster, xy = TRUE)
  names(raster_df)[3] <- "value"
  
  # Get the plot bounds
  x_range <- range(raster_df$x, na.rm = TRUE)
  y_range <- range(raster_df$y, na.rm = TRUE)
  
  # Define frame 
  frame_data <- data.frame(
    x = c(x_range[1] - 20000, x_range[2] + 20000, x_range[2] + 20000, x_range[1] - 20000, x_range[1] - 20000),
    y = c(y_range[1] - 10000,  y_range[1] - 10000, y_range[2] + 20000, y_range[2] + 20000, y_range[1] - 10000)
  )
  
  # Plot using ggplot2 with the specified polygons and customized legend
  ggplot(raster_df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_gradientn(
      colors = color_ramp, 
      breaks = pretty_indices, 
      labels = pretty_labels, 
      limits = z_limits, 
      na.value = "transparent",
      guide = guide_colorbar(
        barwidth = 1.25, 
        barheight = 16.65,  # Updated height
        title = NULL,  # Remove the legend title ("value")
        ticks = TRUE,  # Enable ticks
        ticks.colour = "black",  # Set ticks to black
        ticks.linewidth = .35,  # Set tick thickness
        ticks.length = .5,  # Set tick thickness
        frame.colour = "black",  # Add a black border around the legend
        frame.linewidth = 0.25  # Updated thickness of the frame
      )
    ) +
    theme_void() +
    geom_polygon(data = clear_ocean[[2]], aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.18) +
    geom_polygon(data = frame_data, aes(x = x, y = y), color = "black", fill = NA, size = .5) +  # Thinner frame with size = 0.2
    coord_equal(xlim = x_range, ylim = y_range) +  # Set plot limits to match the clipped extent
    theme(
      legend.position = "right",  
      legend.justification = "left",  
      legend.box.margin = margin(0, 55, 0, 0),  # Updated margin
      legend.text = element_text(size = 16)  # Increase font size of legend labels
    )
}
#-------------plot_g_left 


plot_g_left <- function(raster, color = col_br, breaks = 99, z_limits = NULL, labels = 6, maxpixels = ncell(raster), leg_position = "left") {
  # Replace 0 with NA
  raster[raster == 0] <- NA 
  
  # Determine the range of the raster
  if (is.null(z_limits)) {
    z_limits <- range(raster[], na.rm = TRUE)
  }
  at_values <- seq(z_limits[1], z_limits[2], length.out = breaks)
  
  # Create the color ramp
  color_ramp <- colorRampPalette(color)(length(at_values))
  
  # Generate labels with fractions for values < 1
  pretty_indices <- pretty(at_values, n = labels)
  pretty_labels <- round(pretty_indices, 0)
  
  # Convert raster to data frame for ggplot
  raster_df <- as.data.frame(raster, xy = TRUE)
  names(raster_df)[3] <- "value"
  
  # Get the plot bounds
  x_range <- range(raster_df$x, na.rm = TRUE)
  y_range <- range(raster_df$y, na.rm = TRUE)
  
  # Define the coordinates of the rectangle frame (optional)
  frame_data <- data.frame(
    x = c(x_range[1] - 20000, x_range[2] + 20000, x_range[2] + 20000, x_range[1] - 20000, x_range[1] - 20000),
    y = c(y_range[1] - 10000,  y_range[1] - 10000, y_range[2] + 20000, y_range[2] + 20000, y_range[1] - 10000)
  )
  
  # Plot using ggplot2 with the specified polygons and customized legend
  ggplot(raster_df, aes(x = x, y = y, fill = value)) +
    geom_raster() +
    scale_fill_gradientn(
      colors = color_ramp, 
      breaks = pretty_indices, 
      labels = pretty_labels, 
      limits = z_limits, 
      na.value = "transparent",
      guide = guide_colorbar(
        barwidth = 1.25, 
        barheight = 16.5,  # Updated height
        title = NULL,  # Remove the legend title ("value")
        ticks = TRUE,  # Enable ticks
        label.position = "left",
        ticks.colour = "black",  # Set ticks to black
        ticks.linewidth = .25,  # Set tick thickness
        ticks.length = .5,  # Set tick thickness
        frame.colour = "black",  # Add a black border around the legend
        frame.linewidth = 0.25  # Updated thickness of the frame
      )
    ) +
    theme_void() +
    geom_polygon(data = clear_ocean[[2]], aes(x = long, y = lat, group = group), color = "black", fill = NA, size = 0.18) +
    geom_polygon(data = frame_data, aes(x = x, y = y), color = "black", fill = NA, size = .5) +  # Thinner frame with size = 0.2
    coord_equal(xlim = x_range, ylim = y_range) +  # Set plot limits to match the clipped extent
    theme(
      legend.position = leg_position,  # Use the leg_position argument
      legend.justification = "left",  
      legend.box.margin = margin(0, -30, 0, 0),  # Updated margin
      legend.text = element_text(size = 16),  # Increase font size of legend labels
      plot.margin = margin(t = 5, r = 5, b = 5, l = 50)  # S
      
    )
}
