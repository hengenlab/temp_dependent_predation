# Global Plots - Figures 1, 2, 6

# Load libraries
library(raster)
library(svglite)
library(tidyverse)
library(RColorBrewer)
library(egg)
library(grid)
library(colorspace)

# Load functions
source("/Users/jgradym/Documents/GitHub/mouse_capture/plot_land.R")

# Load Data
vert_df  = read_csv("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Mouse/Richness/vert_df.csv")
vert_df[] <- lapply(vert_df, function(x) ifelse(is.infinite(x) | x == 0, NA, x))
vert_ras = brick("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Mouse/Richness/vert_ras.grd")
names(vert_ras)


# Truncate empty space near antartica lacking terrestrial ectotherms
adjusted_ext <- extent(-17367530, 17372470, -6500000, 7342230)
vert_ras2 <- crop(vert_ras, adjusted_ext)
names(vert_ras2)

#---------- Aesthetics
col = c("#053061" ,"#2166ac", "#4393c3", "#92c5de" ,"#d1e5f0", "#f7f7f7", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
col_dark = darken(col, 0.5)
color_limits <- range(vert_df$endo_ecto_rich, na.rm = TRUE)
label <- expression(frac("Endotherm Richness", "Ectotherm Richness"))


# Define the fill gradient scale
scale_fill <- scale_fill_gradientn(
  colours = col,  # Your existing color palette for fill
  trans = 'log',
  limits = color_limits,
  guide = "colourbar"
)

# Define the color gradient scale
scale_color <- scale_color_gradientn(
  colours = col_dark,  # Your existing dark color palette for stroke
  trans = 'log',
  limits = color_limits,
  guide = "colourbar"
)


theme_plot <- theme(panel.grid = element_blank(), 
                    aspect.ratio = .8,
                    axis.text = element_text(size = 15, color = "black"), 
                    axis.ticks.length=unit(0.2,"cm"),
                    axis.title = element_text(size = 15),
                    axis.title.y.right = element_text(margin = margin(r = 0)),
                    axis.title.y.left = element_text(margin = margin(l = 0)),
                    axis.title.x = element_text(margin = margin(t = 0)),
                    axis.title.x.top = element_text(margin = margin(b = 0)),
                    plot.title = element_text(size = 15, hjust = 10),
                    panel.border = element_rect(colour = "black", fill=NA, linewidth=1),
                    panel.grid.major = element_blank(), 
                    panel.grid.minor = element_blank(),
                    panel.background = element_rect(fill = "transparent",colour = NA),
                    plot.background = element_rect(fill = "transparent",colour = NA),
                    legend.background = element_rect(color = NA),
                    legend.key=element_blank(), 
                    text = element_text(family = 'Helvetica')) 



######################################################
# ------------------  Figure 1 -----------------------
######################################################

#---------- Fig 1A 
plot_land_log(vert_ras2[["endo_ecto_rich"]], col = col)

# Save
svglite("~/Downloads/Fig1A*.svg")
plot_land_log(vert_ras2[["endo_ecto_rich"]], col = col)
dev.off()

#---------Figure 1B Latitude 

# Use Lat Lon coordinates
vert_wgs0 <- st_as_sf(vert_df, coords = c("x", "y"), crs = ea)
vert_wgs <- st_transform(vert_wgs0, crs = "+init=epsg:4326")
vert_wgs$lon <- st_coordinates(vert_wgs)[, 1]  # Longitude
vert_wgs$lat <- st_coordinates(vert_wgs)[, 2]  # Latitude


p <- ggplot(vert_wgs, aes(x = lat, y = endo_ecto_rich, color = endo_ecto_rich, fill = endo_ecto_rich)) +
  geom_point(shape = 21, size = 1.5, stroke = 0.1, show.legend = FALSE) +
  scale_fill +
  scale_color +
  theme_plot +  # Your predefined theme
  stat_smooth(method = "lm", color = "black", formula = y ~ I(x^2), linewidth = 1) + # Quadratic fit
  scale_y_continuous(
    name = label, 
    trans = 'log10', 
    breaks = c(1, 10, 100), 
    labels = c("1", "10", "100"), 
    position = "left") +
  scale_x_continuous(
    name = "Latitude (º)", 
    limits = c(-60, 75), 
    breaks = c(-60, -30, 0, 30, 60)) +
  theme(legend.position = "none")

print(p)

plot <-set_panel_size(p, width=unit(11.8,"cm"), height=unit(8.5,"cm"))
pdf("~/Downloads/elevation.pdf")#, width = 11.8/2.54, height = 8.5/2.54)  # Convert cm to inches
grid.newpage()
grid.draw(plot)
dev.off()

#-----------Figure 1C: Elevation

# Define the function
lat_to_meters <- function(lat_degrees){
  # Create a point with the input latitude and longitude 0
  points <- st_sfc(st_point(c(0, lat_degrees)), crs = 4326)  # WGS84
  # Reproject the point to the Cylindrical Equal Area (CEA) projection
  proj_points <- st_transform(points, crs = "+proj=cea +lat_ts=30 +lon_0=0 +datum=WGS84 +units=m")
  # Extract the Y-coordinate (in meters)
  projected_coordinates <- st_coordinates(proj_points)
  return(projected_coordinates[2])  # Return only the Y-coordinate
}

# Define subtropics
subtropics_north = lat_to_meters(30)    # 3658789 m, For 30 degrees north
subtropics_south = lat_to_meters(-30)   # -3658789 m,  30 degrees south


# Fig 1C
p <- ggplot(vert_df %>% filter(!is.na(endo_ecto_rich), y > subtropics_south, y < subtropics_north),
            aes(x = elevation, y = endo_ecto_rich, fill = endo_ecto_rich, color = endo_ecto_rich)) +
  geom_point(shape = 21, size = 1.5, stroke = 0.2, show.legend = FALSE) +
  scale_color + scale_fill +
  geom_smooth(method = 'lm', color = "black", show.legend = FALSE) +
  scale_y_continuous(name = label,
                     position = "right",
                     trans = 'log10', 
                     limits = c(0.3, 100), 
                     breaks = c(0.1, 1, 10, 100), 
                     labels = c("0.1", "1", "10", "100")) +
  scale_x_continuous(name = "Elevation (m)") +
  theme_plot  # Your predefined theme

print(p)

lm1 = lm(log(endo_ecto_rich) ~elevation, data = (vert_df %>% filter(y > -3658789, y < 3658789)))
summary(lm1)

plot <-set_panel_size(p, width=unit(11.8,"cm"), height=unit(8.5,"cm"))
pdf("~/Downloads/elevation.pdf")#, width = 11.8/2.54, height = 8.5/2.54)  # Convert cm to inches
grid.newpage()
grid.draw(plot)
dev.off()


###############################################################
# ---------------       Figure 2       ------------------------
###############################################################

#-----------Figure 2A: Diversity across clades
# Diversity figure with tempo
(p = ggplot(data = vert_df, aes(x = temp))+
    theme_plot + 
    coord_cartesian() +
    geom_point(aes(y = reptile_rich), color = "blue1", shape = 21,  size = 0.25, stroke = 0.1) +
    geom_point(aes(y = mammal_rich), color = "firebrick2", shape = 21,  size = 0.25, stroke = 0.1) +
    geom_point(aes(y = amph_rich), color = "lightblue1", shape = 21,  size = 0.25, stroke = 0.1) + 
    geom_point(aes(y = bird_rich), color = "pink", shape = 21,  size = 0.25, stroke = 0.1) +
    stat_smooth(aes(y = reptile_rich), color = "blue4", se = F) + 
    stat_smooth(aes(y = mammal_rich), color = "firebrick4", se = F) + 
    stat_smooth(aes(y = bird_rich), color = "pink3", se = F) + 
    stat_smooth(aes(y = amph_rich), color = "lightblue3", se = F) + 
    # scale_y_log10(name = "richness", limits = c(1, 600), breaks = c(1, 3, 10, 30, 100, 300)) + 
    scale_y_log10(name = "Richness", limits = c(1, 900), breaks = c(1,  10, 100, 1000)) + 
    scale_x_continuous(limits = c(-25, 30), name = "Mean Annual Temperature (ºC)"))

plot <-set_panel_size(p, width = unit(11.8,"cm"), height = unit(8.5,"cm"))
pdf("~/Downloads/Fig2A.pdf")#, width = 11.8/2.54, height = 8.5/2.54)  # Convert cm to inches
grid.newpage()
grid.draw(plot)
dev.off()

#--------- Figure 2B: Endo/Ecto rich vs temp

p <- ggplot(vert_df %>% filter(!is.na(one_kT) & !is.na(endo_ecto_rich)),
            aes(x = temp, y = endo_ecto_rich, color = endo_ecto_rich, fill = endo_ecto_rich)) +

  geom_point(shape = 21, size = 1.5, stroke = 0.15, show.legend = FALSE) +
  scale_fill +
  scale_color +
  theme_plot +  
  geom_smooth(method = 'lm', color = "black", show.legend = FALSE) +
  scale_y_continuous(name = label,
                     trans = 'log10', 
                     limits = c(0.4, 200), 
                     breaks = c(0.1, 1, 10, 100), 
                     position = "right",
                     labels = c("0.1", "1", "10", "100")) +
  scale_x_continuous(
    name = "Mean Annual Temperature (ºC)",
    sec.axis = sec_axis(~ 1 / ((. + 273.15) * 0.00008617), breaks = c(45, 43, 41, 39), name = expression("1/kT")))

print(p)

plot <-set_panel_size(p, width = unit(11.8,"cm"), height = unit(8.5,"cm"))
pdf("~/Downloads/temperature.pdf")#, width = 11.8/2.54, height = 8.5/2.54)  # Convert cm to inches
grid.newpage()
grid.draw(plot)
dev.off()


##############################################
# ------------ Figure 6 ---------------------
##############################################

# Note: Plots have color scheme tied to absolute values, so stable across panels to facilitate comparison
# Fig 6A
(p1 = plot_center_g(vert_ras2[["mammal_reptile_rich"]], col = br_ramp2)) 
# Fig 6B
(p2 = plot_center_g(vert_ras2[["mammal_reptile_1_2.6_ratio"]], col = br_ramp2)) 
# Fig 6C
(p3 = plot_center_g(vert_ras2[["mammal_reptile_2_4.5_ratio"]], col = br_ramp2)) 
# Fig 6D
(p4 = plot_center_g(vert_ras2[["mammal_reptile_5_8.5_ratio"]], col = br_ramp2)) 

# Save Panels
(p1 = plot_center_g(vert_ras2[["mammal_reptile_rich"]], col = br_ramp2, title = "Current"))
svglite("~/Downloads/Fig6A.svg")
p1
dev.off()

(p2 = plot_center_g(vert_ras2[["mammal_reptile_1_2.6_ratio"]], col = br_ramp2, title = "+1.4 ºC"))
svglite("~/Downloads/Fig6B.svg")
p2
dev.off()

(p3 = plot_center_g(vert_ras2[["mammal_reptile_2_4.5_ratio"]], col = br_ramp2, title = "+2.6 ºC"))
svglite("~/Downloads/Fig6C.svg")
p3
dev.off()

(p4 = plot_center_g(vert_ras2[["mammal_reptile_5_8.5_ratio"]], col = br_ramp2, title = "+5.4 ºC"))
svglite("~/Downloads/Fig6D.svg")
p4
dev.off()




