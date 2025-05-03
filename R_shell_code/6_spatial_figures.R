#-------------------------- Spatial Plots - Figures 1, 2, 6 ---------------

# ------------------- Load Required Libraries -------------------
library(raster)         # For raster data handling
library(svglite)        # For saving SVG vector graphics
library(tidyverse)      # For dplyr, ggplot2, readr, etc.
library(RColorBrewer)   # For color palettes
library(egg)            # For set_panel_size and multi-panel plotting
library(grid)           # For grid graphics
library(colorspace)     # For color manipulation
library(readxl)         # Read in supplemental Data 1
# ------------------- Load Paths and Custom Functions ---------------------
# Update paths
path <- "/Users/jgradym/Desktop/Predation_Data"
github_path = "/Users/jgradym/Documents/GitHub/mouse_capture"

source(file.path(github_path, "3_spatial_functions.R"))

# ------------------- Load Data ---------------------------------
vert_df  = read_csv(file.path(path, "Spatial_data/vert_df.csv"))

# Convert infinite and zero values to NA
vert_df[] <- lapply(vert_df, function(x) ifelse(is.infinite(x) | x == 0, NA, x)) 
vert_ras = brick(file.path(path, "Spatial_data/vert_ras.grd"))
plot_land_log(vert_ras[["endo_ecto_rich"]])



# ------------------- Aesthetic Definitions ---------------------
col_dark = darken(col_br, 0.5)
color_limits <- range(vert_df$endo_ecto_rich, na.rm = TRUE)
label <- expression(frac("Endotherm Richness", "Ectotherm Richness"))


# Define the fill gradient scale
scale_fill <- scale_fill_gradientn(
  colours = col_br,  # Your existing color palette for fill
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


######################################################
# ------------------  Figure 1 -----------------------
######################################################

# --------- Figure 1A: Global richness map (SVG export) ---------
plot_land_log(vert_ras[["endo_ecto_rich"]])

# Save
#svglite("~/Downloads/Fig1A*.svg")
#plot_land_log(vert_ras2[["endo_ecto_rich"]])
#dev.off()

# --------- Figure 1B: Richness vs Latitude ---------------------

# Project coordinates from raster to geographic lat/lon for plotting
vert_wgs0 <- st_as_sf(vert_df, coords = c("x", "y"), crs = ceaproj)
vert_wgs <- st_transform(vert_wgs0, crs = "EPSG:4326")
vert_wgs$lon <- st_coordinates(vert_wgs)[, 1]  # Longitude
vert_wgs$lat <- st_coordinates(vert_wgs)[, 2]  # Latitude

# Scatterplot of richness ratio vs latitude with quadratic fit
p <- ggplot(vert_wgs %>% filter(is.finite(endo_ecto_rich)), 
            aes(x = lat, y = endo_ecto_rich, color = endo_ecto_rich, fill = endo_ecto_rich)) +
  geom_point(shape = 21, size = 1.5, stroke = 0.1, show.legend = FALSE) +
  scale_fill +scale_color +theme_plot +  
  stat_smooth(method = "lm", color = "black", formula = y ~ I(x^2), linewidth = 1) + # Quadratic fit
  scale_y_continuous(
    name = label, trans = 'log10', breaks = c(1, 10, 100), labels = c("1", "10", "100"), position = "left") +
  scale_x_continuous(
    name = "Latitude (º)", limits = c(-60, 75), breaks = c(-60, -30, 0, 30, 60)) +
  theme(legend.position = "none")

print(p)

# Set consistent figure size (for multi-panel layouts)

#plot <-set_panel_size(p, width=unit(11.8,"cm"), height=unit(8.5,"cm"))
#pdf("~/Downloads/elevation.pdf")#, width = 11.8/2.54, height = 8.5/2.54)  # Convert cm to inches
#grid.newpage()
#grid.draw(plot)
#dev.off()

# --------- Figure 1C: Richness vs Elevation (in tropics/subtropics only) ---------


# Convert latitude to meters
lat_to_meters <- function(lat_degrees){
  # Create a point with the input latitude and longitude 0
  points <- st_sfc(st_point(c(0, lat_degrees)), crs = 4326)  # WGS84
  # Reproject the point to the Cylindrical Equal Area (CEA) projection
  proj_points <- st_transform(points, crs = ceaproj)
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

# Linear model for regresssion against elevation
lm1 = lm(log(endo_ecto_rich) ~elevation, data = (vert_df %>% filter(y > -3658789, y < 3658789)))
summary(lm1)

#plot <-set_panel_size(p, width=unit(11.8,"cm"), height=unit(8.5,"cm"))
#pdf("~/Downloads/elevation.pdf")#, width = 11.8/2.54, height = 8.5/2.54)  # Convert cm to inches
#grid.newpage()
#grid.draw(plot)
#dev.off()


###############################################################
# ---------------       Figure 2       ------------------------
###############################################################

# --------- Figure 2A: Richness for all clades vs temperature -----------
p <- ggplot(data = vert_df, aes(x = temp)) +
  theme_plot + 
  coord_cartesian() +
  geom_point(aes(y = reptiles_richness), color = "blue1", shape = 21, size = 0.25, stroke = 0.1) +
  geom_point(aes(y = mammals_richness), color = "firebrick2", shape = 21, size = 0.25, stroke = 0.1) +
  geom_point(aes(y = amphibian_rich), color = "lightblue1", shape = 21, size = 0.25, stroke = 0.1) +
  geom_point(aes(y = birds_richness), color = "pink", shape = 21, size = 0.25, stroke = 0.1) +
  stat_smooth(aes(y = reptiles_richness), color = "blue4", se = FALSE) + 
  stat_smooth(aes(y = mammals_richness), color = "firebrick4", se = FALSE) + 
  stat_smooth(aes(y = birds_richness), color = "pink3", se = FALSE) + 
  stat_smooth(aes(y = amphibian_rich), color = "lightblue3", se = FALSE) +
  scale_y_log10(name = "Richness", limits = c(1, 900), breaks = c(1, 10, 100, 1000)) + 
  scale_x_continuous(limits = c(-25, 30), name = "Mean Annual Temperature (ºC)")

plot <- set_panel_size(p, width = unit(11.8, "cm"), height = unit(8.5, "cm"))
# pdf("~/Downloads/Fig2A.pdf", width = 11.8/2.54, height = 8.5/2.54)  # Convert cm to inches
grid.newpage()
grid.draw(plot)

# --------- Figure 2B: Endo/Ecto Richness Ratio vs. Temperature ---------
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

#plot <-set_panel_size(p, width = unit(11.8,"cm"), height = unit(8.5,"cm"))
#pdf("~/Downloads/temperature.pdf")#, width = 11.8/2.54, height = 8.5/2.54)  # Convert cm to inches
#grid.newpage()
#grid.draw(plot)
#dev.off()


###################################################################
# ------------ Figure 6: Projected Diversity --------
################################################################

# Note: Plots have color scheme tied to absolute values, where white is equal (endo/ecto = 1), so stable across panels to facilitate comparison
# Fig 6A
(p1 = plot_center(vert_ras[["mammal_reptile_rich"]], col = col_br2) )
# Fig 6B
(p2 = plot_center(vert_ras[["mammal_reptile_1_2.6_ratio"]], col = col_br2)) 
# Fig 6C
(p3 = plot_center(vert_ras[["mammal_reptile_2_4.5_ratio"]], col = col_br2)) 
# Fig 6D
(p4 = plot_center(vert_ras[["mammal_reptile_5_8.5_ratio"]], col = col_br2)) 

# Save Panels

#(p1 = plot_center(vert_ras[["mammal_reptile_rich"]], col = col_br2, title = "Current"))
#svglite("~/Downloads/Fig6A.svg")
#p1
#dev.off()

#(p2 = plot_center(vert_ras[["mammal_reptile_1_2.6_ratio"]], col = col_br2, title = "+1.4 ºC"))
#svglite("~/Downloads/Fig6B.svg")
#p2
#dev.off()

#(p3 = plot_center(vert_ras[["mammal_reptile_2_4.5_ratio"]], col = col_br2, title = "+2.6 ºC"))
#svglite("~/Downloads/Fig6C.svg")
#p3
#dev.off()

#(p4 = plot_center(vert_ras[["mammal_reptile_5_8.5_ratio"]], col = col_br2, title = "+5.4 ºC"))
#svglite("~/Downloads/Fig6D.svg")
#p4
#dev.off()



###########################################
#---------- Supplementals ----------------
###########################################


#------------ Ectotherm Richness ----------
names(vert_ras)

# ----- Amphibians
plot_land(vert_ras[["amphibian_rich"]])

#---Frogs
plot_land(vert_ras[["anura_richness"]])

#--- Salamanders
plot_land(vert_ras[["caudata_richness"]])

#--- Caecilians
plot_land(vert_ras[["gymnophiona_richness"]])

#--- Reptiles
plot_land(vert_ras[["reptiles_richness"]])

#--- Caecilians
plot_land(vert_ras[["gymnophiona_richness"]])

#--- Squamates (lizards and snakes)
plot_land(vert_ras[["squamates_richness"]])

#--- Turtles
plot_land(vert_ras[["turtles_richness"]])

#--- Crocodiles
plot_land(vert_ras[["crocodile_richness"]])

#----------Endotherms ------

#--- Birds
plot_land(vert_ras[["birds_richness"]])

#--- Mammals
plot_land(vert_ras[["mammals_richness"]])


##############################
#------ For Fitted Model figures 
#---see autocorrelation_taxa.R
###############################

##############################
#-------Shrew Richness
###############################
# Panel B # Shrew metabololic Rate

red9 <- brewer.pal(9, "Reds")
purple9 <- brewer.pal(9, "Purples")

# Pamel A
# Plot Spatial Richness
plot_land_log(vert_ras[["crocidurinae_richness"]], color = purple9, num_labels = 4)
plot_land_log(vert_ras[["soricinae_richness"]], color = red9, num_labels = 4) 

# Overlay
plot_land_overlay(vert_ras[["soricinae_richness"]], vert_ras[["crocidurinae_richness"]], red9, purple9, alpha = 1)

label <- bquote(frac("Soricinae Richness", "Crocidurinae Richness"))

# Panel C
# Plot Ratio Richness ~ Temperature
p <- ggplot(vert_df %>% filter(!is.na(one_kT) & !is.na(soric_crocid_rich)),
            aes(x = temp, y = soric_crocid_rich)) +
  geom_point(shape = 21, size = 1.5, stroke = 0.25, show.legend = FALSE) +
  theme_plot +  
  geom_smooth(method = 'lm', color = "black", show.legend = FALSE) +
  scale_y_continuous(name = label,
                     trans = 'log10', 
                     breaks = c(0.1, 1, 10), 
                     limits = c(0.1, 20),
                     position = "right",
                     labels = c("0.1", "1", "10")) +
  scale_x_continuous(
    name = "Mean Annual Temperature (ºC)",
    sec.axis = sec_axis(~ 1 / ((. + 273.15) * 0.00008617), breaks = c(45, 43, 41, 39), name = expression("1/kT")))

print(p)



#--------- Shrew Richness -----------------
shrew_path <- file.path(path, "Data 1.xlsx")
sheet_name <- "3_Shrew_MetRate"
shrew_mr0 <- read_excel(shrew_path, sheet = sheet_name)

# Rename
shrew_mr <- shrew_mr0 %>%
  dplyr::select(Binomial, Mass_g, Met_Rate_W, Subfamily) %>%
  rename(binomial = Binomial, met_mass = Mass_g, met_rate = Met_Rate_W, subfamily = Subfamily) 

# Metabolic Scaling Fit
lm_soric = lm(log(met_rate) ~ log(met_mass), data = (shrew_mr %>% filter(subfamily == "Soricinae")))
lm_crocid = lm(log(met_rate) ~ log(met_mass), data = (shrew_mr %>% filter(subfamily == "Crocidurinae")))
summary(lm_soric)
summary(lm_crocid)

# Number of Species
unique(shrew_mr$binomial[shrew_mr$subfamily == "Soricinae"])
unique(shrew_mr$binomial[shrew_mr$subfamily == "Crocidurinae"])

(p <- ggplot(shrew_mr %>% filter(subfamily != "Myosoricinae"), 
             aes(x = met_mass, y = met_rate, color = subfamily, fill = subfamily)) +
    geom_smooth(method = "lm", alpha = 0.2, aes(color = subfamily)) +
    geom_point(shape = 21, size = 3.5, color = "black", stroke = .5) +
    theme_plot +
    scale_fill_manual(values = c("Crocidurinae" = "#bcbddc", "Soricinae" = "#fcaeb1", "Myosoricinae" = "#9ecae1"))+
    scale_color_manual(values = c("Crocidurinae" = "#3f007d", "Soricinae" = "#a50f15", "Myosoricinae" = "#9ecae1"))+
    scale_y_log10(name = expression("Metabolic Rate (W)"), breaks=c(0.05,0.1, 0.2,0.4))+
    scale_x_log10(name = expression("Mass (g)")))



