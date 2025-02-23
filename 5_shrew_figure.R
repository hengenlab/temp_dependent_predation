library(tidyverse)
library(broom)
library(RColorBrewer)
library(rasterVis)
library(scales)
library(raster)
library(sf)
library(tidyverse)


source("/Users/jgradym/Documents/GitHub/mouse_capture/plot_land.R")

red9 <- c("#fff5f0", "#fee0d2", "#fcbba1", "#fc9272", "#fb6a4a", "#ef3b2c", "#cb181d", "#a50f15", "#67000d")
red9 <- brewer.pal(9, "Reds")

purple9 <- brewer.pal(9, "Purples")

#--------- Shrew Richness -----------------
shrew_mr0 <- read_csv('/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Science/Shrews/data/met_rates/Shrew_metabolic_rates.csv')
shrew_mr0  
length(unique(shrew_mr0$Binomial))
shrew_mr <- shrew_mr0 %>%
  dplyr::select(Binomial, Mass_g, Met_Rate_W, Subfamily) %>%
  rename(binomial = Binomial, met_mass = Mass_g, met_rate = Met_Rate_W, subfamily = Subfamily) 

lm_soric = lm(log(met_rate) ~ log(met_mass), data = (shrew_mr %>% filter(subfamily == "Soricinae")))
lm_crocid = lm(log(met_rate) ~ log(met_mass), data = (shrew_mr %>% filter(subfamily == "Crocidurinae")))
summary(lm_soric)
summary(lm_crocid)

unique(shrew_mr$binomial[shrew_mr$subfamily == "Soricinae"])
unique(shrew_mr$binomial[shrew_mr$subfamily == "Crocidurinae"])

# Plot shrew metabolic scaling
(p <- ggplot(shrew_mr, 
             aes(x = met_mass, y = met_rate, color = subfamily, fill = subfamily)) +
    geom_smooth(method = "lm", alpha = 0.2, aes(color = subfamily)) +
    geom_point(shape = 21, size = 3.5, color = "black", stroke = .5) +
    theme_pred +
    scale_fill_manual(values = c("Crocidurinae" = "#bcbddc", "Soricinae" = "#fcaeb1", "Myosoricinae" = "#9ecae1"))+
    scale_color_manual(values = c("Crocidurinae" = "#3f007d", "Soricinae" = "#a50f15", "Myosoricinae" = "#9ecae1"))+
    scale_y_log10(name = expression("Metabolic Rate (W)"), breaks=c(0.05,0.1, 0.2,0.4))+
    geom_text(aes(label= binomial, color = subfamily), hjust=0, vjust=0, size =3) +
    scale_x_log10(name = expression("Mass (g)")))

(p <- ggplot(shrew_mr %>% filter(subfamily != "Myosoricinae"), 
             aes(x = met_mass, y = met_rate, color = subfamily, fill = subfamily)) +
    geom_smooth(method = "lm", alpha = 0.2, aes(color = subfamily)) +
    geom_point(shape = 21, size = 3.5, color = "black", stroke = .5) +
    theme_pred +
    scale_fill_manual(values = c("Crocidurinae" = "#bcbddc", "Soricinae" = "#fcaeb1", "Myosoricinae" = "#9ecae1"))+
    scale_color_manual(values = c("Crocidurinae" = "#3f007d", "Soricinae" = "#a50f15", "Myosoricinae" = "#9ecae1"))+
    scale_y_log10(name = expression("Metabolic Rate (W)"), breaks=c(0.05,0.1, 0.2,0.4))+
    scale_x_log10(name = expression("Mass (g)")))

pdf("~/Downloads/shrew_mr.pdf")
p
dev.off()
#------- Shrew Richness ------
shrew_rich = raster("/Users/jgradym/Desktop/Richness_Distributions/richnessrasters/shrews_richness.tif")

crocid_rich = raster("/Users/jgradym/Desktop/Richness_Distributions/richnessrasters/crocidurinae_richness.tif")
soric_rich = raster("/Users/jgradym/Desktop/Richness_Distributions/richnessrasters/soricinae_richness.tif")
crocid_rich = mask(crocid_rich, ocean_sp, inverse = T)
soric_rich = mask(soric_rich, ocean_sp, inverse = T)

#Plot
plot_log_g(crocid_rich, color = purple9)
plot_land_log(soric_rich, color = red9, labels = 4) 
plot_land_overlay(soric_rich, crocid_rich, red9, purple9, alpha = 1)

pdf("~/Downloads/shrew_overlap.pdf")
plot_land_overlay(soric_rich, crocid_rich, red9, purple9, alpha_top = 1)
dev.off()

pdf("~/Downloads/soric_overlap.pdf")
plot_land_log(soric_rich,red9, labels =4)
dev.off()

pdf("~/Downloads/crocid_overlap.pdf")
plot_land_log(crocid_rich, purple9, labels =4)
dev.off()

# Remove 0's
crocid_rich_n0 =crocid_rich
crocid_rich_n0$crocidurinae_richness[crocid_rich_n0$crocidurinae_richness == 0] = NA

soric_rich_n0 =soric_rich
soric_rich_n0$soricinae_richness[soric_rich_n0$soricinae_richness == 0] = NA

#Ratio
soric_crocid_rich = soric_rich_n0/crocid_rich_n0

plot_land_log(soric_crocid_rich)

#------- Shrew Ratio by Temp ------
vert_ras = brick("~/Downloads/vert_ras.grd")

vert_ras = brick("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Mouse/Richness/vert_ras.grd")
vert_ras = addLayer(vert_ras, soric_crocid_rich)
vert_ras = addLayer(vert_ras, log(vert_ras[["npp"]]))
vert_ras = addLayer(vert_ras, log(vert_ras[["endo_ecto_rich"]]))

names(vert_ras[[54]]) = "soric_crocid_rich"
vert_ras = mask(vert_ras, ocean_sp, inverse = T)
names(vert_ras[[55]]) = "log_npp"
names(vert_ras[[56]]) = "log_endo_ecto_rich"
plot_g(vert_ras[[]])
names(vert_ras)
writeRaster(vert_ras, "/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Mouse/Richness/vert_ras.grd", overwrite = T)


names(vert_ras)
plot_log_g(vert_ras[["crocid_rich"]], color = purple9, labels = 5)
plot_land_log(vert_ras[["crocid_rich"]], color = purple9, labels = 5)
cellStats()

soric_crocid_rich = crop(soric_crocid_rich, vert_ras)
vert_ras = addLayer(vert_ras, soric_crocid_rich)
str(vert_ras)
names(vert_ras[[54]]) = "soric_crocid_rich"

vert_df = as.data.frame(vert_ras, xy = T)

#---------- Aesthetics
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

col = c("#053061" ,"#2166ac", "#4393c3", "#92c5de" ,"#d1e5f0", "#f7f7f7", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f")
col_dark = darken(col, 0.5)
color_limits <- range(vert_df$soric__rich, na.rm = TRUE)


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


label <- expression(frac("Soricinae Richness", "Crocidurinae Richness"))

p <- ggplot(vert_df %>% filter(!is.na(one_kT) & !is.na(soric_crocid_rich)),
            aes(x = temp, y = soric_crocid_rich)) +#, color = soric_crocid_rich, fill = soric_crocid_rich)) +
  
  geom_point(shape = 21, size = 1.5, stroke = 0.25, show.legend = FALSE) +
  #scale_fill +
  #scale_color +
  theme_plot +  
  geom_smooth(method = 'lm', color = "black", show.legend = FALSE) +
  scale_y_continuous(name = label,
                     trans = 'log10', 
                     #limits = c(0.4, 200), 
                     breaks = c(0.1, 1, 10, 100), 
                     position = "right",
                     labels = c("0.1", "1", "10", "100")) +
  scale_x_continuous(
    name = "Mean Annual Temperature (ºC)",
    sec.axis = sec_axis(~ 1 / ((. + 273.15) * 0.00008617), breaks = c(45, 43, 41, 39), name = expression("1/kT")))

print(p)

pdf("~/Downloads/shrew_ratio2.pdf")
 p
dev.off()
lm1 = lm(log(soric_crocid_rich) ~ one_kT + precip + log(npp), data = vert_df)
summary(lm1)

lm1 = lm(log(soric_crocid_rich) ~ one_kT, data = vert_df)
summary(lm1)
