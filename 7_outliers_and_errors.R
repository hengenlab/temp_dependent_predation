
########################################################################################################
############################### Errors, Outliers and Plot Checks #######################################
########################################################################################################
library(patchwork)
library(scales)
library(colorspace)
library(lme4)
library(lmerTest)
library(egg)
library(scales)
library(grid)
library(tidyverse)


hlab_path  <- ifelse(Sys.info()['user'] == 'jgradym', '/Volumes/HlabShare/Jacob_work',
                     file.path('O:\\Jacob_work'))
save_path  <- ifelse(Sys.info()['user'] == 'jgradym', '/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/mouse/Figures',
                     file.path('G:\\.shortcut-targets-by-id\\1Ece7oSFJTTDHja3uVWJaECrkbNea_bqm\\mouse\\Figures'))
save_path2 <- ifelse(Sys.info()['user'] == 'jgradym', '/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My\\ Drive/mouse/Figures', 
                     file.path('G:\\.shortcut-targets-by-id\\1Ece7oSFJTTDHja3uVWJaECrkbNea_bqm\\mouse\\Figures'))


source("outlier_fun.R") #outlier functions
source("capture_plot_fun.R")
source("remove_iso_numbers_fun.R")
source("contact_functions.R")

temp_scale <- c("14" = '#2c7bb6',"18" = '#abd9e9', "25" = 'gray70', "30" = '#fdae61', "35" = '#d7191c')
temp_scale2 <- c("13" = '#2c7bb6',"18" = '#abd9e9', "25" = 'gray70', "30" = '#fdae61', "35" = '#d7191c')
color_scale2 <- darken(temp_scale2, 0.2)
fill_scale2 <- lighten(temp_scale2, 0.2)

#############################################################################################################
#############################################################################################################
#-------------------------------------        Solo Roach         -------------------------------------------
#############################################################################################################
#############################################################################################################
roach_solo_vid00 = read_csv("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Mouse/Data/roach_solitary_dlc_features_all_temphum_trial_frame_final.csv")
roach_solo_vid00 = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/roach_solitary_dlc_features_all_temphum_trial_frame_final.csv")

str(roach_solo_vid00) #A tibble: 2,525,639 × 258

colnames = c("id", "date","trial_frame","arena", "targ_temp","trial_daily", "temp_median", "roach_mass", 
             "cockroachfront_cockroachback_dist", "roachcenter_disttravl", "roachcenter_vel" ,"roachcenter_acc",
             "cockroachfront_disttravl", "cockroachback_disttravl", "roach_distfromcenter",
             "roach_angular_displacement", "roach_angular_velocity",
             "roachcenter_x","roachcenter_y" , "temp_mean", "keep", "notes", "humd_mean",
             "video_frame")

roach_solo_vid0$date = as.Date(hunt_vid0$date, format="%m-%d-%y")
roach_solo_vid0 = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/roach_solitary_dlc_features_all_temphum_trial_frame_final_short.csv")
roach_solo_vid0 = read_csv("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Mouse/Data/roach_solitary_dlc_features_all_temphum_trial_frame_final_short.csv")

roach_solo_vid0$elapsed_sec = roach_solo_vid0$trial_frame/30 
# Get rid of nonmoving values
roach_solo_vid1 = roach_solo_vid0 %>% filter(keep == T)
roach_solo_vid1$id = paste(roach_solo_vid1$date, roach_solo_vid1$arena, roach_solo_vid1$trial_daily, sep = "_")

#get rid of obvious outliers
roach_solo_vid1$roachcenter_disttravl[is.na(roach_solo_vid1$cockroachfront_disttravl)] = NA
roach_solo_vid1$roachcenter_disttravl[is.na(roach_solo_vid1$cockroachback_disttravl)] = NA
roach_solo_vid1$roachcenter_disttravl[roach_solo_vid1$cockroachfront_cockroachback_dist > 20] = NA
roach_solo_vid1$roachcenter_disttravl[roach_solo_vid1$roachcenter_disttravl > 30]  = NA 

# apply to all movement columns
reference_column <- c("roachcenter_disttravl")
columns_to_change <- c("roachcenter_disttravl","roachcenter_acc","roachcenter_vel",
                       "roachcenter_x", "roachcenter_y") # add column names as needed
roach_solo_vid1[is.na(roach_solo_vid1[[reference_column]]), columns_to_change ] <- NA

# get rid of non-moving value
roach_solo_vid2 = roach_solo_vid1
roach_solo_vid1$roachcenter_vel[roach_solo_vid1$roachcenter_vel < 14.3]  = NA
roach_solo_vid1$roachcenter_acc[roach_solo_vid1$roachcenter_acc < 13.8]  = NA


# Remove outliers using IGR * coefficient
columns_to_check <- c('roachcenter_acc', 'roachcenter_disttravl', 'roachcenter_vel')
roach_solo_vid2 <- NA_outliers_trial(roach_solo_vid1, columns_to_check, coef = 3)
roach_solo_vid6 <- NA_outliers_temp(roach_solo_vid2, columns_to_check, coef = 6)
roach_solo_vid = roach_solo_vid6

#-------Visualize ----
ggplot(roach_solo_vid1, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(0.001, 30000), breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10() +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  


str(roach_solo_vid0)

range(roach_solo_vid0$roachcenter_disttravl, na.rm = T)
range(roach_solo_vid0$roachcenter_acc, na.rm = T)


ggplot(roach_solo_vid0[2000000:2100000,], aes(y = roachcenter_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_log10() +
  theme_plot()

ggplot(roach_solo_vid0[2000000:2100000,], aes(y = roachcenter_acc, x = video_frame)) +
  geom_point(shape = 21) + 
  theme_plot()

ggplot(roach_solo_vid0[2000000:2100000,], aes(y = roachcenter_disttravl, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_log10() +
  theme_plot()

ggplot(roach_solo_vid0[2000000:2100000,], aes(y = cockroachfront_cockroachback_dist, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_log10(limits = c(0.3, 30)) +
  theme_plot()
roach_solo_vid0
range(roach_solo_vid0$cockroachfront_cockroachback_dist, na.rm = T)

#look at outliers Acceleration
ggplot(roach_solo_vid0, aes(x = roachcenter_acc, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500,  show.legend = FALSE) +
  scale_x_log10(breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10() +
  geom_vline(xintercept = 450, color = "blue", linetype  = "dashed") +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

#look at outliers Velocity
ggplot(roach_solo_vid0, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10() +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  


#Visualize
ggplot(roach_solo_vid1, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(0.001, 30000), breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10() +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

ggplot(roach_solo_vid6, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(0.001, 30000), breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10() +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

#check = tibble(id = roach_solo_vid2$id,
 #              roachcenter_disttravl_0 = roach_solo_vid0$roachcenter_disttravl,roachcenter_disttravl_1 = roach_solo_vid1$roachcenter_disttravl, roachcenter_disttravl_2 = roach_solo_vid2$roachcenter_disttravl, 
  #             roachcenter_vel_0 =roach_solo_vid0$roachcenter_vel, roachcenter_vel_1 = roach_solo_vid1$roachcenter_vel, roachcenter_vel_2 = roach_solo_vid2$roachcenter_vel)


#--------compare outlier fixes, x and y log scale
p0 = ggplot(roach_solo_vid0, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 1000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("Original") + theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p1 = ggplot(roach_solo_vid1, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 1000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("Movement only, no obv outliers")  +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p2 = ggplot(roach_solo_vid2, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 1000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("No trial outliers, coef = 6") + theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  


p6 = ggplot(roach_solo_vid6, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 1000), breaks = c(1, 3, 10, 30, 100, 300)) +
  scale_y_log10() +
  ggtitle("No Temp Outliers, coef = 6") +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small

#combined_plot <- (p0 / p1 / plot_spacer()) | (p2  /p6)
#combined_plot
 (p0 / p1)  | (p2  /p6)

#----- compare outlier fixes, log x and arithmetic y  scale
p0 = ggplot(roach_solo_vid0, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 1000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_continuous() +
  ggtitle("Original") + theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p1 = ggplot(roach_solo_vid1, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 1000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_continuous() +
  ggtitle("Movement only")  +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p2 = ggplot(roach_solo_vid2, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 1000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_continuous() +
  ggtitle("No trial outliers, coef = 6") + theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p6 = ggplot(roach_solo_vid6, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 1000), breaks = c(1, 3, 10, 30, 100, 300)) +
  scale_y_continuous() +
  ggtitle("No Temp Outliers, coef = 6") +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small


#combined_plot <- p0/ p1/ p2 / p3/ p6
#combined_plot <- (p0 / p1 / plot_spacer()) | (p2 / p3 /p6)
(p0/ p1) | (p2 /  p6)

range(roach_solo_vid6$roachcenter_vel, na.rm = T)
(p =  ggplot(roach_solo_vid6 %>% filter(!is.na(roachcenter_vel)), aes(x = roachcenter_vel/10, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 300, show.legend = FALSE) +
  scale_x_log10(limits = c(1.4, 100), breaks = c(1, 3, 10, 30, 100, 300), name = "Red Runner Velocity (cm/s)") +
    scale_y_continuous(name = "Count") + 
  #ggtitle("No Temp Outliers, coef = 6") +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small)
save_fun(p = p, 'Fig2/freq_distrib/stacked_roach_vel.pdf', height = 10.25)

pdf("~/Downloads/stacked_roach_vel.pdf")
p
dev.off()

# just 13c
p2 = ggplot(roach_solo_vid2 %>% filter(targ_temp == 13), aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 100), breaks = c(1, 3, 10, 30, 100, 300)) +
  ggtitle("13C Trial outliers") +
  scale_y_log10() +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p3 = ggplot(roach_solo_vid6 %>% filter(targ_temp == 13), aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 100), breaks = c(1, 3, 10, 30, 100, 300)) +
  scale_y_log10() +
  ggtitle("13C 6 coef") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p2 / p3


# just 30c
p2 = ggplot(roach_solo_vid2 %>% filter(targ_temp == 30), aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 600), breaks = c(1, 3, 10, 30, 100, 300)) +
  ggtitle("13C Trial outliers") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p3 = ggplot(roach_solo_vid6 %>% filter(targ_temp == 30), aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 600), breaks = c(1, 3, 10, 30, 100, 300)) +
  ggtitle("13C 6 coef") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p2 / p3


range(roach_solo_vid2$roachcenter_vel[roach_solo_vid6$targ_temp == 35], na.rm = T)
range(roach_solo_vid6$roachcenter_vel[roach_solo_vid6$targ_temp == 35], na.rm = T)
range(roach_solo_vid2$roachcenter_vel[roach_solo_vid6$targ_temp == 23], na.rm = T)
range(roach_solo_vid6$roachcenter_vel[roach_solo_vid6$targ_temp == 23], na.rm = T)
#some different, some not
identical(roach_solo_vid6$roachcenter_vel[roach_solo_vid6$targ_temp == 35], roach_solo_vid2$roachcenter_vel[roach_solo_vid2$targ_temp == 35])
identical(roach_solo_vid6$roachcenter_vel, roach_solo_vid2$roachcenter_vel)
identical(roach_solo_vid6$roachcenter_vel[roach_solo_vid6$targ_temp == 30], roach_solo_vid2$roachcenter_vel[roach_solo_vid2$targ_temp == 30])
identical(roach_solo_vid6$roachcenter_vel[roach_solo_vid6$targ_temp == 23], roach_solo_vid2$roachcenter_vel[roach_solo_vid2$targ_temp == 23])
identical(roach_solo_vid6$roachcenter_vel[roach_solo_vid6$targ_temp == 13], roach_solo_vid2$roachcenter_vel[roach_solo_vid2$targ_temp == 13])

# Usage example
p1 = ggplot(roach_solo_vid6[2000000:3000000,], aes(y = roachcenter_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_log10() +
  theme_plot()

p2 = ggplot(roach_solo_vid6[2000000:3000000,], aes(y = roachcenter_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_log10() +
  theme_plot()

p1/p2

roach_solo_vid =  roach_solo_vid6
###############################################################
#############  roach solo results #########
###############################################################
str(roach_solo_vid)
roach_solo_results = roach_solo_vid  %>% 
  filter(keep == T) %>% #95th quantile on mouse and roach motion when not locomoting
  group_by(id) %>%
  dplyr::summarize(date = first(date),
                   id = first(id),
                   arena = first(arena),
                   trial_daily = first(trial_daily),roach_dist_travel = sum(roachcenter_disttravl, na.rm = T)/10,
                   targ_temp = mean(targ_temp),
                   temp_median = first(temp_median), 
                   temp_median_check = mean(temp_median), 
                   temp_mean = first(temp_mean), 
                   humd_mean = first(humd_mean),
                   roach_g = mean(roach_mass),
                   roach_acc_q50 = quantile(roachcenter_acc, probs = 0.50, na.rm = T)/10,
                   roach_acc_max = max(roachcenter_acc, na.rm = T)/10,
                   roach_angular_vel_q50 = quantile(roach_angular_velocity, probs = 0.5, na.rm = T),
                   roach_angular_vel_max = max(roach_angular_velocity, na.rm = T),
                   roach_dist_travel = sum(roachcenter_disttravl, na.rm = T)/10,
                   roach_vel_q50 = quantile(roachcenter_vel, probs = 0.5, na.rm = T)/10,
                   roach_vel_max = max(roachcenter_vel, na.rm = T)/10,
                   notes = first(notes),
                   keep = first(keep),
                   roach_vel_geomean = mean(log10(roachcenter_vel[roachcenter_vel > 0]), na.rm = TRUE) / 10,
                   roach_acc_geomean = mean(log10(roachcenter_acc[roachcenter_acc > 0]), na.rm = TRUE) / 10) %>%
  ungroup()
identical(roach_solo_results$temp_median,roach_solo_results$temp_median_check )
roach_solo_results$temp_median_check  = NULL
roach_solo_results$targ_temp[roach_solo_results$targ_temp == 23] = 25

#adjust for missing temp data
roach_solo_results = roach_solo_results %>%
  group_by(targ_temp) %>%
  mutate(mean_temp_cond = mean(roach_solo_results$temp_mean, na.rm = T),
         trial_temp = if_else(is.na(temp_mean), mean_temp_cond , temp_mean),#for missing rasp pi data add target data instead
         trial_temp_plot = if_else(is.na(temp_mean), jitter(mean_temp_cond), temp_mean)) %>% #average temp per thermal condition
  ungroup()

 
roach_solo_results$one_kT <-  1/(8.617e-5*(roach_solo_results$trial_temp + 273.15)) # In

#check averages
unique(roach_solo_results$targ_temp)
mean(roach_solo_results$temp_mean[roach_solo_results$targ_temp == 35], na.rm = T) #35.1
mean(roach_solo_results$temp_mean[roach_solo_results$targ_temp == 30], na.rm = T) #30.1
mean(roach_solo_results$temp_mean[roach_solo_results$targ_temp == 25], na.rm = T) #24.6 
mean(roach_solo_results$temp_mean[roach_solo_results$targ_temp == 18], na.rm = T) #17.6


#velocity
ggplot(data = roach_solo_results, aes(x = trial_temp, y = roach_vel_max )) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous() +
  ggtitle("Roach Velocity Max") +
  geom_point(color = "blue", shape = 21) +
  stat_smooth(data = . %>% filter(targ_temp <=30), color = "blue", method = "lm", se= F) +
  stat_smooth(color = "black", se = F) 

ggplot(data = roach_solo_results, aes(x = trial_temp, y = roach_vel_q50)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous() +
  ggtitle("Roach Velocity q50") +
  geom_point(color = "blue", shape = 21) +
  stat_smooth(data = . %>% filter(targ_temp <=30), color = "blue", method = "lm", se= F) +
  stat_smooth(color = "black", se = F) 

#acceleration
ggplot(data = roach_solo_results, aes(x = trial_temp_plot, y = roach_acc_max )) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous() +
  ggtitle("Roach Acceleration Max") +
  geom_point(color = "blue", shape = 21) +
  stat_smooth(data = . %>% filter(targ_temp <=30), color = "blue", method = "lm", se= F) +
  stat_smooth(color = "black", se = F) 

ggplot(data = roach_solo_results, aes(x = trial_temp, y = roach_acc_q50)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous() +
  ggtitle("Roach Acceleration q50") +
  geom_point(color = "blue", shape = 21) +
  stat_smooth(data = . %>% filter(targ_temp <=30), color = "blue", method = "lm", se= F) +
  stat_smooth(color = "black", se = F) 


# Slopes, 30 C cutoff

# Velocity
lm1 = lm(log(roach_vel_max) ~ log(roach_g) + one_kT, data = (roach_solo_results %>% filter(targ_temp <= 30)))
summary(lm1) # E = 0.63

lm1 = lm(log(roach_vel_q50) ~ log(roach_g) + one_kT, data = (roach_solo_results %>% filter(targ_temp <= 30)))
summary(lm1) # E = 0.42

# Acceleration
lm1 = lm(log(roach_acc_max) ~ log(roach_g) + one_kT, data = roach_solo_results %>% filter(targ_temp <= 30))
summary(lm1) # E = 0.32

lm1 = lm(log(roach_acc_q50) ~ log(roach_g) + one_kT, data = roach_solo_results %>% filter(targ_temp <= 30))
summary(lm1) # E = 0.14

#---------Save-----------
write_csv(roach_solo_vid, "/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/roach_solo_vid_jg.csv")
write_csv(roach_solo_vid1, "/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/roach_solo_vid1_jg.csv")

write_csv(roach_solo_results, "/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/roach_solo_results_jg.csv")
#write data wihout obvious outliers



################ SKIP ###################

# previously considererd Criteria for removing outliers and non-locomotory movement

#range(roach_solo_vid1$roachcenter_disttravl, na.rm = T)
#roach_solo_vid1$roachcenter_disttravl[roach_solo_vid1$roachcenter_disttravl > 20]  =NA #outlier threshold for distance traveled frame to frame
#roach_solo_vid1$roachcenter_disttravl[roach_solo_vid1$cockroachfront_cockroachback_dist> 20]  =NA #upper outlier
#roach_solo_vid1$roachcenter_disttravl[is.na(roach_solo_vid1$cockroachfront_disttravl)]  =NA #when missing data on half of roach position
#roach_solo_vid1$roachcenter_disttravl[is.na(roach_solo_vid1$cockroachback_disttravl)]  =NA #when missing data on other half of roach position
#roach_solo_vid1$roachcenter_disttravl[roach_solo_vid1$roachcenter_vel < 14.3]  =NA #velocity when not locomoting (at rest)
#roach_solo_vid1$roachcenter_disttravl[roach_solo_vid1$roachcenter_acc < 13.8]  = NA #accel when not locomoting (at rest)
#roach_solo_vid1$roachcenter_disttravl[roach_solo_vid1$roachcenter_acc > 450]  = NA #upper outlier
#roach_solo_vid1$roachcenter_disttravl[roach_solo_vid1$roachcenter_vel > 600]  = NA #lower outlier
#roach_solo_vid1 <- roach_solo_vid1 %>% # minimum of 10 unique acceleration and velocity values, corresponding to 1/3 of a second
#  group_by(id) %>%
#   mutate(roachcenter_vel = if(length(unique(roachcenter_vel[!is.na(roachcenter_vel)])) <= 10) rep(NA_real_, n()) else roachcenter_vel) %>%
#   mutate(roachcenter_acc = if(length(unique(roachcenter_acc[!is.na(roachcenter_acc)])) <= 10) rep(NA_real_, n()) else roachcenter_acc) %>%
#   ungroup()
# roach_solo_vid1$roachcenter_disttravl[is.na(roach_solo_vid1$roachcenter_acc)]  =NA
# roach_solo_vid1$roachcenter_disttravl[is.na(roach_solo_vid1$roachcenter_vel)]  =NA

#remove outliers based on above criteria

# reference_column <- "roachcenter_disttravl"
# columns_to_change = colnames(roach_solo_vid1)[8:18]
# roach_solo_vid1[is.na(roach_solo_vid1[[reference_column]]), columns_to_change ] <- NA #assign NA to columns where roachcenter_disttravl is NA (which is a function of above criteria)







#################################################################################################################
#################################################################################################################
#-------------------------------------Hunting Videos ------------------------------------------------------------
##################################################################################################################
#################################################################################################################
hunt_vid00 = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/capture_dlc_features_all_temphum_trial_frame_ckbn_final.csv")
hunt_vid00 = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/capture_dlc_features_all_temphum_trial_frame_ckbn_final.csv")

str(hunt_vid00)


formatted_date <- format(date_obj, "%Y-%m-%d")
range(hunt_vid00$snout_roachcenter_dist, na.rm = T)
colnames = c("id","video_frame", "trial_frame", "keep", "roach_g","animal", "trial_daily", "cockroachfront_cockroachback_dist",  "arena", "start_time_cap", "end_time_cap", "elapsed_cap", "date","targ_temp", "roachcenter_disttravl", "spine_vel" ,"roachcenter_acc",
             "head_roachcenter_dist","cockroachfront_disttravl", "cockroachback_disttravl", "roach_distfromcenter","roach_angular_displacement", "roach_angular_velocity", "mouse_angular_displacement", "mouse_angular_velocity", 
             "snout_roachcenter_dist" , "angle_of_attack" , "spine_disttravl" ,"spine_disttravlcum" ,"spine_vel","spine_acc", "snout_roachcenter_azimuth",
             "snout_x", "snout_y", "spine_x", "spine_y","roachcenter_x","roachcenter_y" ,"roachcenter_vel" , "temp_median", "temp_mean", "humd_mean", "humd_median", "temp_max", "humd_max", "temp_min", "humd_min",
             "video")
hunt_vid0  = hunt_vid00 %>% dplyr::select(all_of(colnames))
hunt_vid0 = hunt_vid00 %>%
  rename(arena = areanum)
hunt_vid0$date = as.Date(hunt_vid0$date, format="%m-%d-%y")
hunt_vid0 %>% filter(hunt_vid0$date == "2022-02-21")
hunt_vid0$elapsed_sec = hunt_vid0$trial_frame/30 
hunt_vid0$id = paste(hunt_vid0$date, hunt_vid0$arena, hunt_vid0$trial_daily, sep = "_")
hunt_vid0$id2 = paste(hunt_vid0$date, hunt_vid0$animal, hunt_vid0$trial_daily, sep = "_")
hunt_vid0$elapsed_sec = hunt_vid0$trial_frame/30 
write_csv(hunt_vid0, "/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/capture_dlc_features_temphum_trial_frame_ckbn_final_subset.csv")

#-------- get rid of obvious outliers -------------------
hunt_vid0 = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/capture_dlc_features_temphum_trial_frame_ckbn_final_subset.csv")

#Remove obvious outliers
hunt_vid1 = hunt_vid0 %>% filter(keep == T)
hunt_vid1$roachcenter_disttravl[hunt_vid1$roachcenter_disttravl > 30]  = NA #outlier threshold for distance traveled frame to frame
hunt_vid1$roachcenter_disttravl[is.na(hunt_vid1$cockroachfront_disttravl)] = NA
hunt_vid1$roachcenter_disttravl[is.na(hunt_vid1$cockroachback_disttravl)] = NA
hunt_vid1$roachcenter_disttravl[hunt_vid1$cockroachfront_cockroachback_dist > 20] = NA #when points on roach unnaturally separate
reference_column <- c("roachcenter_disttravl")
columns_to_change <- c("roachcenter_disttravl","roachcenter_acc","roachcenter_vel",  "roachcenter_x", "roachcenter_y") # add column names as needed
hunt_vid1[is.na(hunt_vid1[[reference_column]]), columns_to_change ] <- NA

#------ Get rid of Non-moving Values
hunt_vid2 = hunt_vid1
hunt_vid2$roachcenter_vel[hunt_vid2$roachcenter_vel < 14.3]  = NA
hunt_vid2$roachcenter_acc[hunt_vid2$roachcenter_acc < 13.8]  = NA
hunt_vid2$spine_vel[hunt_vid1$spine_vel < 76.9 ]  = NA
hunt_vid2$spine_vel[hunt_vid1$spine_acc < 31.3]  = NA
reference_column <- c("roachcenter_disttravl")
columns_to_change <- c("roachcenter_disttravl","roachcenter_acc","roachcenter_vel",  "roachcenter_x", "roachcenter_y") # add column names as needed
hunt_vid2[is.na(hunt_vid2[[reference_column]]), columns_to_change ] <- NA

reference_column <- c("spine_vel")
columns_to_change <- c("spine_acc", "spine_disttravl") # add column names as needed
hunt_vid2[is.na(hunt_vid2[[reference_column]]), columns_to_change ] <- NA


#-------------- Remove outliers for mice and roaches using IGR * coefficient
columns_to_check_roaches <- c('roachcenter_vel', "roachcenter_disttravl","roachcenter_acc","roachcenter_vel",  "roachcenter_x", "roachcenter_y")
columns_to_check_mice <- c('spine_vel', 'spine_acc', 'spine_disttravl',  "snout_x", "snout_y")

hunt_vid3 <- NA_outliers_trial(hunt_vid2, columns_to_check_roaches, coef = 3)
hunt_vid3 <- NA_outliers_trial(hunt_vid3, columns_to_check_mice, coef = 3)

hunt_vid4 <- NA_outliers_temp(hunt_vid3, columns_to_check_roaches, coef = 3)
hunt_vid4 <- NA_outliers_temp(hunt_vid4, columns_to_check_mice, coef = 3)

hunt_vid6 <- NA_outliers_temp(hunt_vid3, columns_to_check_roaches, coef = 6)
hunt_vid6 <- NA_outliers_temp(hunt_vid6, columns_to_check_mice, coef = 6)


hunt_vid = hunt_vid6 %>%
  group_by(id) %>%
  mutate(is_contact = is_contact(roachcenter_x, n = 10)) %>%
  mutate(n_contact = n_contact(roachcenter_x, n = 10)) %>%
  mutate(n_contact3 = n_contact(roachcenter_x, n = 3)) %>%
  mutate(n_contact30 = n_contact(roachcenter_x, n = 30)) %>%
  mutate(length_contact = length_contact(roachcenter_x, n = 10)) %>%
  #mutate(length_contact3 = length_contact(roachcenter_x, n = 3)) %>%
  ungroup()

unique(hunt_vid$targ_temp)
hunt_vid$targ_temp[hunt_vid$targ_temp == 23] = 25
hunt_vid$targ_temp[hunt_vid$targ_temp == 24] = 25
hunt_vid$targ_temp[hunt_vid$targ_temp == 11] = 14
hunt_vid$targ_temp[hunt_vid$targ_temp == 13] = 14
hunt_vid$targ_temp[hunt_vid$targ_temp == 34] = 35
hunt_vid$targ_temp[hunt_vid$targ_temp == 17] = 18

#----------- check and visualize 

range(hunt_vid0$roachcenter_disttravl, na.rm = T)
range(hunt_vid0$roachcenter_acc, na.rm = T)

ggplot(data = hunt_vid0 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

ggplot(data = hunt_vid1 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

ggplot(data = hunt_vid6 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

ggplot(data = hunt_vid6 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_point(aes(x = elapsed_sec, y = snout_roachcenter_dist/10), shape = 21, size  =2) +
  geom_line(aes(x = elapsed_sec, y = snout_roachcenter_dist/10))
  # arrow = arrow(angle = 15, ends = "both", type = "closed")

ggplot(hunt_vid0[2000000:2100000,], aes(y = roachcenter_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_continuous() +
  theme_plot()

ggplot(hunt_vid0[2000000:2100000,], aes(y = roachcenter_acc, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_continuous() +
  theme_plot()

ggplot(hunt_vid0[2000000:2100000,], aes(y = roachcenter_disttravl, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_continuous() +
  theme_plot()

ggplot(hunt_vid0[2000000:2100000,], aes(y = spine_disttravl, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_continuous() +
  theme_plot()

ggplot(hunt_vid0[2000000:2100000,], aes(y = cockroachfront_cockroachback_dist, x = video_frame)) +
  geom_point(shape = 21) + 
  theme_plot() + 
  scale_y_continuous(breaks = c(0, 20, 100, 200))

range(hunt_vid0$cockroachfront_cockroachback_dist, na.rm = T)

#Histogram: look at outliers Acceleration
unique(hunt_vid0$targ_temp)
ggplot(hunt_vid0, aes(x = roachcenter_acc, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500,  show.legend = FALSE) +
  scale_x_log10(breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10(name = "Roach Acceleration Count") +
  #geom_vline(color = "blue", linetype  = "dashed") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

#look at outliers Velocity
ggplot(hunt_vid0, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10(name = "Roach Velocity Count") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

#mouse
ggplot(hunt_vid0, aes(x =spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10(name = "Mouse Velocity Count") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

#look at outliers roach distance traveled frame to frame
ggplot(hunt_vid0, aes(x = roachcenter_disttravl, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10("Roach Distance Traveled per Frame Count") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  



source("remove_iso_numbers_fun.R")
#hunt_vid1b <- remove_iso_numbers(data = hunt_vid1, columns = c("roachcenter_x", "roachcenter_y"))

ggplot(data = hunt_vid0 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

ggplot(data = hunt_vid1 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

ggplot(data = hunt_vid2%>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

#Visualize roach
p0 = ggplot(hunt_vid0, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c( 10, 100, 1000, 10000), 
                labels = c("10", "100", "1000", "10000")) +
  scale_y_log10(limits = c(1, 1e15)) +
  ggtitle("Original") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p1 = ggplot(hunt_vid1, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c( 10, 100, 1000, 10000), 
                labels = c("10", "100", "1000", "10000")) +
  ggtitle("No motionless, Some corrections") +
  scale_y_log10(limits = c(1, 1e15)) +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p0/p1

#Visualize mouse
p0 = ggplot(hunt_vid0, aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(0.001, 10000), breaks = c( 10, 100, 1000, 10000), 
                labels = c("10", "100", "1000", "10000")) +
  scale_y_log10(limits = c(1, 1e18)) +
  ggtitle("Original") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p1 = ggplot(hunt_vid1, aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(0.001, 10000), breaks = c( 10, 100, 1000, 10000), 
                labels = c("10", "100", "1000", "10000")) +
  ggtitle("No motionless, Some corrections") +
  scale_y_log10(limits = c(1, 1e18)) +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  
p0/p1

# Mouse upper end comparison
p0 = ggplot(hunt_vid0, aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(30, 3000), breaks = c( 10, 100, 1000, 10000), 
                labels = c("10", "100", "1000", "10000")) +
  scale_y_log10(limits = c(1, 1e15)) +
  ggtitle("Original") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p1 = ggplot(hunt_vid1, aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(30, 3000), breaks = c( 10, 100, 1000, 10000), 
                labels = c("10", "100", "1000", "10000")) +
  ggtitle("No motionless, Some corrections") +
  scale_y_log10(limits = c(1, 1e15)) +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  
p0/p1


#Roach Corrections cont: remove frames where only one roach speed and acceleration values when roaches are not locomoting

#Visualize Roach Speed
ggplot(hunt_vid1, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c( 10, 100, 1000, 10000), 
                labels = c("10", "100", "1000", "10000")) +
  scale_y_log10() +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

#Visualize Mouse Speed
ggplot(hunt_vid1, aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c( 10, 100, 1000, 10000), 
                labels = c("10", "100", "1000", "10000")) +
  scale_y_log10() +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small

ggplot(data = hunt_vid0 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

ggplot(data = hunt_vid1 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

#check_1b = hunt_vid1b %>% select(id, trial_frame, roachcenter_x, roachcenter_y, roachcenter_acc, roachcenter_vel)
ggplot(data = hunt_vid1b %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

ggplot(data = hunt_vid3 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")


ggplot(data = hunt_vid4 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

ggplot(data = hunt_vid6 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")



check0 = hunt_vid0 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21") %>% 
  dplyr::select(trial_frame, roachcenter_x, roachcenter_y, roachcenter_acc, roachcenter_vel, roachcenter_disttravl)  

check1 = hunt_vid1 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21") %>% 
  dplyr::select(trial_frame, roachcenter_x, roachcenter_y, roachcenter_acc, roachcenter_vel, roachcenter_disttravl)  
 

check2 = hunt_vid2 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21") %>% 
  dplyr::select(trial_frame, roachcenter_x, roachcenter_y, roachcenter_acc, roachcenter_vel, roachcenter_disttravl)  

check3 = hunt_vid3 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21") %>% 
  dplyr::select(trial_frame, roachcenter_x, roachcenter_y, roachcenter_acc, roachcenter_vel, roachcenter_disttravl)  

check6 = hunt_vid6 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21") %>% 
  dplyr::select(trial_frame, roachcenter_x, roachcenter_y, roachcenter_acc, roachcenter_vel, roachcenter_disttravl)  


check0 = hunt_vid0 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21") %>% 
  dplyr::select(trial_frame, roachcenter_x, roachcenter_y) %>% 
  rename(roachcenter_x0 = roachcenter_x, roachcenter_y0 = roachcenter_y)

check1 = hunt_vid1 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21") %>% 
  dplyr::select(trial_frame, roachcenter_x, roachcenter_y) %>% 
  rename(roachcenter_x1 = roachcenter_x, roachcenter_y1 = roachcenter_y)

check2 = hunt_vid2 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21") %>% 
  dplyr::select(trial_frame, roachcenter_x, roachcenter_y) %>% 
  rename(roachcenter_x2 = roachcenter_x, roachcenter_y2 = roachcenter_y)

check3 = hunt_vid3 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21") %>% 
  dplyr::select(trial_frame, roachcenter_x, roachcenter_y) %>% 
  rename(roachcenter_x3 = roachcenter_x, roachcenter_y3 = roachcenter_y)

check6 = hunt_vid6 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21") %>% 
  dplyr::select(trial_frame, roachcenter_x, roachcenter_y) %>% 
  rename(roachcenter_x6 = roachcenter_x, roachcenter_y6 = roachcenter_y)

check = check0 %>% 
  left_join(check1, by = "trial_frame") %>%
  left_join(check2, by = "trial_frame") %>%
  left_join(check3, by = "trial_frame") %>%
  left_join(check6, by = "trial_frame")



Check0 = hunt_vid0 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21") %>% 
  dplyr::select(trial_frame,  roachcenter_x, roachcenter_y, roachcenter_vel, roachcenter_acc) %>% 
  rename(roachcenter_x0 = roachcenter_x, roachcenter_y0 = roachcenter_y) %>%
  rename(roachcenter_vel0 = roachcenter_vel, roachcenter_acc0 = roachcenter_acc)

Check1 = hunt_vid1 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21") %>% 
  dplyr::select(trial_frame, roachcenter_x, roachcenter_y, roachcenter_vel, roachcenter_acc) %>% 
  rename(roachcenter_x1 = roachcenter_x, roachcenter_y1 = roachcenter_y) %>%
  rename(roachcenter_vel1 = roachcenter_vel, roachcenter_acc1 = roachcenter_acc)

Check2 = hunt_vid2 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21") %>% 
  dplyr::select(trial_frame, roachcenter_x, roachcenter_y, roachcenter_vel, roachcenter_acc) %>% 
  rename(roachcenter_x2 = roachcenter_x, roachcenter_y2 = roachcenter_y) %>%
  rename(roachcenter_vel2 = roachcenter_vel, roachcenter_acc2 = roachcenter_acc)

Check3 = hunt_vid3 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21") %>% 
  dplyr::select(trial_frame, roachcenter_x, roachcenter_y, roachcenter_vel, roachcenter_acc) %>% 
  rename(roachcenter_x3 = roachcenter_x, roachcenter_y3 = roachcenter_y) %>%
  rename(roachcenter_vel3 = roachcenter_vel, roachcenter_acc3 = roachcenter_acc)

Check6 = hunt_vid6 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21") %>% 
  dplyr::select(trial_frame, roachcenter_x, roachcenter_y, roachcenter_vel, roachcenter_acc) %>% 
  rename(roachcenter_x6 = roachcenter_x, roachcenter_y6 = roachcenter_y) %>%
  rename(roachcenter_vel6 = roachcenter_vel, roachcenter_acc6 = roachcenter_acc)

Check = Check0 %>% 
  left_join(Check1, by = "trial_frame") %>%
  left_join(Check2, by = "trial_frame") %>%
  left_join(Check3, by = "trial_frame") %>%
  left_join(Check6, by = "trial_frame")

range(check$roachcenter_x6, na.rm = T)

ggplot(data = hunt_vid0 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

ggplot(data = hunt_vid1 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

ggplot(data = hunt_vid2 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

ggplot(data = hunt_vid0 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

ggplot(data = hunt_vid %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 3, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

ggplot(data = hunt_vid %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("jmg1, trial 3, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

#--------ROACH compare outlier fixes, x and y log scale #)
p0 = ggplot(hunt_vid0 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("Original") + theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot()  

p1 = ggplot(hunt_vid1 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04" ), aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  scale_y_log10() +
  ggtitle("Move only, corrections") +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p2 = ggplot(hunt_vid2 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("No trial outliers, coef = 6") + theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p3 = ggplot(hunt_vid3 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("No trial, no temp outliers, coef = 3") +theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p6 = ggplot(hunt_vid6 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  scale_y_log10("Roach Velocity Count") +
  ggtitle("No trial, no temp outliers, coef = 6") +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small


#combined_plot <- p0/ p1/ p2/ p3/ p6
combined_plot <- (p0 / p1 / plot_spacer()) | (p2 / p3 /p6)
combined_plot


#-------- MOUSE compare outlier fixes, x and y log scale #)
p0 = ggplot(hunt_vid0 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("Original Roach Velocity in Hunt") + theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot()  

p1 = ggplot(hunt_vid1 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04" ), aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  scale_y_log10() +
  ggtitle("Move only, corrections") +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p2 = ggplot(hunt_vid2 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(70, 3000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("No trial outliers, coef = 6") + theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p3 = ggplot(hunt_vid3 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(70, 3000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("No temp outliers, coef = 3") +theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p6 = ggplot(hunt_vid6 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(70, 3000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  scale_y_log10() +
  ggtitle("No Temp Outliers, coef = 6") +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small


#combined_plot <- p0/ p1/ p2/ p3/ p6
combined_plot <- (p0 / p1 / plot_spacer()) | (p2 / p3 /p6)
combined_plot

#----- compare outlier fixes, log x and arithmetic y  scale
p0 = ggplot(hunt_vid0 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04"), aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_continuous() +
  ggtitle("Original Roach Velocity in Hunt") + theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot()  

p1 = ggplot(hunt_vid1 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04"), aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  scale_y_continuous() +
  ggtitle("Move only, no obv mistakes") +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p2 = ggplot(hunt_vid2 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04"), aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_continuous() +
  ggtitle("No trial outliers, coef = 6") + theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p3 = ggplot(hunt_vid3 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04"), aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_continuous() +
  ggtitle("No temp outliers, coef = 3") +theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p6 = ggplot(hunt_vid6 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04"), aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  scale_y_continuous() +
  ggtitle("No Temp Outliers, coef = 6") +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small


combined_plot2 <- (p0 / p1 / plot_spacer()) | (p2 / p3 /p6)
combined_plot2


#--------Acceleration compare outlier fixes, x and y log scale #)
p0 = ggplot(hunt_vid0 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = roachcenter_acc, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("Original Roach Accel in Hunt") + theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot()  

p1 = ggplot(hunt_vid1 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04" ), aes(x = roachcenter_acc, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  scale_y_log10() +
  ggtitle("Move only, no obv mistakes") +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p2 = ggplot(hunt_vid2 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = roachcenter_acc, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("No trial outliers, coef = 6") + theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p3 = ggplot(hunt_vid3 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = roachcenter_acc, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("No temp outliers, coef = 3") +theme_no_x +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small  

p6 = ggplot(hunt_vid6 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = roachcenter_acc, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  scale_y_log10() +
  ggtitle("No Temp Outliers, coef = 6") +
  scale_fill_manual(values = temp_scale2) +
  theme_plot_small


#combined_plot <- p0/ p1/ p2/ p3/ p6
combined_plot3 <- (p0 / p1 / plot_spacer()) | (p2 / p3 /p6)
combined_plot3

#hunting with mice

p = ggplot(hunt_vid6 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = roachcenter_vel/10, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(1.2, 100), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000), name = "Red Runner Speed (cm/s)") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small

pdf("~/Downloads/roach_speed_with_hunt.pdf")
p
dev.off()
# just 13c
p2 = ggplot(hunt_vid2 %>% filter(targ_temp == 14), aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300)) +
  ggtitle("Roach Vel 13C Trial outliers") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p3 = ggplot(hunt_vid6 %>% filter(targ_temp == 14), aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300)) +
  ggtitle("13C 6 coef") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

combined_plot <- p2 / p3
combined_plot 

# just 30c
p2 = ggplot(hunt_vid2 %>% filter(targ_temp == 30), aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 4000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  ggtitle("Roach Vel 13C Trial outliers") +
  scale_y_log10() +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p3 = ggplot(hunt_vid6 %>% filter(targ_temp == 30), aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 4000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  ggtitle("13C 6 coef") +
  scale_y_log10() +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

combined_plot <- p2 / p3
combined_plot 

# Different comparison
p1 = ggplot(hunt_vid2[2000000:3000000,], aes(y = roachcenter_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_continuous(name = "Roach Velocity", limits = c(0 , 8000)) +
  theme_plot()

p2 = ggplot(hunt_vid6[2000000:3000000,], aes(y = roachcenter_acc, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_continuous(name = "Roach Velocity", limits = c(0 , 8000)) +
  theme_plot()

p1/p2

#log
p1 = ggplot(hunt_vid2[2000000:3000000,], aes(y = roachcenter_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_log10(limits = c(10, 10000)) +
  theme_plot()

p2 = ggplot(hunt_vid6[2000000:3000000,], aes(y = roachcenter_acc, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_log10(limits = c(10, 10000)) +
  theme_plot()

p1/p2

###################### Add STRIKES #############
hunt_vid$elapsed_s = as.numeric(hunt_vid$elapsed_cap)
unique(hunt_vid$elapsed_s)
#############  check results #########
hunt_results0 = hunt_vid %>% 
  group_by(id) %>%
  filter(keep == T) %>% #roach must travel at least 1 cm
  dplyr::summarize(date = first(date),
                   id = first(id),
                   animal = first(animal),
                   trial_daily = first(trial_daily),roach_dist_travel = sum(roachcenter_disttravl, na.rm = T)/10,
                   targ_temp = mean(targ_temp),
                   elapsed_s = first(elapsed_s),
                   roach_dist_travel = sum(roachcenter_disttravl, na.rm = TRUE) / 10,
                   roach_acc_q50 = quantile(roachcenter_acc, probs = 0.50, na.rm = T)/10,
                   roach_acc_geomean = mean(log10(roachcenter_acc[roachcenter_acc > 0]), na.rm = TRUE) / 10, 
                   roach_acc_max = if(all(is.na(roachcenter_acc))) NA else max(roachcenter_acc, na.rm = TRUE)/10,
                   roach_vel_q50 = quantile(roachcenter_vel, probs = 0.5, na.rm = T)/10,
                   roach_vel_max = if(all(is.na(roachcenter_vel))) NA else max(roachcenter_vel, na.rm = TRUE)/10,
                   roach_vel_geomean = mean(log10(roachcenter_vel[roachcenter_vel > 0]), na.rm = TRUE) / 10,
                   roach_angular_vel_q50 = quantile(roach_angular_velocity, probs = 0.5, na.rm = T),
                   roach_angular_vel_max = if(all(is.na(roach_angular_velocity))) NA else max(roach_angular_velocity, na.rm = TRUE),
                   roach_dist_travel = sum(roachcenter_disttravl, na.rm = T)/10,
                   #roach_acc_mean = mean(roachcenter_acc, na.rm = T)/10,
                   snout_roach_dist_q50 = quantile(snout_roachcenter_dist, probs = 0.5, na.rm = TRUE) / 10,
                   strikes_n = if(all(is.na(n_contact))) NA else max(n_contact, na.rm = TRUE),
                   strike_duration_max = if(all(is.na(length_contact))) NA else max(length_contact, na.rm = TRUE)/30,
                   #strike_duration3_max =  if(all(is.na(length_contact3))) NA else max(length_contact3, na.rm = TRUE)/30,
                   strike_duration_q50 = quantile(length_contact, probs = 0.50, na.rm = TRUE) / 30,
                  # strike_duration3_q50 = quantile(length_contact3, probs = 0.50, na.rm = TRUE) / 30,
                   mouse_vel_q50 = quantile(spine_vel, probs = 0.50, na.rm = TRUE) / 10,
                   mouse_vel_max = if(all(is.na(spine_vel))) NA_real_ else max(spine_vel, na.rm = TRUE) / 10,
                   #mouse_vel_mean = mean(spine_vel, na.rm = TRUE) / 10,
                   mouse_vel_geomean = if(all(spine_vel <= 0, na.rm = TRUE)) NA_real_ else exp(mean(log(spine_vel[spine_vel > 0]), na.rm = TRUE)) / 10,
                   mouse_acc_q50 = quantile(spine_acc, probs = 0.50, na.rm = TRUE) / 10,
                   mouse_acc_max = if(all(is.na(spine_acc))) NA_real_ else max(spine_acc, na.rm = TRUE) / 10,
                   mouse_acc_geomean = if(all(spine_acc <= 0, na.rm = TRUE)) NA_real_ else exp(mean(log(spine_acc[spine_acc > 0]), na.rm = TRUE)) / 10,
                   #mouse_acc_mean = mean(spine_acc, na.rm = TRUE) / 10,
                   mouse_angular_vel_q50 = quantile(mouse_angular_velocity, probs = 0.50, na.rm = TRUE),
                   mouse_angular_vel_max = if(all(is.na(mouse_angular_velocity))) NA_real_ else max(mouse_angular_velocity, na.rm = TRUE),
                   mouse_dist_travel = sum(spine_disttravl, na.rm = TRUE) / 10,
                   arena = first(arena),
                   frame_count_roach = sum(is.finite(roachcenter_x)), 
                   frame_count_mouse = sum(is.finite(spine_x)),
                   move_count_roach = sum(is.finite(roachcenter_vel)), 
                   move_count_mouse = sum(is.finite(spine_vel)),
                   temp_median = first(temp_median), 
                   temp_mean = first(temp_mean), 
                   humd_mean = first(humd_mean),
                   roach_g = first(roach_g),
                   keep = first(keep)) %>% 
                   #roach_vel_mean = mean(roachcenter_vel, na.rm = T)/10)
  ungroup()

hunt_results0[sapply(hunt_results0, is.infinite)] <- NA
hunt_results0$id2 = paste(hunt_results0$date, hunt_results0$animal, hunt_results0$trial_daily, sep = "_")

#Add experimental Stage
hunt_results0 <- hunt_results0 %>%
  mutate(stage = case_when(
    date < as.Date("2022-01-24") ~ "initial_trials",
    date >= as.Date("2022-01-24") & date <= as.Date("2022-03-04") ~ "weekly_thermal",
    date >= as.Date("2022-03-07") & date <= as.Date("2022-03-18") ~ "daily_thermal",
    date > as.Date("2022-03-18") ~ "mismatch",
    TRUE ~ NA_character_  # Default case if none of the above conditions are met
  ))

hunt_results0 <- hunt_results0 %>%
  mutate(stage2 = case_when(
    stage %in% c("weekly_thermal", "daily_thermal") ~ "thermal_trials",
    TRUE ~ as.character(stage)  # Retain original stage values for all other cases
  ))
unique(hunt_results0$stage)
unique(hunt_results0$stage2)

check = hunt_results0 %>%
  group_by(targ_temp, stage2) %>%
  mutate(mean_temp_cond = mean(temp_mean, na.rm = TRUE)) %>%
  mutate(trial_temp = if_else(is.na(temp_mean), mean_temp_cond, temp_mean)) 
check $mean_temp_cond
# using observed means at other times when observed recordings are available
str(hunt_results0)
mean(hunt_results0$snout_roach_dist_q50, na.rm = T)
quantile(hunt_results0$snout_roach_dist_q50, 0.5,  na.rm = T)

# hunt_results1 = hunt_results0
# check= hunt_results0 %>% filter(stage2 == "thermal_trials")
# room_temp = mean(check$temp_mean[check$targ_temp == 25], na.rm = T) #24.7273
# hunt_results1$temp_mean2 = hunt_results1$temp_mean
# hunt_results1$temp_mean2[hunt_results1$stage2 == "initial_trials"] = room_temp

hunt_results0 <- hunt_results0 %>%
  group_by(targ_temp) %>%
    mutate(mean_temp_cond = mean(temp_mean, na.rm = TRUE),
           trial_temp = if_else(is.na(temp_mean), mean_temp_cond, temp_mean), 
           trial_temp_plot = if_else(is.na(temp_mean), jitter(mean_temp_cond), temp_mean)
           ) %>%
  ungroup()

hunt_results0$one_kT <-  1/(8.617e-5*(hunt_results0$trial_temp + 273.15)) # In

range(hunt_results0$frame_count_mouse, na.rm = T)
range(hunt_results0$frame_count_roach, na.rm = T)

range(hunt_results0$move_count_mouse, na.rm = T)
range(hunt_results0$move_count_roach, na.rm = T)


#-----at least 10 frames for locomotion analysis, otherwise NA
columns_to_change <- c("roach_dist_travel","roach_acc_q50","roach_acc_max" ,"roach_acc_geomean", "roach_angular_vel_q50", 
                       "roach_angular_vel_max", "roach_vel_q50", "roach_vel_max", "roach_vel_geomean") # add column names as needed
hunt_results1 = hunt_results0 %>%
  mutate(across(all_of(columns_to_change), ~if_else(move_count_roach < 10, NA_real_, .)))


#check averages
unique(hunt_results1$targ_temp)
check = hunt_results1 #%>% filter(date >= "2022-01-24", date <= "2022-03-04")
mean(check$temp_mean[check$targ_temp == 35], na.rm = T) #34.5
mean(check$temp_mean[check$targ_temp == 30], na.rm = T) #30.1
mean(check$temp_mean[check$targ_temp == 25], na.rm = T) #25.5 
mean(check$temp_mean[check$targ_temp == 18], na.rm = T) #17.8
mean(check$temp_mean[check$targ_temp == 14], na.rm = T) #14.0


check= hunt_results1 %>% filter(stage2 == "thermal_trials")
mean(check$temp_mean[check$targ_temp == 35], na.rm = T) #34.5
mean(check$temp_mean[check$targ_temp == 30], na.rm = T) #30.1
mean(check$temp_mean[check$targ_temp == 25], na.rm = T) #24.7
mean(check$temp_mean[check$targ_temp == 18], na.rm = T) #17.6
mean(check$temp_mean[check$targ_temp == 14], na.rm = T) #14.1


# look at counts
hunt_results1  %>%
  group_by(stage2, targ_temp) %>%
  summarize(count_non_na = sum(!is.na(roach_acc_q50)), .groups = 'drop')


#-------- add some metadata ---------------
hunt_results1$animal = NA
hunt_results1$animal[hunt_results$arena == "1A"] = "jmg1"
hunt_results1$animal[hunt_results$arena == "1B"] = "jmg2"
hunt_results1$animal[hunt_results$arena == "1C"] = "jmg3"
hunt_results1$animal[hunt_results$arena == "1D"] = "jmg4"
hunt_results1$animal[hunt_results$arena == "2A"] = "jmg5"
hunt_results1$animal[hunt_results$arena == "2B"] = "jmg6"
hunt_results1$animal[hunt_results$arena == "2C"] = "jmg7"
hunt_results1$animal[hunt_results$arena == "2D"] = "jmg8"


#add sex
hunt_results1$sex = NA
hunt_results1$sex[hunt_results1$animal == "jmg1"] = "F"
hunt_results1$sex[hunt_results1$animal == "jmg2"] = "F"
hunt_results1$sex[hunt_results1$animal == "jmg3"] = "F"
hunt_results1$sex[hunt_results1$animal == "jmg4"] = "M"
hunt_results1$sex[hunt_results1$animal == "jmg5"] = "M"
hunt_results1$sex[hunt_results1$animal == "jmg6"] = "M"
hunt_results1$sex[hunt_results1$animal == "jmg7"] = "F"
hunt_results1$sex[hunt_results1$animal == "jmg8"] = "F"

#roach must travel at least 1 cm for locomotion to be considered
#hunt_results$roach_dist_travel[hunt_results$roach_dist_travel < 1]  = NA #outlier threshold for distance traveled frame to frame

#reference_column <- c("roach_dist_travel")
#columns_to_change <- c("roach_acc_q50", "roach_acc_q95", "roach_acc_max","roach_acc_geomean", "roach_vel_geomean", "roach_vel_q50", "roach_vel_q95", "roach_vel_max") # add column names as needed
#hunt_results[is.na(hunt_results[[reference_column]]), columns_to_change ] <- NA



###################################################
########### add mouse weight ######################
###################################################
mass_path =  googledrive::drive_get("mouse_weights")
1
mouse_weight <- read_sheet(ss = mass_path)
1
mouse_weight
mouse_weight$date = as.Date(mouse_weight$date)
mouse_weight$animal = tolower(mouse_weight$animal)

# data mis-entered as 2021 instead of 2022
mouse_weight %>% filter(date < "2021-07-01")
mouse_weight[mouse_weight$date == "2022-01-10",]
mouse_weight[mouse_weight$date == "2022-01-09",]
mouse_weight$date[mouse_weight$date == "2021-01-09"] = "2022-01-09"
mouse_weight$date[mouse_weight$date == "2021-01-10"] = "2022-01-10"

ggplot(data = mouse_weight, aes(x = date, y = mass_g)) +
  theme_plot() + 
  geom_point(aes(color = animal)) +
  geom_smooth(aes(color = animal), se = F)

mouse_weight$date_animal = paste(mouse_weight$date, mouse_weight$animal, sep = "_")
mouse_weight2 = mouse_weight %>% dplyr::select(mass_g, date_animal) %>%
  rename(mouse_g = mass_g)

#2022-04-02_jmg4_6

#hunt_results2 = hunt_results1 %>% filter(!is.na(date), !is.na(animal))
hunt_results1$date_animal =  paste(hunt_results1$date, hunt_results1$animal, sep = "_")
hunt_results2 = hunt_results1 %>% left_join(mouse_weight2, by = "date_animal")
sum(!is.na(hunt_results2$mouse_g))
sum(is.na(hunt_results2$mouse_g)) #interesting

hunt_results2$date_animal = NULL

########################################
#-------- Add trials at temperature ---
########################################

#-------------------- Add trials at temperature ------------------

# trial at temp excluding initial trials
trial_at_temp_df = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/capture_trial_metadata_with_trial_at_temp.csv")
trial_at_temp_df$date <- format(as.Date(trial_at_temp_df$date, "%m/%d/%y"),  '%Y-%m-%d')
trial_at_temp_df$id2 = paste(trial_at_temp_df$date, trial_at_temp_df$animal, trial_at_temp_df$trial_daily, sep = "_")
trial_at_temp_df = trial_at_temp_df%>% arrange(date, animal, trial_daily)
trial_at_temp2 = trial_at_temp_df %>% dplyr::select(id2, trial_at_temp)
range(trial_at_temp2$trial_at_temp, na.rm = T)

# trial at temp including initial trials
trial_at_temp_init_df = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/capture_trial_metadata_with_trial_at_temp_all.csv")
trial_at_temp_init_df$date <- format(as.Date(trial_at_temp_init_df$date, "%m/%d/%y"),  '%Y-%m-%d')
trial_at_temp_init_df$id2 = paste(trial_at_temp_init_df$date, trial_at_temp_init_df$animal, trial_at_temp_init_df$trial_daily, sep = "_")
trial_at_temp_init_df = trial_at_temp_init_df%>% arrange(date, animal, trial_daily)
trial_at_temp_init2 = trial_at_temp_init_df %>% dplyr::select(id2, trial_at_temp) %>% 
  rename(trial_at_temp_init = trial_at_temp)
#join
trial_at_temp_combo = trial_at_temp_init2 %>%
  left_join(trial_at_temp2, by = "id2")
range(trial_at_temp_combo$trial_at_temp_init, na.rm = T)
range(trial_at_temp_combo$trial_at_temp, na.rm = T)
hunt_results2$animal

# add to results
str(hunt_results2)
hunt_results2$id2
trial_at_temp_combo$id2
hunt_results3 = hunt_results2 %>% 
  left_join(trial_at_temp_combo, by = "id2") %>%
  dplyr::select(date, stage, targ_temp, trial_temp, animal,trial_daily, trial_at_temp, trial_at_temp_init, everything()) %>%
  arrange(date, animal, trial_daily)
range(hunt_results3$trial_temp, na.rm = T)


##########################################################
############# add more meta data #############
##########################################################
# combine with metadata
#----------- add meta code to get capture times ---------
cap_time_path <- googledrive::drive_get("capture_times")
1
meta <- read_sheet(ss = cap_time_path)
1
#quick look at temperature
#ggplot(meta, aes(x = targ_temp)) + geom_histogram() + theme_plot()

#standardize targeted temperature
meta$targ_temp[meta$targ_temp == 23] <- 25
meta$targ_temp[meta$targ_temp == 24] <- 25
meta$targ_temp[meta$targ_temp == 11] <- 14
meta$targ_temp[meta$targ_temp == 12] <- 14
meta$targ_temp[meta$targ_temp == 13] <- 14
meta$targ_temp[meta$targ_temp == 17] <- 18
meta$targ_temp[meta$targ_temp == 34] <- 35
meta$targ_temperature = as.factor(meta$targ_temp)
#ggplot(meta, aes(x = targ_temp)) + geom_histogram() + theme_plot
str(meta)
str(meta$elapsed_cap) # has too much info, need to prune
time_strings <- sub(".* ", "", meta$elapsed_cap)
meta$elapsed_cap2 <- as.POSIXlt(strptime(time_strings, format = "%H:%M:%S"))
meta$elapsed_cap2[1:3]
meta$elapsed_cap<- format(meta$elapsed_cap2, "%H:%M:%S")

# Converting to POSIXlt objects
meta$date_full <- meta$date #change name and vector type from date to character for plotting
meta$date <-  word(as.character(meta$date_full), start = 1, end = 3, sep = fixed("-")) #drop the year for our legend
meta$elapsed_s <- period_to_seconds(hms(meta$elapsed_cap)) #--- convert our times to an elapsed time that starts at 0
meta$mismatch_trial = as.logical(meta$mismatch_trial)
meta$mismatch = as.logical(meta$mismatch)
meta$keep = as.logical(meta$keep)
meta$trial_tot = as.numeric(meta$trial_tot)

# Create hunt dataset
meta <- meta %>%
  #filter(animal != "jmg2" ) %>%
  dplyr::select(date, targ_temp, animal, stage, trial_daily, elapsed_s, everything()) #---- rearrange columns  so important ones are first

unique(meta$targ_temp)
meta$targ_temperature <- as.factor(meta$targ_temp) 
meta$keep = as.logical(meta$keep)
meta <- meta %>%
  mutate(box = if_else(animal == "jmg1" | animal == "jmg2" | animal == "jmg3" | animal == "jmg4", "box1", "box2"))
meta$arena = NA
meta$arena[meta$animal == "jmg1"] = "1A"
meta$arena[meta$animal == "jmg2"] = "1B"
meta$arena[meta$animal == "jmg3"] = "1C"
meta$arena[meta$animal == "jmg4"] = "1D"
meta$arena[meta$animal == "jmg5"] = "2A"
meta$arena[meta$animal == "jmg6"] = "2B"
meta$arena[meta$animal == "jmg7"] = "2C"
meta$arena[meta$animal == "jmg8"] = "2D"

meta$id = paste(meta$date, meta$arena, meta$trial_daily, sep = "_")

unique(meta$targ_temp)
meta$stage
meta_short = meta %>% dplyr::select(id, mismatch, mismatch_trial, vid_review, elapsed_cap)
meta_short$elapsed_cap
meta_short$id

#--- join metadata
hunt_results3$id
meta_short$id
hunt_results4 = hunt_results3 %>%
  left_join(meta_short, by = "id") %>% 
  mutate(targ_temperature = as.factor(targ_temp),
  strike_rate = strikes_n/elapsed_s)

hunt_results = hunt_results4 %>% filter(keep == T, is.na(vid_review), animal != "jmg2")

range(hunt_results4$elapsed_cap, na.rm = T)
hunt_results = hunt_results4
hunt_35 = hunt_results %>% filter(targ_temp <= 35, stage == "weekly_thermal")
hunt_30 = hunt_results %>% filter(targ_temp <= 30, stage == "weekly_thermal")
hunt_25 = hunt_results %>% filter(targ_temp <= 25, stage == "weekly_thermal")

write_csv(hunt_results, "/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/hunt_results_jg.csv")
sum(!is.na(hunt_results$elapsed_s)) #1782 #2235

hunt_results$elapsed_s
meta$vid_review
unique(meta$vid_review)
meta_keep = meta %>% filter(keep == T, is.na(vid_review))
meta_keep$elapsed_s[1:10]
hunt_results$elapsed_s[1:10]
######################Acceleration and Velocity###########
check = hunt_vid %>% filter(date == "2022-02-17", trial_daily == 6, arena == "2A")
check

# Time to capture
ggplot(data = hunt_results %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-18"), 
       aes(x = trial_temp, y = elapsed_s)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("time to capture") +
  #geom_text(aes(label = id)) +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 

lmer1 = lmer(log(elapsed_s) ~ one_kT + log(roach_g) + (1|arena), data = hunt_results %>% filter(targ_temp <= 30))
summary(lmer1) #0.52

# Time to capture
ggplot(data = hunt_results %>% filter(stage2 == "thermal_trials"), 
       aes(x = trial_temp, y = snout_roach_dist_q50)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("snout_roach_dist") +
  #geom_text(aes(label = id)) +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 


# Time to capture
ggplot(data = hunt_results %>% filter(stage2 == "thermal_trials"), 
       aes(x = trial_temp, y = strikes_n)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("snout_roach_dist") +
  #geom_text(aes(label = id)) +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 

lmer1 = lmer(log(strikes_n) ~ one_kT + log(roach_g) + (1|arena), data = hunt_results %>% filter(targ_temp <= 30))
summary(lmer1) #0.52

#check "2022-03-10_2C_6"


# Mouse Distance Traveled
ggplot(data = hunt_results %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-18", mouse_dist_travel > 0), 
       aes(x = trial_temp, y = mouse_dist_travel)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Mouse Distance Traveled (cm)") +
  #geom_text(aes(label = id)) +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 

lmer1 = lmer(log(mouse_dist_travel) ~ one_kT + log(roach_g) + (1|arena), data = hunt_results %>% filter(targ_temp <= 30))
summary(lmer1) #0.63


# Roach Distance Traveled
ggplot(data = hunt_results %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-18", mouse_dist_travel > 0), 
       aes(x = trial_temp, y = roach_dist_travel)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Roach Distance Traveled (cm)") +
  #geom_text(aes(label = id)) +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 

lmer1 = lmer(log(roach_dist_travel) ~ one_kT + log(roach_g) + (1|arena), data = hunt_results%>% filter(targ_temp <= 30))
summary(lmer1) #1.34

#
#--------Roaches

# Original Acceleration q50
p0 = ggplot(data = hunt_results0 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-18"), 
       aes(x = trial_temp, y = roach_acc_q50)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Roach Acceleration q50 old") +
  #geom_text(aes(label = id)) +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 

# Accerlation q50, min 10 frames
p1 = ggplot(data = hunt_results %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-18"), 
       aes(x = trial_temp, y = roach_acc_q50)) +
  theme_plot() + 
  scale_y_log10(limits = c(1, 30)) + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Roach Acceleration q50") +
  #geom_text(aes(label = id)) +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 

p0 | p1

# Acceleration max
p2 = ggplot(data = hunt_results %>% filter(date >= "2022-01-24", date <= "2022-03-18"), 
       aes(x = trial_temp, y = roach_acc_max)) +
  theme_plot() + 
  scale_y_log10() + 
  geom_text(aes(label = id)) +
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Roach Acceleration Max") +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 


# Velocity q50
p3 = ggplot(data = hunt_results %>% filter(date >= "2022-01-24", date <= "2022-03-18"), 
       aes(x = trial_temp, y = roach_vel_q50)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Roach Velocity q50") +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 


lmer1 = lmer(log(roach_vel_q50) ~ one_kT + log(roach_g) + (1|arena), data = hunt_results %>% filter(targ_temp <= 30))
summary(lmer1) #0.51
#compare 1.21 (dist travel) - 0.47 (time to capture) = 0.74

# Velocity Max
p4 = ggplot(data = hunt_results %>% filter(date >= "2022-01-24", date <= "2022-03-18"), 
       aes(x = trial_temp, y = roach_vel_max)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Roach Velocity Max") +
  geom_text(aes(label = id)) +
 # geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  #geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
   #           aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 

(p1/p2) | (p3/p4)

check = hunt_vid %>% filter(id == "2022-03-04_2A_6")

#Angular Velocity

ggplot(data = hunt_results %>% filter(date >= "2022-01-24", date <= "2022-03-18"), 
       aes(x = trial_temp, y = roach_angular_vel_max)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Roach Velocity Max") +
  #geom_text(aes(label = id)) +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 

lmer1 = lmer(log(roach_angular_vel_max) ~ one_kT + log(roach_g) + (1|arena), data = hunt_results)
summary(lmer1) #0.50


lmer1 = lmer(log(roach_angular_vel_q50) ~ one_kT + log(roach_g) + (1|arena), data = hunt_results)
summary(lmer1) #0.50

ggplot(data = hunt_results %>% filter(date >= "2022-01-24", date <= "2022-03-18"), 
            aes(x = trial_temp, y = roach_angular_vel_q50)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Roach Velocity Max") +
  #geom_text(aes(label = id)) +
   geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
             aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
   geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 

str(hunt_results)
#------- contacts
ggplot(data = hunt_results %>% filter(date >= "2022-01-24", date <= "2022-03-18"), 
       aes(x = trial_temp, y = strikes_n)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("# of Strikes") +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 
lmer1 = lmer(log(strikes_n) ~ one_kT + log(roach_g) + (1|arena), data = hunt_results)
summary(lmer1) #0.50

ggplot(data = hunt_results %>% filter(date >= "2022-01-24", date <= "2022-03-18"), 
       aes(x = trial_temp, y = roach_vel_max)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Max Duration of Strike (s)") +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 


#median strike length
ggplot(data = hunt_results %>% filter(date >= "2022-01-24", date <= "2022-03-18"), 
       aes(x = trial_temp, y = strike_duration_q50)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Max Duration of Strike (s)") +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 
lmer1 = lmer(log(strike_duration_q50) ~ one_kT + log(roach_g) + (1|arena), data = hunt_results)
summary(lmer1) #0.02


#jitter unavailable temp data

check %>% filter(!targ_temp %in% c(14, 18, 30, 35))

# Slopes, 30 C cutoff

# Velocity

lmer1 = lmer(log(roach_vel_max) ~ log(roach_g) + one_kT + (1|arena), data = (hunt_results %>% filter(stage2 == "thermal_trials", targ_temp <= 30)))
summary(lmer1) # E = 0.62

lmer1 = lmer(log(roach_vel_q50) ~ log(roach_g) + one_kT + (1|arena), data = (hunt_results %>% filter(stage2 == "thermal_trials", targ_temp <= 30)))
summary(lmer1) # E = 0.62

# Acceleration
lmer1 = lmer(log(roach_acc_max) ~ log(roach_g) + one_kT + (1|arena), data = hunt_results %>% filter(stage2 == "thermal_trials", targ_temp <= 30))
summary(lmer1) # E = 0.50

lmer1 = lmer(log(roach_acc_q99) ~ log(roach_g) + one_kT + (1|arena), data = hunt_results %>% filter(stage2 == "thermal_trials", targ_temp <= 30))
summary(lmer1) # E = 0.47

lmer1 = lmer(log(roach_acc_q50) ~ log(roach_g) + one_kT + (1|arena), data = hunt_results %>% filter(stage2 == "thermal_trials", targ_temp <= 30))
summary(lmer1) # E = 0.21


#-------------------Save ----------------
write_csv(hunt_vid, "/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/hunt_vid_jg.csv")
write_csv(hunt_vid1, "/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/hunt_vid1_jg.csv")
write_csv(hunt_results, "/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/hunt_results_jg.csv")


##################################################################################
################  Pre- Drop Videos #################################################
##################################################################################
prehunt_vid00 = read_csv('/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/capture_pretrial_dlc_features_all_temphum_trial_frame_ckbn_final.csv')
colnames = c("id","video_frame", "trial_frame", "areanum", "start_time_cap", "end_time_cap", "elapsed_cap", "date","targ_temp", "animal", "trial_daily", "roachcenter_disttravl", "roachcenter_vel" ,"roachcenter_acc",
             "head_roachcenter_dist", "roach_distfromcenter","roach_angular_displacement", "roach_angular_velocity", "mouse_angular_displacement", "mouse_angular_velocity", 
             "snout_roachcenter_dist" , "angle_of_attack" , "spine_disttravl" ,"spine_disttravlcum" ,"spine_vel","spine_acc", "snout_roachcenter_azimuth",
             "snout_x", "snout_y", "spine_x", "spine_y","roachcenter_x","roachcenter_y" ,  "temp_median", "temp_mean", "humd_mean", "humd_median", "temp_max", "humd_max", "temp_min", "humd_min",
             "video")
prehunt_vid0 = prehunt_vid00%>% dplyr::select(all_of(colnames))
prehunt_vid0 = prehunt_vid0 %>% rename(arena = areanum)
prehunt_vid0$targ_temp[prehunt_vid0$targ_temp == 23] <- 25
prehunt_vid0$targ_temp[prehunt_vid0$targ_temp == 24] <- 25
prehunt_vid0$targ_temp[prehunt_vid0$targ_temp == 11] <- 14
prehunt_vid0$targ_temp[prehunt_vid0$targ_temp == 12] <- 14
prehunt_vid0$targ_temp[prehunt_vid0$targ_temp == 13] <- 14
prehunt_vid0$targ_temp[prehunt_vid0$targ_temp == 17] <- 18
prehunt_vid0$targ_temp[prehunt_vid0$targ_temp == 34] <- 35
prehunt_vid0$date 
prehunt_vid0$date = as.Date(prehunt_vid0$date, format = "%m-%d-%y")
#prehunt_vid0 = read_csv('/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/capture_pretrial_dlc_features_subset.csv')
prehunt_vid0$id = paste(prehunt_vid0$date, prehunt_vid0$arena, prehunt_vid0$trial_daily, sep = "_")
prehunt_vid0$elapsed_sec = prehunt_vid0$trial_frame/30 
prehunt_vid0$targ_temperature = as.factor(prehunt_vid0$targ_temp)

str(prehunt_vid0)

#---------Remove obvious outliers
prehunt_vid2 = prehunt_vid0 #%>% filter(keep == T)
prehunt_vid2$spine_vel[prehunt_vid2$spine_vel < 76.9 ]  = NA
prehunt_vid2$spine_acc[prehunt_vid2$spine_acc < 31.3]  = NA
reference_column <- c("spine_vel")
columns_to_change <- c("spine_acc", "spine_disttravl", "spine_x", "spine_y") # add column names as needed
prehunt_vid2[is.na(prehunt_vid2[[reference_column]]), columns_to_change ] <- NA


# Remove outliers for mice using IGR * coefficient
columns_to_check_mice <- c('spine_acc', 'spine_disttravl', 'spine_vel')

prehunt_vid3 <- NA_outliers_trial(prehunt_vid2, columns_to_check_mice, coef = 3)
prehunt_vid6 <- NA_outliers_temp(prehunt_vid3, columns_to_check_mice, coef = 6)
#prehunt_vid6b <- NA_outliers_temp(prehunt_vid2, columns_to_check_mice, coef = 6)

prehunt_vid = prehunt_vid6
str(prehunt_vid0)

range(prehunt_vid0$spine_vel, na.rm = T)

p1 = ggplot(prehunt_vid0[2000000:2050000,], aes(y = spine_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  ggtitle("all prehunt") +
  scale_y_continuous(limits = c(0, 2000)) +
  theme_plot()

p2 =ggplot(prehunt_vid2[2000000:2050000,], aes(y = spine_acc, x = video_frame)) +
  geom_point(shape = 21) + 
  ggtitle("movement only") +
  scale_y_continuous(limits = c(0, 2000)) +
  theme_plot()


p3 = ggplot(prehunt_vid6[2000000:2050000,], aes(y = spine_acc, x = video_frame)) +
  geom_point(shape = 21) + 
  ggtitle("movement only, no outliers") +
  scale_y_continuous(limits = c(0, 2000)) +
  theme_plot()

p1/p2/p3

range(prehunt_vid0$spine_acc, na.rm = T)
range(prehunt_vid1$spine_acc, na.rm = T)
range(prehunt_vid6$spine_acc, na.rm = T)

#Histogram: look at outliers Acceleration
unique(prehunt_vid0$targ_temp)

#mouse
p1 = ggplot(prehunt_vid2, aes(x =spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(70, 2000), breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10(name = "Mouse Velocity Count, only move") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

#look at outliers 
p2 = ggplot(prehunt_vid6, aes(x = spine_acc, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(70, 2000),breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10("Mouse ACC count, only move no outliers") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p1/p2
#no limits
ggplot(prehunt_vid6, aes(x = spine_acc, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10("Mouse ACC count, only move no outliers") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

ggplot(prehunt_vid6, aes(x = spine_acc, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_continuous("Mouse ACC count, only move no outliers") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

#Visualize mouse
p0 = ggplot(prehunt_vid0, aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(0.001, 10000), breaks = c( 10, 100, 1000, 10000), 
                labels = c("10", "100", "1000", "10000")) +
  scale_y_log10(limits = c(1, 1e19)) +
  ggtitle("Original") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p1 = ggplot(prehunt_vid1, aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(0.001, 10000), breaks = c( 10, 100, 1000, 10000), 
                labels = c("10", "100", "1000", "10000")) +
  ggtitle("No motionless") +
  scale_y_log10(limits = c(1, 1e19)) +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  
p0/p1


#Roach Corrections cont: remove frames where only one roach speed and acceleration values when roaches are not locomoting

#Visualize Mouse Speed
ggplot(prehunt_vid1, aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c( 10, 100, 1000, 10000), 
                labels = c("10", "100", "1000", "10000")) +
  scale_y_log10() +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small



#-------- MOUSE compare outlier fixes, x and y log scale #)
p0 = ggplot(prehunt_vid0 %>% filter(date >= "2022-01-24", date <= "2022-03-04") ,
            aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("Original Roach Velocity in prehunt") + theme_no_x +
  scale_fill_manual(values = temp_scale) +
  theme_plot()  

p1 = ggplot(prehunt_vid1 %>% filter(date >= "2022-01-24", date <= "2022-03-04" ), 
            aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  scale_y_log10() +
  ggtitle("Move only, corrections") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p2 = ggplot(prehunt_vid2 %>% filter(date >= "2022-01-24", date <= "2022-03-04") ,
            aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(70, 3000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("No trial outliers, coef = 6") + theme_no_x +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p3 = ggplot(prehunt_vid3 %>% filter( date >= "2022-01-24", date <= "2022-03-04") , 
            aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(70, 3000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("No temp outliers, coef = 3") +theme_no_x +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p6 = ggplot(prehunt_vid6 %>% filter(date >= "2022-01-24", date <= "2022-03-04") , 
            aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(70, 3000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  scale_y_log10() +
  ggtitle("No Temp Outliers, coef = 6") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small


#combined_plot <- p0/ p1/ p2/ p3/ p6
combined_plot <- (p0 / p1 / plot_spacer()) | (p2 / p3 /p6)
combined_plot



# Different comparison
p1 = ggplot(prehunt_vid2[2000000:2200000,], aes(y = spine_acc, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_continuous(name = "Mouse Velocity", limits = c(0 , 8000)) +
  theme_plot()

p2 = ggplot(prehunt_vid6[2000000:2200000,], aes(y = spine_acc, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_continuous(name = "Mouse Velocity", limits = c(0 , 8000)) +
  theme_plot()

p1/p2

#log
p1 = ggplot(prehunt_vid0[2000000:2200000,], aes(y = spine_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_log10(limits = c(10, 10000)) +
  theme_plot()

p2 = ggplot(prehunt_vid6[2000000:2200000,], aes(y = spine_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_log10(limits = c(10, 10000)) +
  theme_plot()

p1/p2

identical(prehunt_vid2, prehunt_vid6)

#############  check results #########
str(prehunt_vid6)
prehunt_results = prehunt_vid6  %>% 
  group_by(id) %>%
  dplyr::summarize(date = first(date),
                   id = first(id),
                   arena = first(arena),
                   trial_daily = first(trial_daily),roach_dist_travel = sum(roachcenter_disttravl, na.rm = T)/10,
                   targ_temp = mean(targ_temp),
                   targ_temperature = first(targ_temp),
                   temp_median = first(temp_median), 
                   temp_mean = first(temp_mean), 
                   humd_mean = first(humd_mean),
                   #elapsed_s = first(elapsed_s),
                   mouse_vel_q50 = quantile(spine_vel, probs = 0.50, na.rm = TRUE) / 10,
                   mouse_vel_q95 = quantile(spine_vel, probs = 0.95, na.rm = TRUE) / 10,
                   mouse_vel_q99 = quantile(spine_vel, probs = 0.99, na.rm = TRUE) / 10,
                   mouse_vel_max = if(all(is.na(spine_vel))) NA_real_ else max(spine_vel, na.rm = TRUE) / 10,
                   mouse_vel_mean = mean(spine_vel, na.rm = TRUE) / 10,
                   mouse_vel_geomean = if(all(spine_vel <= 0, na.rm = TRUE)) NA_real_ else exp(mean(log(spine_vel[spine_vel > 0]), na.rm = TRUE)) / 10,
                   mouse_acc_q50 = quantile(spine_acc, probs = 0.50, na.rm = TRUE) / 10,
                   mouse_acc_q95 = quantile(spine_acc, probs = 0.95, na.rm = TRUE) / 10,
                   mouse_acc_q99 = quantile(spine_acc, probs = 0.99, na.rm = TRUE) / 10,
                   mouse_acc_max = if(all(is.na(spine_acc))) NA_real_ else max(spine_acc, na.rm = TRUE) / 10,
                   mouse_acc_geomean = if(all(spine_acc <= 0, na.rm = TRUE)) NA_real_ else exp(mean(log(spine_acc[spine_acc > 0]), na.rm = TRUE)) / 10,
                   mouse_acc_mean = mean(spine_acc, na.rm = TRUE) / 10,
                   mouse_angular_vel_q50 = quantile(mouse_angular_velocity, probs = 0.50, na.rm = TRUE),
                   mouse_angular_vel_max = if(all(is.na(mouse_angular_velocity))) NA_real_ else max(mouse_angular_velocity, na.rm = TRUE),
                   mouse_dist_travel = sum(spine_disttravl, na.rm = TRUE) / 10) %>% 
  ungroup()
prehunt_results$targ_temperature = as.factor(prehunt_results$targ_temperature)
prehunt_results$targ_temp[prehunt_results$targ_temp == 23] = 25
prehunt_results$mouse_acc_q50[1:10]
prehunt_results$mouse_acc_max[1:10]
#Add experimental Stage
prehunt_results <- prehunt_results %>%
  mutate(stage = case_when(
    date < as.Date("2022-01-24") ~ "initial_trials",
    date >= as.Date("2022-01-24") & date <= as.Date("2022-03-04") ~ "weekly_thermal",
    date >= as.Date("2022-03-07") & date <= as.Date("2022-03-18") ~ "daily_thermal",
    date > as.Date("2022-03-18") ~ "mismatch",
    TRUE ~ NA_character_  # Default case if none of the above conditions are met
  ))

prehunt_results <- prehunt_results %>%
  mutate(stage2 = case_when(
    stage %in% c("weekly_thermal", "daily_thermal") ~ "thermal_trials",
    TRUE ~ as.character(stage)  # Retain original stage values for all other cases
  ))
unique(prehunt_results$stage)
unique(prehunt_results$stage2)

#check averages
unique(prehunt_results$targ_temp)
check = prehunt_results %>% filter(date >= "2022-01-24", date <= "2022-03-04")
mean(check$temp_mean[check$targ_temp == 35], na.rm = T) #34.5
mean(check$temp_mean[check$targ_temp == 30], na.rm = T) #30.1
mean(check$temp_mean[check$targ_temp == 25], na.rm = T) #25.5 
mean(check$temp_mean[check$targ_temp == 18], na.rm = T) #17.8
mean(check$temp_mean[check$targ_temp == 14], na.rm = T) #14.0

# look at counts
prehunt_results  %>%
  group_by(stage2, targ_temp) %>%
  summarize(count_non_na = sum(!is.na(mouse_vel_max)), .groups = 'drop')

# using observed means at other times when observed recordings are available
prehunt_results <- prehunt_results %>%
  group_by(targ_temp) %>%
  mutate(mean_temp_cond = mean(temp_mean, na.rm = TRUE),
         trial_temp = if_else(is.na(temp_mean), mean_temp_cond, temp_mean), 
         trial_temp_plot = if_else(is.na(temp_mean), jitter(mean_temp_cond), temp_mean)
  ) %>%
  ungroup()
prehunt_results$one_kT <-  1/(8.617e-5*(prehunt_results$trial_temp + 273.15)) # In

#velocity



#jitter unavailable temp data
p1 = ggplot(data = prehunt_results %>% filter(date >= "2022-01-24", date <= "2022-03-18"), 
            aes(x = trial_temp, y = mouse_acc_q50)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Mouse median Acceleration") +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=35),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 
lmer1 = lmer(log(mouse_acc_q50) ~  one_kT + (1|arena), data = prehunt_results %>% filter(stage2 == "thermal_trials"))
summary(lmer1) # E = -0.0014

p2 = ggplot(data = prehunt_results %>% filter(date >= "2022-01-24", date <= "2022-03-18"), 
       aes(x = trial_temp, y = mouse_acc_max)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Mouse Acc Max") +
 
   geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  #geom_text(aes(x = targ_temp, color = as.factor(targ_temp), label = id), shape = 21, size = 2.5,  show.legend = F) +
  #geom_text(aes(x = temp_mean, color = as.factor(targ_temp), label = id), shape = 21, size = 2.5,  show.legend = F) +
  
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=35),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 

lmer1 = lmer(log(mouse_acc_max) ~  one_kT + (1|arena), data = prehunt_results %>% filter(stage2 == "thermal_trials"))
summary(lmer1) # E = 0.0018

p3 = ggplot(data = prehunt_results %>% filter(date >= "2022-01-24", date <= "2022-03-18"), 
       aes(x = trial_temp, y = mouse_vel_max)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Mouse Velocity Max") +
   geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  #geom_text(aes(x = targ_temp, color = as.factor(targ_temp), label = id), shape = 21, size = 2.5,  show.legend = F) +
  #geom_text(aes(x = temp_mean, color = as.factor(targ_temp), label = id), shape = 21, size = 2.5,  show.legend = F) +
  
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=35),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 


p4 = ggplot(data = prehunt_results %>% filter(date >= "2022-01-24", date <= "2022-03-18"), 
         aes(x = trial_temp, y = mouse_vel_q50)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Mouse Velocity Median") +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  #geom_text(aes(x = targ_temp, color = as.factor(targ_temp), label = id), shape = 21, size = 2.5,  show.legend = F) +
  #geom_text(aes(x = temp_mean, color = as.factor(targ_temp), label = id), shape = 21, size = 2.5,  show.legend = F) +
  
  # geom_boxplot(aes(color = as.factor(targ_temp)), size = 1, outlier.shape = NA, fill = NA, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=35),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 

(p1/p2) | (p3/p4)


check25 = check %>% filter(!targ_temp %in% c(14, 18, 30, 35))
check25
# Slopes, 30 C cutoff

# Velocity

lmer1 = lmer(log(mouse_vel_max) ~  one_kT + (1|arena), data = (prehunt_results %>% filter(stage2 == "thermal_trials", targ_temp <= 30)))
summary(lmer1) # E = 0.029

lmer1 = lmer(log(mouse_vel_q99) ~  one_kT+ (1|arena), data = (prehunt_results %>% filter(stage2 == "thermal_trials", targ_temp <= 30)))
summary(lmer1) # E = 0.22

lmer1 = lmer(log(mouse_vel_q50) ~  one_kT + (1|arena), data = (prehunt_results %>% filter(stage2 == "thermal_trials", targ_temp <= 30)))
summary(lmer1) # E = 0.0086

# Acceleration
lmer1 = lmer(log(mouse_acc_max) ~  one_kT + (1|arena), data = prehunt_results %>% filter(stage2 == "thermal_trials", targ_temp <= 30))
summary(lmer1)# E = 0.014

lmer1 = lmer(log(mouse_acc_q99) ~  one_kT+ (1|arena), data = prehunt_results %>% filter(stage2 == "thermal_trials", targ_temp <= 30))
summary(lmer1) # E = 0.0060

lmer1 = lmer(log(mouse_acc_q50) ~  one_kT+ (1|arena), data = prehunt_results %>% filter(stage2 == "thermal_trials", targ_temp <= 30))
summary(lmer1) # E = 0.00048


#Save

write_csv(prehunt_vid, "/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/prehunt_vid_jg.csv")
write_csv(prehunt_vid0, "/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/prehunt_vid1_jg.csv")
write_csv(prehunt_results, "/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/prehunt_results_jg.csv")








