
########################################################################################################
############################### Errors, Outliers and Plot Checks #######################################
########################################################################################################
library(patchwork)
library(scales)
library(colorspace)
library(scales)
library(grid)
library(tidyverse)

# ------------------- Load Paths and Custom Functions ---------------------
# Update paths
path <- "/Users/jgradym/Desktop/Predation_Data"
github_path = "/Users/jgradym/Documents/GitHub/mouse_capture"

# Load Functions
source(file.path(github_path, "7_experim_functions.R")) #outlier functions

# Load Files
roach_solo_vid00 = read_csv(file.path(path, "Experim_data/roach_solitary_dlc_features_all_temphum_trial_frame_final.csv"))
hunt_vid00 = read_csv(file.path(path, "Experim_data/capture_dlc_features_all_temphum_trial_frame_ckbn_final.csv"))
prehunt_vid00 = read_csv(file.path(path, "Experim_data", 'capture_pretrial_dlc_features_all_temphum_trial_frame_ckbn_final.csv'))
mouse_weight = read_csv(file.path(path, "Experim_data", "mouse_weight.csv"))
trial_at_temp = read_csv(file.path(path, "Experim_data", "trial_at_temp.csv"))
meta = read_csv(file.path(path, "Experim_data", "metadata.csv"))

#############################################################################################################
#############################################################################################################
#-------------------------------------        Solo Roach         -------------------------------------------
#############################################################################################################
#############################################################################################################

colnames = c("id", "date","trial_frame","arena", "targ_temp","trial_daily", "temp_mean", "roach_mass", 
             "cockroachfront_cockroachback_dist", "roachcenter_disttravl", "roachcenter_vel" ,"roachcenter_acc",
             "cockroachfront_disttravl", "cockroachback_disttravl", "roach_distfromcenter",
             "roach_angular_displacement", "roach_angular_velocity", 
             "roachcenter_x","roachcenter_y" , "keep", "notes", "humd_mean",
             "video_frame")
roach_solo_vid0 = roach_solo_vid00 %>% dplyr::select(all_of(colnames))
roach_solo_vid0$date = as.Date(roach_solo_vid0$date , format="%m-%d-%y")
roach_solo_vid0$elapsed_sec = roach_solo_vid0$trial_frame/30 

# Get rid of noted erros from trial (keep = F)
roach_solo_vid1 = roach_solo_vid0 %>% filter(keep == T)
roach_solo_vid1$id = paste(roach_solo_vid1$date, roach_solo_vid1$arena, roach_solo_vid1$trial_daily, sep = "_")

#----Preliminary Screening - Removal of incomplete paired observation and biological implausible values (see also plots below)

# Incomplete data pairs
roach_solo_vid1$roachcenter_disttravl[is.na(roach_solo_vid1$cockroachfront_disttravl)] = NA
roach_solo_vid1$roachcenter_disttravl[is.na(roach_solo_vid1$cockroachback_disttravl)] = NA

# Roach body parts more than 2 cm separated
roach_solo_vid1$roachcenter_disttravl[roach_solo_vid1$cockroachfront_cockroachback_dist > 20] = NA

# Distance traveled in one frame exceed 3 cm
roach_solo_vid1$roachcenter_disttravl[roach_solo_vid1$roachcenter_disttravl > 30]  = NA 

# Apply exclusion to all movement columns
reference_column <- c("roachcenter_disttravl")
columns_to_change <- c("roachcenter_disttravl","roachcenter_acc","roachcenter_vel",
                       "roachcenter_x", "roachcenter_y", "cockroachfront_cockroachback_dist") # add column names as needed
roach_solo_vid1[is.na(roach_solo_vid1[[reference_column]]), columns_to_change ] <- NA

# Get rid of independently determined non-locomoting values (based on mannually scored video analysis on non lomocoting animals)
roach_solo_vid2 = roach_solo_vid1
roach_solo_vid1$roachcenter_vel[roach_solo_vid1$roachcenter_vel < 14.3]  = NA
roach_solo_vid1$roachcenter_acc[roach_solo_vid1$roachcenter_acc < 13.8]  = NA


# Remove outliers using IGR * coefficient
columns_to_check <- c('roachcenter_acc', 'roachcenter_disttravl', 'roachcenter_vel')
roach_solo_vid2 <- NA_outliers_trial(roach_solo_vid1, columns_to_check, coef = 3)
roach_solo_vid6 <- NA_outliers_temp(roach_solo_vid2, columns_to_check, coef = 6)
roach_solo_vid = roach_solo_vid6

# Get Median and Max per trial
roach_solo_results = roach_solo_vid  %>% 
  filter(keep == T) %>% #95th quantile on mouse and roach motion when not locomoting
  group_by(id) %>%
  dplyr::summarize(date = first(date),
                   id = first(id),
                   arena = first(arena),
                   trial_daily = first(trial_daily),roach_dist_travel = sum(roachcenter_disttravl, na.rm = T)/10,
                   targ_temp = first(targ_temp),
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
#Correct label discrepancy
roach_solo_results$targ_temp[roach_solo_results$targ_temp == 23] = 25

#adjust for missing temp data
roach_solo_results = roach_solo_results %>%
  group_by(targ_temp) %>%
  mutate(mean_temp_cond = mean(roach_solo_results$temp_mean, na.rm = T),
         trial_temp = if_else(is.na(temp_mean), mean_temp_cond , temp_mean),#for missing rasp pi data at room temperature add mean value instead
         trial_temp_plot = if_else(is.na(temp_mean), jitter(mean_temp_cond), temp_mean)) %>% #average temp per thermal condition
  ungroup()

# Get Boltzmann factor
roach_solo_results$one_kT <-  1/(8.617e-5*(roach_solo_results$trial_temp + 273.15)) # In
roach_solo_results$targ_temperature = as.factor(roach_solo_results$targ_temp)

#check averages
unique(roach_solo_results$targ_temp)
mean(roach_solo_results$temp_mean[roach_solo_results$targ_temp == 35], na.rm = T) #35.1
mean(roach_solo_results$temp_mean[roach_solo_results$targ_temp == 30], na.rm = T) #30.1
mean(roach_solo_results$temp_mean[roach_solo_results$targ_temp == 25], na.rm = T) #24.6 
mean(roach_solo_results$temp_mean[roach_solo_results$targ_temp == 18], na.rm = T) #17.6
mean(roach_solo_results$temp_mean[roach_solo_results$targ_temp == 13], na.rm = T) #13.2

# Final version
roach_solo_vid =  roach_solo_vid6

#---------Save-----------
write_csv(roach_solo_vid, file.path(path, "Experim_data", "roach_solo_vid.csv"))
write_csv(roach_solo_vid1, file.path(path, "Experim_data", "roach_solo_vid1.csv"))
write_csv(roach_solo_results, file.path(path, "Experim_data", "roach_solo_results.csv"))

#--------------- Visual Checks -----------------

# Visual inspection over a subset 
p1 = ggplot(roach_solo_vid0[2000000:2100000,], aes(y = roachcenter_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_log10() +
  ggtitle("Original")
  theme_plot()

p2 = ggplot(roach_solo_vid6[2000000:2100000,], aes(y = roachcenter_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_log10() +
  ggtitle("Final")
  theme_plot()
p1/p2

# Visual inspection of distance between front and back or roach  at 35 C - 
ggplot(roach_solo_vid0 %>% filter(targ_temp == 35), aes(y = cockroachfront_cockroachback_dist, x = video_frame)) +
  geom_point(shape = 21) + 
  ggtitle("Original") +
  scale_y_log10(limits = c(.01, 1000), breaks = c(.1, 1, 10, 100, 1000),
                labels = c("0.1", "1", "10",  "100", "1000")) +
  theme_plot()

# Visual inspection of distance between front and back or roach  at 35 C - 
ggplot(roach_solo_vid %>% filter(targ_temp == 35), aes(y = cockroachfront_cockroachback_dist, x = video_frame)) +
  geom_point(shape = 21) + 
  ggtitle("Final") +
  scale_y_log10(limits = c(.01, 1000), breaks = c(.1, 1, 10, 100, 1000),
                labels = c("0.1", "1", "10",  "100", "1000")) +
  theme_plot()


#Visualize Red runner Velocity
ggplot(roach_solo_vid0, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(breaks = c(1,  10,  100, 1000), name = NULL) +
  scale_y_log10(name = "Number of Frames") +
  ggtitle("Original") + 
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

#--------compare outlier fixes, x and y log scale
p0 = ggplot(roach_solo_vid0, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1,  10,  100, 1000, 10000), name = NULL) +
  scale_y_log10(name = "Number of Frames") +
  ggtitle("Original") + theme_no_x +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p1 = ggplot(roach_solo_vid1, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1,  10,  100, 1000, 10000), , name = "Red Runner Velocity (mm/s)") +
  scale_y_log10(name = "Number of Frames") +
  ggtitle("Movement only, no obv outliers")  +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p2 = ggplot(roach_solo_vid2, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = T) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1,  10,  100, 1000, 10000), name = NULL) +
  scale_y_log10(name = "Number of Frames") +
  ggtitle("No Trial Outliers, coef = 3") + theme_no_x +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  + 
  theme_no_y +
  theme(legend.position = c(0.85, 0.65)) + guides(fill = guide_legend(title = "Temp (ºC)"))


p6 = ggplot(roach_solo_vid6, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1,  10,  100, 1000, 10000), name = "Red Runner Velocity (mm/s)") +
  scale_y_log10() +
  ggtitle("No Temp Outliers, coef = 6") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small +
  theme_no_y 

# Compare Plots
 (p0 / p1)  | (p2  /p6)

#----- # Arithemetic y axis
p0 = ggplot(roach_solo_vid0, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 700), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_continuous(limits = c(0, 5000)) +
  ggtitle("Original") + theme_no_x +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p1 = ggplot(roach_solo_vid1, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 700), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_continuous(limits = c(0, 5000)) +
  ggtitle("Movement only")  +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p2 = ggplot(roach_solo_vid2, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 700), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_continuous(limits = c(0, 5000)) +
  ggtitle("No trial outliers, coef = 6") + theme_no_x +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p6 = ggplot(roach_solo_vid6, aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 700), breaks = c(1, 3, 10, 30, 100, 300)) +
  scale_y_continuous(limits = c(0, 5000)) +
  ggtitle("No Temp Outliers, coef = 6") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small

(p0/ p1) | (p2 /  p6)



##################################################################################
################---------  Solo Mouse --------------------------##################
# ---Mouse movement before roach drop
##################################################################################
colnames = c("id","video_frame", "trial_frame", "areanum", "start_time_cap", "end_time_cap", "elapsed_cap", "date",
             "targ_temp", "animal", "trial_daily", "roachcenter_disttravl", "roachcenter_vel" ,"roachcenter_acc",
             "head_roachcenter_dist", "roach_distfromcenter",
             "snout_roachcenter_dist" ,  "spine_disttravl" ,"spine_disttravlcum" ,"spine_vel","spine_acc", 
             "snout_x", "snout_y", "spine_x", "spine_y","roachcenter_x","roachcenter_y" , "temp_mean", "humd_mean", "temp_max",
             "video")
prehunt_vid0 = prehunt_vid00%>% dplyr::select(all_of(colnames))
prehunt_vid0 = prehunt_vid0 %>% rename(arena = areanum)

#Standardize target trial temperature (empirical is recorded)
prehunt_vid0$targ_temp[prehunt_vid0$targ_temp == 23] <- 25
prehunt_vid0$targ_temp[prehunt_vid0$targ_temp == 24] <- 25
prehunt_vid0$targ_temp[prehunt_vid0$targ_temp == 11] <- 14
prehunt_vid0$targ_temp[prehunt_vid0$targ_temp == 12] <- 14
prehunt_vid0$targ_temp[prehunt_vid0$targ_temp == 13] <- 14
prehunt_vid0$targ_temp[prehunt_vid0$targ_temp == 17] <- 18
prehunt_vid0$targ_temp[prehunt_vid0$targ_temp == 34] <- 35
prehunt_vid0$date = as.Date(prehunt_vid0$date, format = "%m-%d-%y")
prehunt_vid0$id = paste(prehunt_vid0$date, prehunt_vid0$arena, prehunt_vid0$trial_daily, sep = "_")
prehunt_vid0$elapsed_sec = prehunt_vid0$trial_frame/30 
prehunt_vid0$targ_temperature = as.factor(prehunt_vid0$targ_temp)

str(prehunt_vid0)

#--------- preliminary removal of outliers
prehunt_vid1 = prehunt_vid0 
# Non-locmotory movement
prehunt_vid1$spine_vel[prehunt_vid1$spine_vel < 76.9 ]  = NA
prehunt_vid1$spine_acc[prehunt_vid1$spine_acc < 31.3]  = NA
reference_column <- c("spine_vel")
columns_to_change <- c("spine_acc", "spine_disttravl", "spine_x", "spine_y") # add column names as needed
prehunt_vid1[is.na(prehunt_vid1[[reference_column]]), columns_to_change ] <- NA

# Remove outliers for mice using IGR * coefficient
columns_to_check_mice <- c('spine_acc', 'spine_disttravl', 'spine_vel')

prehunt_vid3 <- NA_outliers_trial(prehunt_vid1, columns_to_check_mice, coef = 3)
prehunt_vid6 <- NA_outliers_temp(prehunt_vid3, columns_to_check_mice, coef = 6)

prehunt_vid = prehunt_vid6
#############  Prehunt results #########
prehunt_results = prehunt_vid6  %>% 
  group_by(id) %>%
  dplyr::summarize(date = first(date),
                   id = first(id),
                   targ_temp = first(targ_temp),
                   arena = first(arena),
                   animal = first(animal),
                   trial_daily = first(trial_daily),roach_dist_travel = sum(roachcenter_disttravl, na.rm = T)/10,
                   targ_temp = mean(targ_temp),
                   temp_mean = first(temp_mean), 
                   humd_mean = first(humd_mean),
                   mouse_vel_q50 = quantile(spine_vel, probs = 0.50, na.rm = TRUE) / 10,
                   mouse_vel_max = if(all(is.na(spine_vel))) NA_real_ else max(spine_vel, na.rm = TRUE) / 10,
                   mouse_acc_q50 = quantile(spine_acc, probs = 0.50, na.rm = TRUE) / 10,
                   mouse_acc_max = if(all(is.na(spine_acc))) NA_real_ else max(spine_acc, na.rm = TRUE) / 10,
                   mouse_dist_travel = sum(spine_disttravl, na.rm = TRUE) / 10) %>% 
  ungroup()
prehunt_results$targ_temperature = as.factor(prehunt_results$targ_temp)
prehunt_results$targ_temp[prehunt_results$targ_temp == 23] = 25

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

pre_hunt30 = prehunt_results %>% filter(targ_temp <= 30, stage2 == "thermal_trials")
pre_hunt35 = prehunt_results %>% filter(targ_temp <= 35, stage2 == "thermal_trials")

#Save

write_csv(prehunt_vid, file.path(path, "Experim_data", "prehunt_vid.csv"))
write_csv(prehunt_vid1, file.path(path, "Experim_data", "prehunt_vid1.csv"))
write_csv(prehunt_results, file.path(path, "Experim_data",  "prehunt_results.csv"))
write_csv(pre_hunt30, file.path(path,"Experim_data",  "pre_hunt30.csv"))
write_csv(pre_hunt35, file.path(path,"Experim_data",  "pre_hunt35.csv"))



#-------------------- Plot Checks
p1 = ggplot(prehunt_vid0[2000000:2050000,], aes(y = spine_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  ggtitle("all prehunt") +
  scale_y_continuous(limits = c(0, 2000)) +
  theme_plot()

p2 =ggplot(prehunt_vid1[2000000:2050000,], aes(y = spine_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  ggtitle("movement only") +
  scale_y_continuous(limits = c(0, 2000)) +
  theme_plot()


p3 = ggplot(prehunt_vid6[2000000:2050000,], aes(y = spine_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  ggtitle("movement only, no outliers") +
  scale_y_continuous(limits = c(0, 2000)) +
  theme_plot()

p1/ p2 /p3

range(prehunt_vid0$spine_acc, na.rm = T)
range(prehunt_vid1$spine_acc, na.rm = T)
range(prehunt_vid6$spine_acc, na.rm = T)

# Histogram: look at outliers Acceleration
# mouse
p1 = ggplot(prehunt_vid0, aes(x =spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(name = "Mouse Velocity (mm/s)", limits = c(1, 2000), 
                breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10(name = "Count") +
  ggtitle("Original") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  


p2 = ggplot(prehunt_vid1, aes(x =spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(name = "Mouse Velocity (mm/s)", limits = c(1, 2000), 
                breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10(name = "Count") +
  ggtitle("Non-locomoting") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p3 = ggplot(prehunt_vid6, aes(x =spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(name = "Mouse Velocity (mm/s)", limits = c(1, 2000), 
                breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  ggtitle("Final") +
  scale_y_log10(name = "Count") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p1 / p2 / p3



# Compare values with video time
p1 = ggplot(prehunt_vid1[2000000:2200000,], aes(y = spine_acc, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_continuous(name = "Mouse Velocity", limits = c(0 , 2000)) +
  theme_plot()

p2 = ggplot(prehunt_vid6[2000000:2200000,], aes(y = spine_acc, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_continuous(name = "Mouse Velocity", limits = c(0 , 2000)) +
  theme_plot()

p1/p2

#log plot
p1 = ggplot(prehunt_vid0[2000000:2200000,], aes(y = spine_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_log10(limits = c(10, 10000)) +
  theme_plot()

p2 = ggplot(prehunt_vid6[2000000:2200000,], aes(y = spine_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_log10(limits = c(10, 10000)) +
  theme_plot()

p1/p2








#################################################################################################################
#-------------------------------------Hunting Videos ------------------------------------------------------------
#################################################################################################################
hunt_vid00 = hunt_vid00 %>% 
  rename(arena = areanum)

# Remmove uneeded columns
colnames = c("date", "id","video_frame", "trial_frame", "arena", "keep", "roach_g","animal", "trial_daily", 
             "start_time_cap", "end_time_cap", "elapsed_cap", "day_since_first_cap", "days_at_temp", "week_day",
             "targ_temp",  "temp_mean", "humd_mean",  "video",
             "spine_disttravl" ,"spine_disttravlcum" ,"spine_vel","spine_acc", "spine_x", "spine_y", "snout_x", "snout_y",
             "roachcenter_vel", "roachcenter_acc","roachcenter_disttravl" ,  "roachcenter_x", "roachcenter_y", 
             "cockroachfront_cockroachback_dist","cockroachback_disttravl", "cockroachfront_disttravl",
             "snout_roachcenter_dist" )
hunt_vid0  = hunt_vid00 %>% dplyr::select(all_of(colnames))
hunt_vid0$date = as.Date(hunt_vid0$date, format="%m-%d-%y")
hunt_vid0$elapsed_sec = hunt_vid0$trial_frame/30 
hunt_vid0$id = paste(hunt_vid0$date, hunt_vid0$arena, hunt_vid0$trial_daily, sep = "_")
hunt_vid0$id2 = paste(hunt_vid0$date, hunt_vid0$animal, hunt_vid0$trial_daily, sep = "_")

#Standardize Target Temperature Notation
hunt_vid0$targ_temp[hunt_vid0$targ_temp == 11] = 14
hunt_vid0$targ_temp[hunt_vid0$targ_temp == 13] = 14
hunt_vid0$targ_temp[hunt_vid0$targ_temp == 17] = 18
hunt_vid0$targ_temp[hunt_vid0$targ_temp == 23] = 25
hunt_vid0$targ_temp[hunt_vid0$targ_temp == 24] = 25
hunt_vid0$targ_temp[hunt_vid0$targ_temp == 34] = 35

#----Preliminary Screening - Removal of incomplete paired observation and biological implausible values (see also plots below)
hunt_vid1 = hunt_vid0 %>% filter(keep == T) 
hunt_vid1$roachcenter_disttravl[hunt_vid1$roachcenter_disttravl > 30]  = NA #outlier threshold for distance traveled frame to frame
hunt_vid1$roachcenter_disttravl[is.na(hunt_vid1$cockroachfront_disttravl)] = NA
hunt_vid1$roachcenter_disttravl[is.na(hunt_vid1$cockroachback_disttravl)] = NA
hunt_vid1$roachcenter_disttravl[hunt_vid1$cockroachfront_cockroachback_dist > 20] = NA #when points on roach unnaturally separate
hunt_vid1$roachcenter_disttravl[hunt_vid1$roachcenter_disttravl > 30]  = NA 
reference_column <- c("roachcenter_disttravl")
columns_to_change <- c("roachcenter_disttravl","roachcenter_acc","roachcenter_vel",  "roachcenter_x", "roachcenter_y", "cockroachfront_cockroachback_dist") # add column names as needed
hunt_vid1[is.na(hunt_vid1[[reference_column]]), columns_to_change ] <- NA


#------ Get rid of Non-locomoting Values
hunt_vid2 = hunt_vid1
hunt_vid2$roachcenter_vel[hunt_vid2$roachcenter_vel < 14.3]  = NA
hunt_vid2$roachcenter_acc[hunt_vid2$roachcenter_acc < 13.8]  = NA
hunt_vid2$spine_vel[hunt_vid1$spine_vel < 76.9 ]  = NA
hunt_vid2$spine_vel[hunt_vid1$spine_acc < 31.3]  = NA
reference_column <- c("spine_vel")
columns_to_change <- c("spine_acc", "spine_disttravl") # add column names as needed
hunt_vid2[is.na(hunt_vid2[[reference_column]]), columns_to_change ] <- NA

#-------------- Remove outliers for mice and roaches using IGR * coefficient
columns_to_check_roaches <- c('roachcenter_vel', "roachcenter_disttravl","roachcenter_acc","roachcenter_vel",  "roachcenter_x", "roachcenter_y")
columns_to_check_mice <- c('spine_vel', 'spine_acc', 'spine_disttravl',  "snout_x", "snout_y")

hunt_vid3 <- NA_outliers_trial(hunt_vid2, columns_to_check_roaches, coef = 3)
hunt_vid3 <- NA_outliers_trial(hunt_vid3, columns_to_check_mice, coef = 3)

hunt_vid6 <- NA_outliers_temp(hunt_vid3, columns_to_check_roaches, coef = 6)
hunt_vid6 <- NA_outliers_temp(hunt_vid6, columns_to_check_mice, coef = 6)

# Final Version
hunt_vid = hunt_vid6 %>%
  group_by(id) %>%
  mutate(is_contact = is_contact(roachcenter_x, n = 10)) %>% # at least 10 consecutive frame with roach obscured to count as a contact
  mutate(n_contact = n_contact(roachcenter_x, n = 10)) %>% # sum of is_contacts per trial
  ungroup()

# Add Time to Capture (elapsed seconds after red runner is dropped into arena until incapacitated)
hunt_vid$elapsed_s = as.numeric(hunt_vid$elapsed_cap) 


###################### Add Strikes and get per trial median and max values #############
hunt_results0 = hunt_vid %>% 
  group_by(id) %>%
  filter(keep == T) %>% #roach must travel at least 1 cm
  dplyr::summarize(date = first(date),
                   id = first(id),
                   animal = first(animal),
                   day_since_first_cap = first(day_since_first_cap),
                   week_day = first(week_day),
                   days_at_temp = first(days_at_temp),
                   trial_daily = first(trial_daily),roach_dist_travel = sum(roachcenter_disttravl, na.rm = T)/10,
                   targ_temp = mean(targ_temp),
                   elapsed_s = first(elapsed_s),
                   roach_dist_travel = sum(roachcenter_disttravl, na.rm = TRUE) / 10,
                   roach_acc_q50 = quantile(roachcenter_acc, probs = 0.50, na.rm = T)/10,
                   roach_acc_max = if(all(is.na(roachcenter_acc))) NA else max(roachcenter_acc, na.rm = TRUE)/10,
                   roach_vel_q50 = quantile(roachcenter_vel, probs = 0.5, na.rm = T)/10,
                   roach_vel_max = if(all(is.na(roachcenter_vel))) NA else max(roachcenter_vel, na.rm = TRUE)/10,
                   roach_dist_travel = sum(roachcenter_disttravl, na.rm = T)/10,
                   snout_roach_dist_q50 = quantile(snout_roachcenter_dist, probs = 0.5, na.rm = TRUE) / 10,
                   strikes_n = if(all(is.na(n_contact))) NA else max(n_contact, na.rm = TRUE),
                   mouse_vel_q50 = quantile(spine_vel, probs = 0.50, na.rm = TRUE) / 10,
                   mouse_vel_max = if(all(is.na(spine_vel))) NA_real_ else max(spine_vel, na.rm = TRUE) / 10,
                   mouse_acc_q50 = quantile(spine_acc, probs = 0.50, na.rm = TRUE) / 10,
                   mouse_acc_max = if(all(is.na(spine_acc))) NA_real_ else max(spine_acc, na.rm = TRUE) / 10,
                   mouse_dist_travel = sum(spine_disttravl, na.rm = TRUE) / 10,
                   arena = first(arena),
                   frame_count_roach = sum(is.finite(roachcenter_x)), 
                   frame_count_mouse = sum(is.finite(spine_x)),
                   move_count_roach = sum(is.finite(roachcenter_vel)), 
                   move_count_mouse = sum(is.finite(spine_vel)),
                   temp_mean = first(temp_mean), 
                   humd_mean = first(humd_mean),
                   roach_g = first(roach_g),
                   days_at_temp = first(days_at_temp), 
                   keep = first(keep)) %>%
  ungroup()

# Remove infinite values
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

# Fill missing measured temperatures using the group mean, and create a jittered version for plotting
hunt_results0 <- hunt_results0 %>%
  group_by(targ_temp) %>%
  mutate(mean_temp_cond = mean(temp_mean, na.rm = TRUE),
         trial_temp = if_else(is.na(temp_mean), mean_temp_cond, temp_mean), 
         trial_temp_plot = if_else(is.na(temp_mean), jitter(mean_temp_cond), temp_mean)
  ) %>%
  ungroup()

hunt_results0$one_kT <-  1/(8.617e-5*(hunt_results0$trial_temp + 273.15)) # In


#-----at least 10 continuous frames for locomotion analysis, otherwise NA
columns_to_change <- c("roach_dist_travel","roach_acc_q50","roach_acc_max" ,
                       "roach_vel_q50", "roach_vel_max") # add column names as needed
hunt_results1 = hunt_results0 %>%
  mutate(across(all_of(columns_to_change), ~if_else(move_count_roach < 10, NA_real_, .)))


#check averages

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
hunt_results1$animal[hunt_results1$arena == "1A"] = "jmg1"
hunt_results1$animal[hunt_results1$arena == "1B"] = "jmg2"
hunt_results1$animal[hunt_results1$arena == "1C"] = "jmg3"
hunt_results1$animal[hunt_results1$arena == "1D"] = "jmg4"
hunt_results1$animal[hunt_results1$arena == "2A"] = "jmg5"
hunt_results1$animal[hunt_results1$arena == "2B"] = "jmg6"
hunt_results1$animal[hunt_results1$arena == "2C"] = "jmg7"
hunt_results1$animal[hunt_results1$arena == "2D"] = "jmg8"

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

hunt_results1 <- hunt_results1 %>%
  mutate(
    chamber = case_when(
      animal %in% c("jmg1", "jmg2", "jmg3", "jmg4") ~ "A",
      animal %in% c("jmg5", "jmg6", "jmg7", "jmg8") ~ "B",
      TRUE ~ NA_character_
    )
  )


##--------add mouse weight 
mouse_weight$date_animal = paste(mouse_weight$date, mouse_weight$animal, sep = "_")
mouse_weight2 = mouse_weight %>% dplyr::select(mass_g, date_animal) %>%
  rename(mouse_g = mass_g)

# Join Data
hunt_results1$date_animal =  paste(hunt_results1$date, hunt_results1$animal, sep = "_")
hunt_results2 = hunt_results1 %>% left_join(mouse_weight2, by = "date_animal")
hunt_results2$date_animal = NULL

#-------- Add trials at temperature ---

# add to results
hunt_results3 = hunt_results2 %>% 
  left_join(trial_at_temp, by = "id2") %>%
  dplyr::select(date, stage, targ_temp, trial_temp, animal,trial_daily, trial_at_temp, trial_at_temp_init, everything()) %>%
  arrange(date, animal, trial_daily)
range(hunt_results3$trial_temp, na.rm = T)

#----add Metadata and time to capture

# Excluded data percentage
keep_counts <- meta %>%
  filter(!is.na(keep)) %>%
  count(keep) %>%
  mutate(percentage = n / sum(n) * 100)

print(keep_counts)

# percent excluded in hot trials
keep_counts_hot <- meta %>%
  filter(!is.na(keep)) %>%
  filter(targ_temp == 35) %>%
  count(keep) %>%
  mutate(percentage = n / sum(n) * 100)
keep_counts_hot

meta_short = meta %>% dplyr::select(id, mismatch, mismatch_trial, vid_review, elapsed_cap, person_scorer, person_trial)

#--- Join metadata
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

#-------------------Save ----------------
write_csv(hunt_vid0, file.path(path, "Experim_data", "hunt_vid0.csv"))
write_csv(hunt_vid, file.path(path, "Experim_data", "hunt_vid.csv"))
write_csv(hunt_vid1, file.path(path, "Experim_data", "hunt_vid1.csv"))
write_csv(hunt_results, file.path(path, "Experim_data", "hunt_results.csv"))
write_csv(hunt_30, file.path(path, "Experim_data", "hunt_30.csv"))
write_csv(hunt_35, file.path(path, "Experim_data", "hunt_35.csv"))



################################################
#----------- Visualize -------------------
################################################

#------ Movement Paths

# Original Animal paths
p1 = ggplot(data = hunt_vid0 %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("Original \n jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")


# Final Animal Paths
p2 = ggplot(data = hunt_vid %>% filter(animal == "jmg1", trial_daily ==  3, date == "2022-02-21")) +
  theme_plot() + theme(aspect.ratio = 1) +
  ggtitle("Final Dataset \n jmg1, trial 2, 24 C, 2022-02-21") +
  scale_y_continuous((name = "Y Coordinates (cm)"), breaks = c(5, 15, 25), limits = c(1, 28)) +
  scale_x_continuous((name = "X Coordinates (cm)"), breaks = c(10, 20, 30), limits = c(6, 33), labels = c("5", "15", "25")) +
  geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
  geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "#0066FF")  # arrow = arrow(angle = 15, ends = "both", type = "closed")

p1 |p2 

#-----------Locomotion Over Video Frames" ----

# Before and after raoch velocity
p1 = ggplot(hunt_vid0[2000000:2100000,], aes(y = roachcenter_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  ggtitle("Original") + 
  scale_y_continuous() +
  theme_plot()

p2 = ggplot(hunt_vid[2000000:2100000,], aes(y = roachcenter_vel, x = video_frame)) +
  geom_point(shape = 21) + 
  ggtitle("Final") + 
  scale_y_continuous() +
  theme_plot()

p1| p2

# Before and after roach acceleration
p1 = ggplot(hunt_vid0[2000000:2100000,], aes(y = roachcenter_acc, x = video_frame)) +
  geom_point(shape = 21) + 
  ggtitle("Original") + 
  scale_y_continuous() +
  theme_plot()

p2 = ggplot(hunt_vid[2000000:2100000,], aes(y = roachcenter_acc, x = video_frame)) +
  geom_point(shape = 21) + 
  scale_y_continuous() +
  ggtitle("Final") + 
  theme_plot()
p1| p2
# Before and after raoch distance traveled
p1 = ggplot(hunt_vid0[2000000:2100000,], aes(y = roachcenter_disttravl, x = video_frame)) +
  geom_point(shape = 21) + 
  ggtitle("Original") + 
  scale_y_continuous() +
  theme_plot()

p2 =ggplot(hunt_vid[2000000:2100000,], aes(y = roachcenter_disttravl, x = video_frame)) +
  geom_point(shape = 21) + 
  ggtitle("Final") + 
  scale_y_continuous() +
  theme_plot()
p1| p2

# Before and after distance from roach front to back
p1 =ggplot(hunt_vid0[2000000:2100000,], aes(y = cockroachfront_cockroachback_dist, x = video_frame)) +
  geom_point(shape = 21) + 
  ggtitle("Original") + 
  theme_plot() + 
  scale_y_continuous(breaks = c(0, 20, 100, 200), name = "Front to Back Distance")


p2 =ggplot(hunt_vid[2000000:2100000,], aes(y = cockroachfront_cockroachback_dist, x = video_frame)) +
  geom_point(shape = 21) + 
  ggtitle("Final") + 
  theme_plot() + 
  scale_y_continuous(breaks = c(0, 20, 100, 200), name = "Front to Back Distance")
p1| p2


#------ Distribution of Data

#--------ROACH  - compare outlier fixes, x and y log scale #)
p0 = ggplot(hunt_vid0 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("Original Red Runner Velocity") + theme_no_x +
  scale_fill_manual(values = temp_scale) +
  theme_plot()  

p1 = ggplot(hunt_vid1 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04" ), aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  scale_y_log10() +
  ggtitle("Move only, Preliminary Corrections") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p2 = ggplot(hunt_vid2 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("No Trial Outliers") + theme_no_x +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p3 = ggplot(hunt_vid6 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = roachcenter_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  scale_y_log10("Roach Velocity Count") +
  ggtitle("No Trial or Temp outliers") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small


#combined_plot <- p0/ p1/ p2/ p3/ p6
combined_plot <- (p0 / p1) | (p2  /p3)
combined_plot


#-------- MOUSE compare outlier fixes, x and y log scale #)
p0 = ggplot(hunt_vid0 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("Original Mouse Velocity in Hunt") + theme_no_x +
  scale_fill_manual(values = temp_scale) +
  theme_plot()  

p1 = ggplot(hunt_vid1 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04" ), aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(10, 10000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  scale_y_log10() +
  ggtitle("Move only, Preliminary Corrections") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p2 = ggplot(hunt_vid2 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(70, 3000), breaks = c(1, 3, 10, 30, 100, 300), name = NULL) +
  scale_y_log10() +
  ggtitle("No Trial Outlier") + theme_no_x +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  


p3 = ggplot(hunt_vid6 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-04") , aes(x = spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(limits = c(70, 3000), breaks = c(1, 3, 10, 30, 100, 300, 1000, 3000)) +
  scale_y_log10() +
  ggtitle("No Trial or Temp Outliers") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small


#combined_plot <- p0/ p1/ p2/ p3/ p6
combined_plot <- (p0 / p1) | (p2 / p3)
combined_plot

#More comparisons
p1 = ggplot(hunt_vid0, aes(x = roachcenter_disttravl, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10("Roach Distance Traveled per Frame Count") +
  ggtitle("Initial Dataset") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p2 = ggplot(hunt_vid, aes(x = roachcenter_disttravl, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), 
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10("Roach Distance Traveled per Frame Count") +
  ggtitle("Final Dataset") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  
p1| p2

# ------Mouse Distribution of velocity
#- Fewer recognition errors to correct Due to larger size and more distinctive shape 
# In final dataset non-locomotory movement is exluded (e.g. grooming)
p1 = ggplot(hunt_vid0, aes(x =spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), name = "Velocity (mm/s)",
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10(name = "Mouse Velocity Count") +
  ggtitle("Original") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  

p2 = ggplot(hunt_vid, aes(x =spine_vel, fill = as.factor(targ_temp))) +
  geom_histogram(position = "stack", bins = 500, show.legend = FALSE) +
  scale_x_log10(breaks = c(.0001, .001, .01, .1, 1, 10, 100, 1000, 10000), name = "Velocity (mm/s)",
                labels = c(".0001", ".001", ".01", ".1", '1', "10", "100", "1000", "10000")) +
  scale_y_log10(name = "Mouse Velocity Count") +
  ggtitle("Final") +
  scale_fill_manual(values = temp_scale) +
  theme_plot_small  
p1 | p2


# Original Acceleration q50
p0 = ggplot(data = hunt_results0 %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-18"), 
       aes(x = trial_temp, y = roach_acc_q50)) +
  theme_plot() + 
  scale_y_log10() + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Roach Acceleration q50 old") +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 

# Accerelation q50, minimum 10 frames
p1 = ggplot(data = hunt_results %>% filter(keep == T, date >= "2022-01-24", date <= "2022-03-18"), 
       aes(x = trial_temp, y = roach_acc_q50)) +
  theme_plot() + 
  scale_y_log10(limits = c(1, 30)) + 
  scale_x_continuous(limits = c(10, 40)) +
  ggtitle("Roach Acceleration q50") +
  geom_point(aes(x = temp_mean, fill = as.factor(targ_temp)), shape = 21, size = 2.5,  show.legend = F) +
  geom_jitter(data = . %>% filter(stage2 == "thermal_trials", is.na(temp_mean)),
              aes(x = targ_temp, fill = as.factor(targ_temp)), shape = 21, size = 2.5, show.legend = F) +
  scale_color_manual(values = temp_col) +
  scale_fill_manual(values = temp_scale) +
  stat_smooth(data = . %>% filter(targ_temp <=30),  color = "black", method = "lm", se= F) +
  stat_smooth(color = "black", linetype = "dashed", se = F) 

p0 | p1






