library(tidyverse)
library(googledrive)
library(googlesheets4)
################# March 6, 2024  Mouse Capture Data to load #######

hlab_path  <- ifelse(Sys.info()['user'] == 'jgradym', '/Volumes/HlabShare/Jacob_work',
                     file.path('O:\\Jacob_work'))
save_path  <- ifelse(Sys.info()['user'] == 'jgradym', '/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/mouse/Figures',
                     file.path('G:\\.shortcut-targets-by-id\\1Ece7oSFJTTDHja3uVWJaECrkbNea_bqm\\mouse\\Figures'))

save_path2 <- ifelse(Sys.info()['user'] == 'jgradym', "/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My\ Drive/mouse/Figures/",
                     file.path('G:\\.shortcut-targets-by-id\\1Ece7oSFJTTDHja3uVWJaECrkbNea_bqm\\mouse\\Figures'))

save_path2 <- ifelse(Sys.info()['user'] == 'jgradym', "/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My\\ Drive/mouse/Figures/",
                     file.path('G:\\.shortcut-targets-by-id\\1Ece7oSFJTTDHja3uVWJaECrkbNea_bqm\\mouse\\Figures'))

##################### Read in Functions #################
source("capture_plot_fun.R")


##################### Read in data ####################
#---- Hunt Data
#----------- add meta code to get capture times ---------
cap_time_path <- googledrive::drive_get("capture_times")
2
meta <- read_sheet(ss = cap_time_path)

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
meta_short = meta %>% 
  dplyr::select(id, trial_tot)
#quick look at temperature
#ggplot(meta, aes(x = targ_temp)) + geom_histogram() + theme_plot()

hunt_results = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/hunt_results_jg.csv")
hunt_results = read_csv("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Mouse/Data/hunt_results_jg.csv")
hunt_results = hunt_results %>% left_join(meta_short, by = "id")
hunt_results$targ_temperature = as.factor(hunt_results$targ_temp)
hunt_all = hunt_results %>% filter(keep == T, mismatch_trial == F)  #but already filtered
hunt_35 = hunt_all %>% filter(targ_temp <= 35, stage2 == "thermal_trials")
hunt_30 = hunt_all %>% filter(targ_temp <= 30, stage2 == "thermal_trials")
hunt_25 = hunt_all %>% filter(targ_temp <= 25, stage2 == "thermal_trials")
thermal_trials = hunt_results %>% filter(stage2 == "thermal_trials")
training_trials = hunt_results %>% filter(stage2 == "initial_trials")

hunt_vid = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/hunt_vid_jg.csv")
hunt_vid1 = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/hunt_vid1_jg.csv")

hunt_vid = read_csv("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Mouse/Data/hunt_vid_jg.csv")
hunt_vid1 = read_csv("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Mouse/Data/hunt_vid1_jg.csv")

#hunt_vid1$elapsed_sec[1:10]


#---- Pre-Hunt Mouse Data
prehunt_results = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/prehunt_results_jg.csv")
prehunt_results$targ_temperature = as.factor(prehunt_results$targ_temp)
pre_35 = prehunt_results %>% filter(targ_temp <= 35, stage2 == "thermal_trials")
pre_30 = prehunt_results %>% filter(targ_temp <= 30, stage2 == "thermal_trials")
pre_25 =prehunt_results %>% filter(targ_temp <= 25, stage2 == "thermal_trials")


prehunt_vid = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/prehunt_vid_jg.csv")
prehunt_vid1 = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/prehunt_vid1_jg.csv")
#prehunt_vid1$elapsed_sec[1:10]


#---- Solo Roach Movement Data
roach_solo_vid = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/roach_solo_vid_jg.csv")
roach_solo_vid1 = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/roach_solo_vid1_jg.csv")
#roach_solo_vid$elapsed_sec
roach_solo_results = read_csv("/Volumes/HlabShare/Jacob_work/Prey_Capture--Data_and_Methods/DLC_Data_and_Analysis/saDLC_out_analysis/final/roach_solo_results_jg.csv")
roach_solo_results$targ_temperature = as.factor(roach_solo_results$targ_temp)
roach_solo_30 = roach_solo_results %>% filter(targ_temp <= 30)
save.image(file = "/Volumes/HlabShare/Gradys_project/capture_data.RData") 
