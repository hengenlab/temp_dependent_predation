#--------- Load data files into R-----------

library(tidyverse)

# ------------------- Load Paths and Custom Functions ---------------------
# Update paths
path <- "/Users/jgradym/Desktop/Predation_Data"


##################### Read in data for analysis ####################

meta              <- read_csv(file.path(path, "Experim_data", "metadata.csv"))
hunt_results      <- read_csv(file.path(path, "Experim_data", "hunt_results.csv"))
hunt_30           <- read_csv(file.path(path, "Experim_data", "hunt_30.csv"))
hunt_35           <- read_csv(file.path(path, "Experim_data", "hunt_35.csv"))
hunt_vid0          <- read_csv(file.path(path, "Experim_data", "hunt_vid0.csv"))
hunt_vid          <- read_csv(file.path(path, "Experim_data", "hunt_vid.csv"))
hunt_vid1         <- read_csv(file.path(path, "Experim_data", "hunt_vid1.csv"))
prehunt_results   <- read_csv(file.path(path, "Experim_data", "prehunt_results.csv"))
prehunt_vid       <- read_csv(file.path(path, "Experim_data", "prehunt_vid.csv"))
prehunt_vid1      <- read_csv(file.path(path, "Experim_data", "prehunt_vid1.csv"))
roach_solo_vid    <- read_csv(file.path(path, "Experim_data", "roach_solo_vid.csv"))
roach_solo_vid1   <- read_csv(file.path(path, "Experim_data", "roach_solo_vid1.csv"))
roach_solo_results <- read_csv(file.path(path, "Experim_data", "roach_solo_results.csv"))
roach_solo_30 = roach_solo_results %>% filter(targ_temp <= 30)
pre_hunt30 = prehunt_results %>% filter(targ_temp <= 30, stage2 == "thermal_trials")
pre_hunt35 = prehunt_results %>% filter(targ_temp <= 35, stage2 == "thermal_trials")

# Save a version of target trial temperature as a factor
target_dfs = c("hunt_results", "hunt_30", "hunt_35", "hunt_vid0", "hunt_vid", 
               "hunt_vid1", "prehunt_results", "prehunt_vid", "prehunt_vid1", 
               "roach_solo_vid", "roach_solo_vid1", "roach_solo_results")

for (df_name in c("meta", "hunt_results", "hunt_30", "hunt_35", "hunt_vid0", "hunt_vid", "hunt_vid1", "prehunt_results", "prehunt_vid", "prehunt_vid1", "roach_solo_vid", "roach_solo_vid1", "roach_solo_results", "roach_solo_30", "pre_hunt30", "pre_hunt35")) {
  df = get(df_name)
  if (is.data.frame(df)) {
    # Convert to factor if targ_temperature exists
    if ("targ_temperature" %in% names(df)) {
      df$targ_temperature <- as.factor(df$targ_temperature)
    }
    # Remove spec attribute if present
    if (!is.null(attr(df, "spec"))) {
      attr(df, "spec") <- NULL
    }
    # Reassign modified object back to global env
    assign(df_name, df)
  }
}

save.image(file.path(path, "Experim_data", "capture_data.RData"))

