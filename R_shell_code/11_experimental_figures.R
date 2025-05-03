library(tidyverse)
library(patchwork)
library(ggsignif)
library(lubridate) 
library(data.table)


#------ Relative File paths - UPDATE -----------
path <- "/Users/jgradym/Desktop/Predation_Data"
github_path = "/Users/jgradym/Documents/GitHub/mouse_capture"

########### Load Data and Functions ###########
load(file.path(path, "Experim_data", "capture_data.RData"))
source(file.path(github_path, "7_experim_functions.R"))

##################################################
#########  Figure 3 Hunting Prey ######
##################################################

hunt_vid_exampleA = hunt_vid0 %>% 
  filter(animal == "jmg4", trial_daily == 2, date == "2022-03-08") #original data
hunt_vid_exampleB = hunt_vid %>% 
  filter(animal == "jmg4", trial_daily == 2, date == "2022-03-08") #includes contacts

#------ Fig 3A: Coordinates of a hunt
(coord <- ggplot(data = hunt_vid_exampleA) +
   theme_plot() + theme(aspect.ratio = 1) +
   scale_y_continuous(name = "Y Coordinates (cm)") + #, labels = c("5", "15", "25")) +#, breaks = c(5, 15, 25), limits = c(1, 28)) +
   scale_x_continuous(name = "X Coordinates (cm)") +#, labels = c("5", "15", "25")) +#, breaks = c(10, 20, 30), limits = c(14, 41)) +
   geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
   geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "cornflowerblue")) 

#------ Fig 3B: Distances in a hunt
(dist_con  = ggplot(data = hunt_vid_exampleA,
                    aes(x = elapsed_sec, y = snout_roachcenter_dist/10)) +
    theme_plot() + 
    geom_point() +
    geom_point(data = hunt_vid_exampleB %>% filter(is_contact == 1),
               aes(y = 0),color = "gold2", size = 3, shape = "|") +
    geom_line() + 
    scale_y_continuous(breaks = c(0, 3, 6, 9),
                       name = "Distance to Roach (cm)") +
    scale_x_continuous(breaks = c(0, 5, 10), 
                       name = "Time (s)"))
#------ Fig 3C: Speed of a hunt
(speed = ggplot(hunt_vid_exampleA,
                aes(x = elapsed_sec)) +
    theme_plot() + 
    geom_point(data = hunt_vid_exampleB %>% filter(is_contact == 1),
              aes(y = 0),color = "gold2", size = 3, shape = "|") +
    geom_line(aes(y = spine_vel/10), color = "brown3") + 
    geom_line(aes(y =  roachcenter_vel/10), color = "cornflowerblue") + 
    scale_y_continuous(name = "Speed (cm/s)") +
    scale_x_continuous(breaks = c(0, 5, 10), name = "Time (s)"))

#------ Fig 3D: Learning to hunt
# geometric mean function
geo_mean = function(x) {
  exp(mean(log(x), na.rm = TRUE))
}
ggplot(hunt_results %>% filter(keep == T, stage == "initial_trials", day_since_first_cap <= 6),
       aes(x = day_since_first_cap, y = elapsed_s)) +
  stat_summary(fun = geo_mean, geom = "line", aes(group = 1), size = 1.2) +  # <--- Adds line connecting medians
  geom_boxplot(aes(group = day_since_first_cap), fill = "white", alpha = 1, outlier.shape = NA, lwd = 0.5) +
  geom_jitter(shape = 21, width = 0.1, fill = "gray70", size = 2) + 
  geom_boxplot(aes(group = day_since_first_cap), fill = "white", alpha = 0.5, outlier.shape = NA) +
  scale_y_log10(name = "Time to Capture (s)", breaks = c(10, 100, 1000, 10000)) +
  scale_x_continuous(breaks =1:6, name = "Training Day") +
  theme_plot()


#########################################################################
####### Figure 4: Solitary, Pursuit, Capature Speeds ###################
#########################################################################


#---- Fig 4B: Solitary Mouse median speed

prehunt2 = prehunt_results %>% filter(stage2 == "thermal_trials")
range(prehunt2$mouse_vel_q50, na.rm = T) # 8.00235 20.19782
speed_limits = c(1, 30)
speed_breaks  = c(1, 3, 10, 30)
speed_labels  = c("1", "3", "10", "30")
speed_name = expression(paste("Median Speed (cm s"^-1, ")"))

df = prehunt2 %>% filter(!is.na(mouse_vel_q50) & !is.na(trial_temp_plot) & !is.na(targ_temp))
newdata = temp_fun(df = df, response = df$mouse_vel_q50, roach_g = F) #Function includes mixed model
(p = plot_temp_col(df = prehunt2, response = prehunt2$mouse_vel_q50, col = endo_col, add_smooth = T, y_name = speed_name, breaks = speed_breaks, limits = speed_limits, labels = speed_labels))

#---- Fig 4C: Solitary Roach median speed -
# no need for mixed model, each red runner used only once
(p = ggplot(data = roach_solo_results) +
   theme_plot() + 
   labs(color = "Temperature") +
   theme(aspect.ratio = 0.8) +
   geom_point(aes(x = trial_temp_plot,  y = roach_vel_q50, fill = targ_temperature),  
              shape = 21, size = 2.5, color = "black", stroke = .4, show.legend = F) +
   scale_fill_manual(values = temp_scale2) +
   scale_x_continuous(name = "Ambient Temperature (ºC)", limits = temp_limits, breaks = temp_breaks) + 
   stat_smooth(data = roach_solo_results %>% filter(targ_temp <= 30), 
               aes(x = temp_mean, y = roach_vel_q50), size = 1.5,
               method = "lm", color = ecto_col, fill = ecto_col, alpha = 0.3, show.legend = F) +
   stat_smooth(aes(x = temp_mean, y = roach_vel_q50),size = 1.5, linetype = "dashed",
               color = ecto_col, fill = ecto_col, se = F,  show.legend = F) +
   scale_y_log10(name = speed_name, limits = speed_limits, speed_breaks, labels = speed_labels)
)


#------Fig 4E-F
range(hunt_35$roach_vel_q50, na.rm = T) #1.6, 35
range(hunt_35$mouse_vel_q50, na.rm = T) #8.1, 28
speed_limits = c(1, 50)
speed_breaks  = c(1, 3, 10, 30)
speed_labels  = c("1", "3", "10", "30")
speed_name = expression(paste("Median Speed (cm s"^-1, ")"))

#---Fig 4E: Mouse Median  Speed
df = hunt_30 %>% filter(!is.na(mouse_vel_q50))
newdata = temp_fun(df = df, response = df$mouse_vel_q50, roach_g = F)
(p = plot_temp_col(df = hunt_35, response = hunt_35$mouse_vel_q50, col = endo_col, add_smooth = T, y_name = speed_name, breaks = speed_breaks, limits = speed_limits, labels = speed_labels))

#-- Fig 4F Roach Median Speed
df = hunt_30 %>% filter(!is.na(roach_vel_q50))
newdata = temp_fun(df = df, response = df$roach_vel_q50)

(p = plot_temp_col(df = hunt_35, response = hunt_35$roach_vel_q50, col = ecto_col, add_smooth = T, y_name = speed_name,  breaks = speed_breaks, limits = speed_limits, labels = speed_labels))
#save_fun(p = p,'Fig4/speed/roach_speed_q50_col.pdf')

# Fig 4H: Strikes per Capture
range(hunt_35$strikes_n, na.rm = T)
strike_name = expression(paste("Strikes per Capture"))
strike_limits = c(1, 40)
strike_breaks  = c( 1,  3, 10, 30)
strike_labels  = c("1",  '3', '10', '30')
str(hunt_35)
hunt_30_strike = hunt_30 %>% filter(!is.na(strikes_n))

newdata = temp_fun(df = hunt_30_strike, response = hunt_30_strike$strikes_n)
(p = plot_temp_col(df = hunt_35, response = hunt_35$strikes_n, y_name = strike_name, breaks = strike_breaks,limits = strike_limits, labels = strike_labels, col = "black"))

#save_fun(p = p, 'Fig4/strikes/strike_capture.pdf')

#-- Fig 4H: Time to Capture
range(hunt_35$elapsed_s, na.rm = T)#  1 258
time_name = expression(paste("Time to Capture (s)"))
time_limits = c(1, 300)
time_breaks  =c(1, 3, 10, 30, 100, 300)
time_labels  = c("1", "3", "10", "30", "100", '300')

hunt_30_time = hunt_30 %>% filter(!is.na(elapsed_s))
range(hunt_30_time$date)
newdata = temp_fun(df = hunt_30_time, response = hunt_30_time$elapsed_s)
(p = plot_temp_col(df = hunt_35, response = hunt_35$elapsed_s, y_name = time_name, breaks = time_breaks, limits = time_limits, labels = time_labels, col = "black"))


#-- Fig 4I: Mice strikes per capture 
range(hunt_35$strikes_n, na.rm = T)
strike_name = expression(paste("Strikes per Capture"))
strike_limits = c(1, 40)
strike_breaks  = c( 1,  3, 10, 30)
strike_labels  = c("1",  '3', '10', '30')
str(hunt_35)
hunt_30_strike = hunt_30 %>% filter(!is.na(strikes_n))
newdata = temp_fun(df = hunt_30_strike, response = hunt_30_strike$strikes_n)

(p = plot_temp_col(df = hunt_35, response = hunt_35$strikes_n, y_name = strike_name, breaks = strike_breaks,  limits = strike_limits, labels = strike_labels, col = "black"))



################################################
# ----------- Figure 5: Pursue, Strike, Capture
################################################

initial_trials  = hunt_results %>% filter(stage == "initial_trials")
thermal_trials = hunt_results %>% filter(stage2 == "thermal_trials")
trial_name = "Trial at Temperature"
range(thermal_trials$trial_at_temp)
t_breaks = c(0, 20, 40, 60)
t_labels = c("0","20",  '40', '60')


#---Fig 5A: Distance Between Mouse and Roach, Initial Trials
range(initial_trials$snout_roach_dist_q50, na.rm = T) #1.320354 18.153057 #use to define limits
range(thermal_trials$snout_roach_dist_q50, na.rm = T)#0.5237994 24.7735426
thermal_trials$id2[thermal_trials$snout_roach_dist_q50 < 0.053]

# Plot arguments
dist_limits = c(0.5, 30)
dist_breaks = c(1, 3, 10,30)
dist_labels =  c("1",  "3","10",  "30")
dist_name =  expression(paste("Predator-Prey Distance (cm)"))
dist_name = expression(atop("Predator-Prey", "Distance (cm)"))

#
df = initial_trials %>% filter(snout_roach_dist_q50 > 0)
newdata_init = trial_init_fun(df = df, response = df$snout_roach_dist_q50)
(p1a = plot_trial_init(ratio = 0.5, df = df, x_name = NULL, x_labels = NULL, newdata = newdata_init, 
                       response = df$snout_roach_dist_q50, y_name = dist_name,
                       y_breaks = dist_breaks, y_labels  = dist_labels, y_limits = dist_limits))

#save_fun(p = p1a, 'Fig5/dist_between/distance_between_init.pdf')

#--- Fig 5B: Distance Between Mouse and Roach, Thermal Trials
df = thermal_trials %>% filter(snout_roach_dist_q50 > 0)
newdata_init = trial_temp_fun(df = df, response = df$snout_roach_dist_q50)
(p1b = plot_trial_temp(ratio = 0.5,df = df, newdata = newdata_init, response = df$snout_roach_dist_q50, y_name = NULL, y_breaks = NULL, x_name = NULL, x_labels = NULL, y_limits = dist_limits, y_labels = NULL ))
#save_fun(p = p1b, 'Fig5/dist_between/distance_between.pdf')


#---Fig 5C: Initial Speed 
range(initial_trials$mouse_vel_q50, na.rm = T) #[1] 11.20352 25.30692
range(thermal_trials$mouse_vel_q50, na.rm = T) #[1]  8.151703 28.173332

speed_name = expression(paste("Speed (cm s"^-1, ")"))
vel_lim = c(8, 32)
vel_breaks = c(8, 16, 32)
vel_labs = c("8", "16", "32")

df =  initial_trials %>% filter(mouse_vel_q50 > 0, !is.na(mouse_vel_q50))
newdata = trial_init_fun(df = df, response = df$mouse_vel_q50)
(p2a = plot_trial_init(ratio = .5, df = df, newdata = newdata, response = df$mouse_vel_q50,  y_name = speed_name, x_name = NULL, x_labels = NULL, y_limits = vel_lim, y_breaks = vel_breaks, y_labels = vel_labs))

#save_fun(p = p2a, 'Fig5/speed/mouse_velocity_q50_init.pdf')

#---Fig 5D: Speed over thermal trials
df = thermal_trials %>% filter(mouse_vel_q50 > 0, !is.na(mouse_vel_q50))
newdata = trial_temp_fun(df = df, response = df$mouse_vel_q50)
(p2b = plot_trial_temp(ratio = 0.5, df = df, newdata = newdata, response = df$mouse_vel_q50,  x_name = NULL, y_name = NULL, x_labels = NULL, y_limits = vel_lim, y_breaks = NULL,y_labels = NULL))
#save_fun(p = p2b, 'Fig5/speed/mouse_velocity_q50.pdf')

#--------
#-- Fig 5E: Strike rate in initial trials
range(initial_trials$strike_rate, na.rm = T) # 0.007473842 0.857142857
initial_trials[initial_trials$strike_rate < 0.02,]
range(thermal_trials$strike_rate, na.rm = T) #0.0212766 1.2500000

strike_name = expression(paste("Strike Rate (s"^-1, ")"))
strike_break = c(0.01,  0.1,  1)
strike_lim = c(0.007, 2)
strike_label =  c("0.01", "0.1","1")

df = initial_trials %>% filter(strike_rate > 0)
newdata_init = trial_init_fun(df = df, response = df$strike_rate)
(p3a = plot_trial_init(ratio = 0.5, df = df, newdata = newdata_init, response = df$strike_rate, y_name = strike_name, 
                       y_breaks =strike_break,  x_name = NULL, x_labels = NULL,  y_limits = strike_lim, y_labels = strike_label))

#save_fun(p = p3a, 'Fig5/strike_rate/strike_rate_init.pdf')

# Fig 5F: Strike rate over thermal trials
df = thermal_trials %>% filter(strike_rate > 0)
newdata = trial_temp_fun(df = df, response = df$strike_rate)
(p3b = plot_trial_temp(ratio = 0.5, df = df, newdata = newdata, response = df$strike_rate, y_name = NULL,
                       y_breaks = NULL, x_name = NULL, x_labels = NULL, y_limits = strike_lim, y_labels = NULL ))

#save_fun(p = p, 'Fig5/strike_rate/strike_rate.pdf')

#---Fig 5G: Initial strikes per capture 
range(initial_trials$strikes_n, na.rm = T)
range(thermal_trials$strikes_n, na.rm = T)

strike_name2 = expression(paste("Strikes per Capture"))
strike_break2 = c(1,  10, 100)
strike_lim2 = c(1, 100)
strike_label2 =  c("1","10","100")

df = initial_trials %>% filter(strikes_n > 0)
newdata_init = trial_init_fun(df = df, response = df$strikes_n)
(p4a = plot_trial_init(ratio = 0.5, df = df, newdata = newdata_init, response = df$strikes_n, y_name = strike_name2, 
                       y_breaks = strike_break2, x_name = NULL, x_labels = NULL, y_limits = strike_lim2, y_labels = strike_label2))

#save_fun(p = p, 'Fig5/strikes_capture/strike_capture_init.pdf')

#---Fig 5H: Initial strikes per capture 
df = thermal_trials %>% filter(strikes_n> 0)
newdata_init = trial_temp_fun(df = df, response = df$strikes_n)
(p4b = plot_trial_temp(ratio = 0.5, df = df, newdata = newdata_init, response = df$strikes_n, y_name = NULL, 
                       y_breaks = NULL,  y_limits =strike_lim2, y_labels = NULL, x_labels = NULL, x_name = NULL ))

#save_fun(p = p4b, 'Fig5/strikes_capture/strike_capture.pdf')

#---Fig 5I: Time to Capture 
range(initial_trials$elapsed_s, na.rm = T) # 3 10035
range(thermal_trials$elapsed_s, na.rm = T) # 1 258

time_name = expression(paste("Time per Capture (s)"))
time_break = c(1, 10, 100, 1000, 10000)
time_lim = c(1, 10035)
time_label =  c("1", "10","100", "1000", "10,000")

df = initial_trials %>% filter(elapsed_s > 0)
newdata_init = trial_init_fun(df = df, response = df$elapsed_s)
(p5a = plot_trial_init(ratio = 0.5, df = df, newdata = newdata_init, response = df$elapsed_s, y_name = time_name,
                       y_breaks = time_break, x_name = NULL, x_labels = NULL, y_limits = time_lim, y_labels = time_label))

#save_fun(p = p5a, 'Fig5/time_capture/time_capture_init.pdf')

#---Fig 5J: Time to Capture 
df = thermal_trials %>% filter(elapsed_s > 0)
newdata_init = trial_temp_fun(df = df, response = df$elapsed_s)
(p5b = plot_trial_temp(ratio = 0.5, df = df, newdata = newdata_init, response = df$elapsed_s, y_name = NULL, 
                       y_breaks = NULL,   y_limits = time_lim, y_labels = NULL, x_labels = NULL, x_name = NULL ))

#save_fun(p = p5b, 'Fig5/time_capture/time_capture.pdf')

#---Fig 5K: Initial Distance to Capture 
range(initial_trials$mouse_dist_travel[initial_trials$mouse_dist_travel > 0], na.rm = T) #0.490363 2786.805149
range(thermal_trials$mouse_dist_travel[thermal_trials$mouse_dist_travel > 0], na.rm = T) #0.2717234 972.3092285

dist_name2 = expression(atop("Distance Traveled", "per Capture (cm)"))
dist_break2 = c(1, 10, 100, 1000)
dist_lim2 = c(0.25, 2800)
dist_label2 =  c( "1", "10","100", "1000")

df = initial_trials %>% filter(mouse_dist_travel > 0)
newdata_init = trial_init_fun(df = df, response = df$mouse_dist_travel)
(p6a = plot_trial_init(ratio = 0.5, df = df, newdata = newdata_init, response = df$mouse_dist_travel, y_name = dist_name2, 
                       y_breaks = dist_break2,  y_limits = dist_lim2, y_labels = dist_label2))

#save_fun(p = p6a, 'Fig5/dist_capture/dist_capture_init.pdf')

#---Fig 5L: Initial Distance to Capture 
df = thermal_trials %>% filter(mouse_dist_travel > 0)
newdata_init = trial_temp_fun(df = df, response = df$mouse_dist_travel)
(p6b = plot_trial_temp(ratio = 0.5, df = df, newdata = newdata_init, response = df$mouse_dist_travel, y_name = NULL, 
                       y_breaks = NULL,  y_limits = dist_lim2, y_labels = NULL ))
#save_fun(p = p6b, 'Fig5/dist_capture/dist_capture.pdf')

avg_dist = hunt_results %>% filter(stage2 == "thermal_trials")
median(avg_dist$snout_roach_dist_q50, na.rm = T)


(p = (p1a/p2a/p3a/p4a/p5a/p6a) | (p1b/p2b/p3b/p4b/p5b/p6b) )



###########################################################
#------------- Supplemental Experimental Figures #########
###########################################################

# ---------- Chamber/arena temperature over time ------

# Lst of chamber temperature CSV file paths
filenames <- list.files(file.path(path, "Experim_data/arena_temps") , recursive = TRUE, pattern = "*.csv", full.names = TRUE)

# Read in all CSV files and concatenate them into a single data frame
  temp_main <- do.call("rbind", lapply(filenames, function(x) {
    dat <- fread(x) 
    dat$file_name <- tools::file_path_sans_ext(basename(x))
    dat
  }))

str(temp_main)

# Add more columns of information based on the file name
temp_main$Date = as_datetime(temp_main$Date, tz = "America/Chicago") #convert Date to a type of object recognized as date/time
temp_main$date = as_date(word(temp_main$Date, 1, sep = " ")) #extract date from Date into its own column
temp_main$start_time0 = word(temp_main$file_name, 5, sep = "_") #extract start time from file_name into its own column
temp_main$start_time = as_datetime(paste(temp_main$date, temp_main$start_time0, sep = "_"), tz = "America/Chicago") #Combine date + start+time0 into its own column and convert it to a type of object recognized as date/time
temp_main <- temp_main %>% mutate(start_time0 = gsub("-", ":", start_time0)) #replaces '-' with ':' in start_time0
temp_main$start_time_s = period_to_seconds(hms(temp_main$start_time0)) #convert start_time0 to seconds and save in new column (start_time_s)
temp_main$start_time0 = NULL # delete start_time0 column because it's not needed anymore

temp_main$time = word(temp_main$Date, 2, sep = " ") #extract time from Date into its own column
temp_main$elapsed_s = period_to_seconds(hms(temp_main$time)) #convert time to seconds and save in new column (elapsed_s)
temp_main$elapsed_s_from_zero <- temp_main$elapsed_s - temp_main$start_time_s #subract start_time_s from elapsed_s and save in new column (elapsed_s_from_zero

temp_main$box = word(temp_main$file_name, 6, sep = "_") #extract box # into its own column
temp_main$file = paste(temp_main$date, temp_main$box, sep = "_") #combine date and box # into its own column

# Assign targ_temp to each file
temp_mapping <- data.frame(
  file = c('2022-02-01_box1', 
           '2022-01-28_box1', 
           '2022-03-01_box1', 
           '2022-01-28_box2', 
           '2022-02-11_box2'),
  targ_temp = c(14, 18, 25, 30, 35)
)

temp_main_merged <- temp_main %>%
  left_join(temp_mapping, by = "file")


# Manually delete "tails" from 14C and 35C, in which the sensors continued running after the temp control finished
temp_main_merged <- temp_main_merged[temp_main_merged$elapsed_s_from_zero <= 5400, ]

# Manually align temp data so "0" reflects when cooling/heating started (ie remove latency)
temp_main_merged$elapsed_s_from_zero[temp_main_merged$targ_temp == 35] <- temp_main_merged$elapsed_s_from_zero[temp_main_merged$targ_temp == 35] - 300
temp_main_merged$elapsed_s_from_zero[temp_main_merged$targ_temp == 14] <- temp_main_merged$elapsed_s_from_zero[temp_main_merged$targ_temp == 14] - 200


# Plot final temp data

p_final <- ggplot(data = temp_main_merged) + 
  theme_plot() + 
  scale_color_manual(name = "Temperature (\u00B0C)", values = temp_scale) +
  geom_line(size = 1,
            aes(x = elapsed_s_from_zero, y = Temperature, group = file, color = as.factor(targ_temp))) +
  scale_x_continuous(name = "Time (min)", limits = c(0, 6000), breaks = seq(0, 5400, by = 1800), labels = seq(0, 90, by = 30)) +
  coord_cartesian(xlim = c(0, 5400)) +
  scale_y_continuous(name = "Temperature (\u00B0C)", 
                     breaks = seq(10, 35, by = 5), 
                     minor_breaks = seq(17.5, 32.5, by = 5)) +
  theme(legend.key = element_blank()) 
  
  p_final

################## Mismatch ###################
mismatch = hunt_results %>% filter(mismatch_trial == T, trial_daily ==  "1") %>%
  dplyr::select(date, animal, trial_daily, mismatch, targ_temp, trial_temp, trial_at_temp, everything())

mismatch_cold = mismatch %>% filter(targ_temp == 14)

cold_q50 = mismatch_cold %>% 
  filter(trial_daily == "1", snout_roach_dist_q50 > 0, mouse_dist_travel > 0) %>%
  dplyr::select(mismatch, animal, roach_g, roach_vel_q50, roach_acc_q50, mouse_vel_q50, mouse_acc_q50, snout_roach_dist_q50, strikes_n, strike_rate, elapsed_s, mouse_dist_travel)  

cold_long_q50 = cold_q50 %>% pivot_longer( cols = 4:12) %>% rename(response = name) %>% arrange(response)
cold_long_q50$mismatch  <- factor(cold_long_q50$mismatch , levels = c('FALSE', 'TRUE'))
cold_long_q50$response = factor(cold_long_q50$response, levels = c("roach_vel_q50", "roach_acc_q50", "mouse_vel_q50", "mouse_acc_q50", "snout_roach_dist_q50","strike_rate",  "strikes_n", "elapsed_s" ,"mouse_dist_travel"))

levels(cold_long_q50$response)

# Cold Mismatch Figure
(p = ggplot(aes(x = response, y = value, fill = mismatch), data = cold_long_q50) +
    theme_plot_small +
    scale_y_log10(name = 'Response', limits = c(0.04, 300), breaks = c(0.1, 1, 10, 100, 1000), labels = c("0.1", "1", "10", "100", "1000")) + 
    geom_boxplot(outlier.shape = NA, aes(fill = mismatch), alpha = 0.3) + 
    geom_point(shape = 21, size = 2, aes(fill = mismatch), color = "black", show.legend = F,
               position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.35)) +
    scale_fill_manual(values = c("FALSE" = "dodgerblue", "TRUE" = "gray37"), drop = F, 
                      labels = c("FALSE" = "Standard", "TRUE" = "Mismatch")) +
    geom_signif(y_position = c(1.4, 0.9, 1.5, 1.2 , .5, .1, 1.2, 1.8, 2.3), xmin = seq(from = 0.8, to = 8.8, by = 1), xmax =  seq(from = 1.2, to = 9.2, by = 1), 
                annotation = c("ns", "ns", "ns", "ns", "ns", "ns", "**", "***", "**"), size = NA, textsize = 6) +
    theme(legend.title = element_blank(), aspect.ratio = 0.6) +
    geom_vline(xintercept = 2.5) +
    scale_x_discrete(name = NULL, 
                     labels=c("Roach Speed","Roach Acceleration", "Mouse Speed","Mouse Acceleration", "Predator-Prey Distance","Strikes per Time",  
                              "Strikes per Capture", "Time to Capture","Distance Traveled to Capture")) + 
    theme(axis.text.x =element_text(angle=45, vjust = 1, hjust = 1)) +
    theme(legend.key = element_blank()) +
    labs(title = "Roach is 14 ºC") )




#----- Warm Mismatch
mismatch_warm=  mismatch %>% filter(targ_temp == 30) #trial_temp

warm_q50 = mismatch_warm %>% 
  filter(trial_daily == "1", snout_roach_dist_q50 > 0, mouse_dist_travel > 0) %>%
  dplyr::select(mismatch, animal, roach_g, roach_vel_q50, roach_acc_q50, mouse_vel_q50, mouse_acc_q50, snout_roach_dist_q50, strikes_n, strike_rate, elapsed_s, mouse_dist_travel)  
warm_long_q50 = warm_q50 %>% pivot_longer( cols = 4:12) %>% rename(response = name) %>% arrange(response)
warm_long_q50$mismatch  <- factor(warm_long_q50$mismatch , levels = c('FALSE', 'TRUE'))

warm_long_q50$response = factor(warm_long_q50$response, levels = c("roach_vel_q50", "roach_acc_q50", "mouse_vel_q50", "mouse_acc_q50", "snout_roach_dist_q50","strike_rate",  "strikes_n", "elapsed_s" ,"mouse_dist_travel"))
levels(warm_long_q50$response)

(p = ggplot(aes(x = response, y = value, fill = mismatch), data = warm_long_q50) +
    theme_plot_small +
    scale_y_log10(name = 'Response', limits = c(0.03, 1000), breaks = c(0.1, 1, 10, 100, 1000), labels = c("0.1", "1", "10", "100", "1000")) + 
    geom_boxplot(outlier.shape = NA, aes(fill = mismatch), alpha = 0.3) + 
    geom_point(shape = 21, size = 2, aes(fill = mismatch), color = "black", show.legend = F,
               position = position_jitterdodge(dodge.width = 0.75, jitter.width = 0.35)) +
    scale_fill_manual(values = c("FALSE" = "#fdae61", "TRUE" = "gray37"), drop = F, 
                      labels = c("FALSE" = "Standard", "TRUE" = "Mismatch")) +
    geom_signif(y_position = c(1.5, 1, 1.5, 1.1, .3, 0, 1.3, 2.1, 2.8), xmin = seq(from = 0.8, to = 8.8, by = 1), xmax =  seq(from = 1.2, to = 9.2, by = 1), 
                annotation = c("ns", "ns", "ns", "ns", "ns", "ns", "ns", "**", "***"), size = NA, textsize = 6) +
    theme(legend.title= element_blank(), aspect.ratio = 0.6) +
    geom_vline(xintercept = 2.5) +
    scale_x_discrete(name = NULL, 
                     labels=c("Roach Speed","Roach Acceleration", "Mouse Speed","Mouse Acceleration", "Predator-Prey Distance","Strikes per Time",  
                              "Strikes per Capture", "Time to Capture","Distance to Capture")) + 
    theme(axis.text.x =element_text(angle = 45, vjust = 1, hjust = 1)) +
    theme(legend.key = element_blank()) +
    labs(title = "Roach is 30 ºC") )


################## Locomotory Movement Threshold ###################
# Videos observed of mice or red runners not locomoting but showing moveemnt recording - eg from grooming, pixel jittering, etc 
# Observed non locomotory movement as used as a threshold to exclude from locomotory analysis (95% quantile)
no_loco = read_csv(file.path(path, "Experim_data/no_locomotion.csv"))

#standardize temperatures
no_loco$temperature[no_loco$temperature == 13]  =14
no_loco$temperature[no_loco$temperature == 11]  =14
no_loco$temperature[no_loco$temperature == 17]  =18
no_loco$temperature[no_loco$temperature == 23]  = 25

# plotting terms
speed_name = expression(paste("Speed (cm s"^-1, ")"))
acc_name = expression(paste("Acceleration (cm s"^-2, ")"))

speed_limits = c(.25, 150)
acc_limits = c(.025, 150)

# 95% Quantiles
quantile(no_loco$mouse_acc, 0.95, na.rm = T) #31.3
quantile(no_loco$roach_acc, 0.95, na.rm = T) #13.8 

quantile(no_loco$mouse_speed, 0.95, na.rm = T) #76.9
quantile(no_loco$roach_speed, 0.95, na.rm = T) #14.3


# Roach non-locomotory movement (Blue)
p1 = ggplot(no_loco  %>% filter(type_animal == "roach"), aes(x = as.factor(temperature), y = roach_speed)) + 
    scale_y_log10(limits = speed_limits, breaks = c(1, 10, 100), labels = c("1",   '10', '100'), 
                  name = speed_name) + 
    theme_plot() +
    geom_hline(yintercept = 14.3, linetype = "dashed", color = "dodgerblue2", linewidth = 1) +
    scale_x_discrete( name = NULL, labels = NULL) +
    geom_jitter(shape = 21, size = 3, fill = "dodgerblue2", color = "black", width = 0.1) +
    geom_boxplot(fill = NA, outlier.shape = NA)

# Mouse non-locomotory movement (Red)
p2 = ggplot(no_loco  %>% filter(type_animal == "mouse"), aes(x = as.factor(temperature), y = mouse_speed)) + 
  scale_y_log10(limits = speed_limits, breaks = c(1, 10, 100), labels = NULL,  name = NULL) + 
  theme_plot()+
  geom_hline(yintercept = 76.9, linetype = "dashed", color = "#EE220C", linewidth = 1) +
  scale_x_discrete( name = NULL, labels = NULL) +
  geom_jitter(shape = 21, size = 3, fill = "#EE220C", color = "black", width = 0.1) +
  geom_boxplot(fill = NA, outlier.shape = NA)

# Acceleration
p3 = ggplot(no_loco  %>% filter(type_animal == "roach"), aes(x = as.factor(temperature), y = roach_acc)) + 
  scale_y_log10(limits = acc_limits, breaks =  c(0.1, 1, 10, 100), labels = c('0.1', "1",   '10', '100'), name = acc_name) + 
  theme_plot()+
  scale_x_discrete(name = "Temperature (ºC)") +
  geom_hline(yintercept = 13.8, linetype = "dashed", color = "dodgerblue2", linewidth = 1) +
  geom_jitter(shape = 21, size = 3, fill = "dodgerblue2", color = "black", width = 0.1) +
  geom_boxplot(fill = NA, outlier.shape = NA)

p4 = ggplot(no_loco  %>% filter(type_animal == "mouse"), aes(x = as.factor(temperature), y = mouse_acc)) + 
  scale_y_log10(limits = acc_limits, breaks = c(0.1, 1, 10, 100),labels = NULL,  name = NULL) + 
  theme_plot()+
  scale_x_discrete(name = "Temperature (ºC)") +
  geom_hline(yintercept = 31.3, linetype = "dashed", color = "#EE220C", linewidth = 1) +
  geom_jitter(shape = 21, size = 3, fill = "#EE220C", color = "black", width = 0.1) +
  geom_boxplot(fill = NA, outlier.shape = NA)



p = (p1 + p2)/ (p3 + p4)
p

################## Time to capture and speed as a funtion of roach mass ###################

p1 = ggplot(data = hunt_results %>% filter(stage2 == "thermal_trials"), aes(x = roach_g, y = roach_vel_q50, group = targ_temperature)) +
  geom_point(shape = 21, aes(color = targ_temperature)) +
  scale_fill_manual(values = temp_scale) + 
  scale_color_manual(values = temp_scale) + 
  scale_x_log10(name = "Red runner mass (g)",limits = c(0.05, 0.2)) +
  scale_y_log10(name = expression("Red runner speed (cm s"^{-1}*")")) +
  theme_plot() +
  labs(color = "Temperature (ºC)") +
  theme_no_x +
  stat_smooth(method = lm, se = T, aes(color = targ_temperature, fill = targ_temperature), alpha = 0.1, show.legend = F) +
  guides(fill ="none", color = guide_legend(override.aes = list(size = 4, stroke = 1))) +
  theme(strip.text = element_text(size = 14), legend.key = element_blank()) 

p2 = ggplot(data = hunt_results %>% filter(stage2 == "thermal_trials"), aes(x = roach_g, y = elapsed_s, group = targ_temperature)) +
  geom_point(shape = 21, aes(color = targ_temperature), show.legend = F) +
  scale_fill_manual(values = temp_scale) + 
  scale_color_manual(values = temp_scale) + 
  scale_x_log10(name = "Red runner mass (g)",limits = c(0.05, 0.2)) +
  scale_y_log10(name = "Time to capture (s)") + 
  theme_plot() +
  stat_smooth(method = lm, se = T, aes(color = targ_temperature, fill = targ_temperature), alpha = 0.1, show.legend = F) 


p1/p2

################## Roach Temperature for mismatch ###################

mismatch_temps = read_csv(file.path(path, "Experim_data", "mismatch_temps.csv"))

library(tidyverse)

# Reshape data to long format
library(tidyverse)

# Reshape data to long format
mismatch_long <- mismatch_temps %>%
  pivot_longer(cols = c(probe1, probe2), names_to = "probe", values_to = "value")

# Plot with reshaped data
p <- ggplot(data = mismatch_long %>% filter(elapsed_time_s <= 60), 
            aes(x = elapsed_time_s, y = value)) +
  theme_plot() +
  scale_y_continuous(name = "Roach Internal Temperature (ºC)") +
  scale_x_continuous(name = "Elapsed Time (s)", breaks = c(0 , 20, 40, 60, 80)) +
  stat_smooth(aes(color = type), se = FALSE, show.legend = FALSE) +
  geom_point(size = 4, shape = 21, stroke = 0.5, color = "black", 
             aes(fill = type), show.legend = FALSE) +
  scale_fill_manual(values = c("HotToCold" = "brown2", "ColdToHot" = "royalblue3")) +
  scale_color_manual(values = c("HotToCold" = "brown2", "ColdToHot" = "royalblue3"))

# Optional: resize plot
p1 <- set_panel_size(p, width = unit(10.25, "cm"), height = unit(8, "cm"))
grid.newpage()
grid.draw(p1)


########### Internal body temperature #########
# body temp regression

body_temps = read_csv(file.path(path, "Experim_data/animal_body_temps.csv"))

p1 = ggplot(data = body_temps %>% filter(species == "mouse"), 
            aes(x = ambient, y = body_temp)) +
  theme_plot() +
  geom_point(shape = 21, color = "black", size = 4,fill = "firebrick2") + 
  scale_y_continuous(limits = c(10, 40), name = "Body Temperature (ºC)") +
  scale_x_continuous(limits = c (8, 35)) +
  theme_no_x +
  stat_smooth(method = "lm", color = "firebrick2", fill = "firebrick2", alpha = 0.2)

p2 = ggplot(data = body_temps %>% filter(species == "roach"), 
            aes(x = ambient, y = body_temp)) +
  theme_plot() +
  geom_point(shape = 21, color = "black", size = 4,fill = "cornflowerblue") + 
  scale_y_continuous(limits = c(10, 40), name = "Body Temperature (ºC)") +
  scale_x_continuous(limits = c (8, 35), name = "Ambient Temperature (ºC)") +
  stat_smooth(method = "lm", color = "cornflowerblue", fill = "cornflowerblue", alpha = 0.2)

p1/p2


# Mouse regression fit
mouse = body_temps %>% filter(species == "mouse")
lm1 = lm(body_temp ~ ambient,  data = mouse )
summary(lm1) #slope = 0.003468, r2 = 0.0007954, p = 0.9471, n = 8

# roach regression fit
roach = body_temps %>% filter(species == "roach")
lm2 = lm(body_temp ~ ambient,  data = roach )
summary(lm2) #slope = .92957 + 0.96756, r2 = 0.9736, p-value: < 2.2e-16, n = 10


####### Per mouse Time to capture initial trials #######

hunt_animals <- hunt_results %>% filter(stage == "initial_trials") %>%
  filter(stage == "initial_trials") %>%
  mutate(panel_id = animal)

hunt_all <- bind_rows(
  hunt_animals,
  hunt_animals %>% mutate(panel_id = "All"))


# Plot
ggplot(data = hunt_all, aes(x = trial_at_temp_init, y = elapsed_s)) +
  geom_point(shape = 21, size = 2, color = "black", fill = "gray70") +
  stat_smooth(color = "black") +
  theme_plot() +
  scale_x_continuous(name = "Training Trial #") +
  scale_y_log10(name = "Time to Capture (s)") +
  facet_wrap(~ panel_id, labeller = as_labeller(c(
    "All" = "All Animals",
    "jmg1" = "Mouse 1",
    "jmg2" = "Mouse 2",
    "jmg3" = "Mouse 3",
    "jmg4" = "Mouse 4",
    "jmg5" = "Mouse 5",
    "jmg6" = "Mouse 6",
    "jmg7" = "Mouse 7",
    "jmg8" = "Mouse 8" ))) +
  theme(strip.text = element_text(size = 14))

################## Per Mouse Time to capture Thermal Trials ###################

hunt_animals <- hunt_results %>% 
  filter(stage2 == "thermal_trials") %>%
  mutate(panel_id = animal)

hunt_all <- bind_rows(
  hunt_animals,
  hunt_animals %>% mutate(panel_id = "All")
)


ggplot(data = hunt_all, aes(x = trial_temp, y = elapsed_s)) +
  geom_point(aes(fill = targ_temperature), shape = 21, size = 2, color = "black") +
  stat_smooth(color = "black", fill = NA, size = 1) +
  scale_fill_manual(values = temp_scale) + 
  scale_color_manual(values = temp_scale) +
  scale_x_continuous(name = "Training Trial #") +
  scale_y_log10(name = "Time to Capture (s)") +
  labs(fill = "Temperature (ºC)") +
  facet_wrap(~ panel_id, labeller = as_labeller(c(
    "All" = "All Animals",
    "jmg1" = "Mouse 1",
    "jmg3" = "Mouse 3",
    "jmg4" = "Mouse 4",
    "jmg5" = "Mouse 5",
    "jmg6" = "Mouse 6",
    "jmg7" = "Mouse 7",
    "jmg8" = "Mouse 8"))) +
  theme_plot() +
  theme(strip.text = element_text(size = 14),legend.key = element_blank()) +
  guides(fill = guide_legend(override.aes = list(size = 4)), color = "none")

