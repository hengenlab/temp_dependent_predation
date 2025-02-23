
########### Load Data and Functions ###########
load("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Mouse/capture_data.RData") 
source("capture_plot_fun.R")

##################################################
#########  Figure 3 Hunting Prey ######
##################################################
hunt_vid_example = hunt_vid1  %>% filter(animal == "jmg4", trial_daily == 2, date == "2022-03-08")
hunt_vid_example2 = hunt_vid0  %>% filter(animal == "jmg4", trial_daily == 2, date == "2022-03-08")
#------ Fig 3A: Coordinates of a hunt
(coord <- ggplot(data = hunt_vid_example2) +
   theme_plot() + theme(aspect.ratio = 1) +
   #ggtitle(x_name) +
   scale_y_continuous(name = "Y Coordinates (cm)") + #, labels = c("5", "15", "25")) +#, breaks = c(5, 15, 25), limits = c(1, 28)) +
   scale_x_continuous(name = "X Coordinates (cm)") +#, labels = c("5", "15", "25")) +#, breaks = c(10, 20, 30), limits = c(14, 41)) +
   geom_path(aes(x = snout_x/10, y = snout_y/10), color = "#EE220C") + #, arrow = arrow(angle = 15, ends = "both", type = "closed")
   geom_path(aes(x = roachcenter_x/10, y = roachcenter_y/10), color = "cornflowerblue")) 

#------ Fig 3B: Distances in a hunt
(dist_con  = ggplot(data = hunt_vid_example2,
                    aes(x = elapsed_sec, y = snout_roachcenter_dist)) +
    theme_plot() + 
    geom_point() +
    geom_point(data = x %>% filter( is_contact == 1),
               aes(y = 0),color = "gold2", size = 3, shape = "|") +
    geom_line() + 
    scale_y_continuous(breaks = c(0, 3, 6, 9), limits = c(0, 9),
                       name = "Distance to Roach (cm)") +
    scale_x_continuous(breaks = c(0, 5, 10), 
                       name = "Time (s)"))
#------ Fig 3C: Speed of a hunt
(speed = ggplot(x1,
                aes(x = elapsed_sec)) +
    theme_plot() + #ggtitle(x_name) +
    geom_line(aes(y = spine_vel/10), color = "brown3") + 
    geom_line(aes(y =  roachcenter_vel/10), color = "cornflowerblue") + 
    scale_y_continuous(name = "Speed (cm/s)") +
    scale_x_continuous(name = "Time (s)"))

#------ Fig 3D: Learning to hunt

#########################################################################
####### Figure 4: Solitary, Pursuit, Capature Speeds ###################
#########################################################################
#---- Fig 4B: Solitary Mouse median speed
prehunt_results <- prehunt_results %>%
  mutate(animal = case_when(
    arena == "1A" ~ "jmg1",
    arena == "1B" ~ "jmg2",
    arena == "1C" ~ "jmg3",
    arena == "1D" ~ "jmg4",
    arena == "2A" ~ "jmg5",
    arena == "2B" ~ "jmg6",
    arena == "2C" ~ "jmg7",
    arena == "2D" ~ "jmg8",
    TRUE ~ NA_character_ # Keeps NA if no matching arena
  ))
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
               aes(x = temp_median, y = roach_vel_q50), size = 1.5,
               method = "lm", color = ecto_col, fill = ecto_col, alpha = 0.3, show.legend = F) +
   stat_smooth(aes(x = temp_median, y = roach_vel_q50),size = 1.5, linetype = "dashed",
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
save_fun(p = p,'Fig4/speed/roach_speed_q50_col.pdf')

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

save_fun(p = p, 'Fig4/strikes/strike_capture.pdf')

#-- Fig 4H: Mice strikes per capture 
range(hunt_35$strikes_n, na.rm = T)
strike_name = expression(paste("Strikes per Capture"))
strike_limits = c(1, 40)
strike_breaks  = c( 1,  3, 10, 30)
strike_labels  = c("1",  '3', '10', '30')
str(hunt_35)
hunt_30_strike = hunt_30 %>% filter(!is.na(strikes_n))
newdata = temp_fun(df = hunt_30_strike, response = hunt_30_strike$strikes_n)

(p = plot_temp_col(df = hunt_35, response = hunt_35$strikes_n, y_name = strike_name, breaks = strike_breaks,  limits = strike_limits, labels = strike_labels, col = "black"))


#-- Fig 4I: Time to Capture
range(hunt_35$elapsed_s, na.rm = T)#  1 258
time_name = expression(paste("Time to Capture (s)"))
time_limits = c(1, 300)
time_breaks  =c(1, 3, 10, 30, 100, 300)
time_labels  = c("1", "3", "10", "30", "100", '300')

hunt_30_time = hunt_30 %>% filter(!is.na(elapsed_s))
range(hunt_30_time$date)
newdata = temp_fun(df = hunt_30_time, response = hunt_30_time$elapsed_s)
(p = plot_temp_col(df = hunt_35, response = hunt_35$elapsed_s, y_name = time_name, breaks = time_breaks, limits = time_limits, labels = time_labels, col = "black"))


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
range(initial_trials$snout_roach_dist_q50, na.rm = T) #1.320354 18.153057
range(thermal_trials$snout_roach_dist_q50, na.rm = T)#0.5237994 24.7735426
thermal_trials$id2[thermal_trials$snout_roach_dist_q50 < 0.053]
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

save_fun(p = p, 'Fig5/dist_between/distance_between_init.pdf')

#--- Fig 5B: Distance Between Mouse and Roach, Thermal Trials
df = thermal_trials %>% filter(snout_roach_dist_q50 > 0)
newdata_init = trial_temp_fun(df = df, response = df$snout_roach_dist_q50)
(p1b = plot_trial_temp(ratio = 0.5,df = df, newdata = newdata_init, response = df$snout_roach_dist_q50, y_name = NULL, y_breaks = NULL, x_name = NULL, x_labels = NULL, y_limits = dist_limits, y_labels = NULL ))
save_fun(p = p, 'Fig5/dist_between/distance_between.pdf')


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

save_fun(p = p, 'Fig5/speed/mouse_velocity_q50_init.pdf')

#---Fig 5D: Speed over thermal trials
df = thermal_trials %>% filter(mouse_vel_q50 > 0, !is.na(mouse_vel_q50))
newdata = trial_temp_fun(df = df, response = df$mouse_vel_q50)
(p2b = plot_trial_temp(ratio = 0.5, df = df, newdata = newdata, response = df$mouse_vel_q50,  x_name = NULL, y_name = NULL, x_labels = NULL, y_limits = vel_lim, y_breaks = NULL,y_labels = NULL))
save_fun(p = p, 'Fig5/speed/mouse_velocity_q50.pdf')

#--------
lmer1 = lmer(log(mouse_vel_q50) ~ trial_at_temp *targ_temperature + log(roach_g) + (1|animal), data = thermal_trials)
summary(lmer1)
anova(lmer1)



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

save_fun(p = p, 'Fig5/strike_rate/strike_rate_init.pdf')

# Fig 5F: Strike rate over thermal trials
df = thermal_trials %>% filter(strike_rate > 0)
newdata = trial_temp_fun(df = df, response = df$strike_rate)
(p3b = plot_trial_temp(ratio = 0.5, df = df, newdata = newdata, response = df$strike_rate, y_name = NULL,
                       y_breaks = NULL, x_name = NULL, x_labels = NULL, y_limits = strike_lim, y_labels = NULL ))

save_fun(p = p, 'Fig5/strike_rate/strike_rate.pdf')
lmer1 = lmer(log(strike_rate ) ~ trial_at_temp *targ_temperature + log(roach_g) + (1|animal), data = df)
summary(lmer1)



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

save_fun(p = p, 'Fig5/strikes_capture/strike_capture.pdf')

lmer1 = lmer(log(strikes_n) ~ trial_at_temp *targ_temperature + log(roach_g) + (1|animal), data = df)
summary(lmer1)

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

save_fun(p = p, 'Fig5/time_capture/time_capture_init.pdf')

#---Fig 5J: Time to Capture 
df = thermal_trials %>% filter(elapsed_s > 0)
newdata_init = trial_temp_fun(df = df, response = df$elapsed_s)
(p5b = plot_trial_temp(ratio = 0.5, df = df, newdata = newdata_init, response = df$elapsed_s, y_name = NULL, 
                       y_breaks = NULL,   y_limits = time_lim, y_labels = NULL, x_labels = NULL, x_name = NULL ))

save_fun(p = p, 'Fig5/time_capture/time_capture.pdf')

lmer1 = lmer(log(elapsed_s) ~ trial_at_temp *targ_temperature + log(roach_g) + (1|animal), data = df)
summary(lmer1)


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

save_fun(p = p, 'Fig5/dist_capture/dist_capture_init.pdf')

#---Fig 5L: Initial Distance to Capture 
df = thermal_trials %>% filter(mouse_dist_travel > 0)
newdata_init = trial_temp_fun(df = df, response = df$mouse_dist_travel)
(p6b = plot_trial_temp(ratio = 0.5, df = df, newdata = newdata_init, response = df$mouse_dist_travel, y_name = NULL, 
                       y_breaks = NULL,  y_limits = dist_lim2, y_labels = NULL ))
save_fun(p = p, 'Fig5/dist_capture/dist_capture.pdf')

lmer1 = lmer(log(mouse_dist_travel) ~ trial_at_temp *targ_temperature + log(roach_g) + (1|animal), data = df)
summary(lmer1)

avg_dist = hunt_results %>% filter(stage2 == "thermal_trials")
median(avg_dist$snout_roach_dist_q50, na.rm = T)
library(patchwork)

(p = (p1a/p2a/p3a/p4a/p5a/p6a) | (p1b/p2b/p3b/p4b/p5b/p6b) )




