library(lme4)
library(lmerTest)
library(tidyverse)
library(MuMIn)
hlab_path <- ifelse(Sys.info()['user'] == 'jgradym', '/Volumes/HlabShare/Jacob_work',
                     file.path('O:\\Jacob_work'))
load("/Volumes/Hlabshare/Gradys_project/capture_data.RData") 
thermal_trials$targ_temperature = as.factor(thermal_trials$targ_temperature )

# n trials
sum(!is.na(prehunt_results$mouse_vel_q50)) #2261
sum(!is.na(hunt_results$roach_vel_q50)) #2226
sum(!is.na(hunt_results$elapsed_s)) #2225
sum(!is.na(roach_solo_results$roach_vel_q50)) #262
# 2261 + 2226 + 262 = 4749

##########################################################
#########################  Fig 2 #########################
##########################################################

# body temp regression
 
body_temps = read_csv("/Volumes/HlabShare/grady_amme_cockroach_manuscript/figure_2/animal_body_temps.csv")
body_temps

body_temps$one_kT =  1/(8.617e-5*(body_temps$ambient + 273.15)) # In
unique(body_temps$species)

#mouse
mouse = body_temps %>% filter(species == "mouse")
lm1 = lm(body_temp ~ ambient,  data = mouse )
summary(lm1) #slope = 0.003468, r2 = 0.0007954, p = 0.9471, n = 8
lm1 = lm(body_temp ~ ambient,  data = mouse )
summary(lm1) #slope = 0.003468, r2 = 0.0007954, p = 0.9471, n = 8
#roach body tempo
roach = body_temps %>% filter(species == "roach")
lm1 = lm(body_temp ~ ambient,  data = roach )
summary(lm1) #slope = .92957 + 0.96756, r2 = 0.9736, p-value: < 2.2e-16, n = 10


# mouse movement
# max acc
str(prehunt_results)
lmer1 = lmer(log(mouse_acc_max) ~ one_kT + (1|arena),  data = (prehunt_results %>% filter(stage2 == "thermal_trials")))
sum(!is.na(prehunt_results$mouse_acc_max[prehunt_results$stage2 == "thermal_trials"])) #n = 2261
summary(lmer1)  #E = 2.397e-02, p = 0.00446 **  
confint(lmer1)
anova(lmer1) 
r.squaredGLMM(lmer1) #0.009329084 0.1503717


# median acc
lmer1 = lmer(log(mouse_acc_q50) ~ one_kT + (1|arena),  data = prehunt_results)
sum(!is.na(prehunt_results$mouse_acc_q50)) #n = 2261
summary(lmer1)  #E = 5.590e-03,  0.0245*, 
anova(lmer1) 
r.squaredGLMM(lmer1) #0.002092235 0.07019761

# speed E, mouse and roach
lmer1 = lmer(log(mouse_vel_q50) ~ one_kT + (1|arena),  data = prehunt_results)
sum(!is.na(prehunt_results$mouse_vel_q50)) #n = 2261
summary(lmer1) #E = 2.643e-02, <2e-16 ***
anova(lmer1) 
r.squaredGLMM(lmer1) #0.02753151 0.2304595



# Roach Acceleration, mouse and roach
#max
lm1 = lm(log(roach_acc_max) ~ one_kT + log(roach_g),  data = (roach_solo_results %>% filter(targ_temp <= 30)))
sum(!is.na(roach_solo_results$roach_acc_max)) #n = 262
summary(lm1)  #-0.30118, log(roach_g) = 0.05426 r2 = 0.7009,  p = <2e-16 ***
anova(lm1) 

#median
lm1 = lm(log(roach_acc_q50) ~ one_kT + log(roach_g),  data = (roach_solo_results %>% filter(targ_temp <= 30)))
sum(!is.na(roach_solo_results$roach_acc_q50)) #n = 262
summary(lm1)  #E = -0.134619 , log(roach_g) = 0.035389 r2 = 0.7161,  p = <2e-16 ***
anova(lm1) 


#max
lm1 = lm(log(roach_vel_max) ~ one_kT + log(roach_g),  data = (roach_solo_results %>% filter(targ_temp <= 30)))
sum(!is.na(roach_solo_results$roach_vel_max)) #n = 262
sum(!is.na(roach_solo_results$roach_vel_max)) #n = 262
summary(lm1)  #-0.64475 , log(roach_g) = 0.05426 r2 = 0.7009,  p = <2e-16 ***
confint(lm1)
anova(lm1) 

#median
lm1 = lm(log(roach_vel_q50) ~ one_kT + log(roach_g),  data = (roach_solo_results %>% filter(targ_temp <= 30)))
sum(!is.na(roach_solo_results$roach_acc_q50)) #n = 262
summary(lm1)  #E = -0.134619 , log(roach_g) = 0.035389 r2 = 0.7161,  p = <2e-16 ***
anova(lm1) 



##########################################################
#########################  Fig 3 #########################
##########################################################

# initial learning time to capture



geometric_mean_se <- function(data) {
  n <- length(data)
  
  if (n <= 1) {
    stop("Data must have at least two values for calculating standard error of geometric mean.")
  }
  
  geometric_mean <- exp(mean(log(data)))
  geometric_mean_se <- sqrt(sum((log(data) - log(geometric_mean))^2) / (n * (n - 1)))
  
  return(geometric_mean_se)
}
strike_sub = hunt_35 %>% filter(targ_temp == 25, date >= as.Date("2022-01-24"), !is.na(strike_count))
exp(mean(log(strike_sub$strike_count)))
sd(strike_sub$strike_count)/sqrt(length((strike_sub$strike_count)))
geometric_mean_se(strike_sub$strike_count)

##########################################################
#########################  Fig 4 #########################
##########################################################


############### In Isolation ############## 

#-------------------------------
#-------------- Mouse ----------
#-------------------------------
# Mouse in ISOLATION
# -------- Mouse Speed 
sum(!is.na(pre_35$mouse_vel_q50))
lmer1 = lmer(log(mouse_vel_q50) ~ one_kT + (1|arena), data = pre_35)
summary(lmer1) #2.167e-02 
anova(lmer1) #0.05647 
confint(lmer1) #--0.04195502 0.06975612
r.squaredGLMM(lmer1) # 0.002850818 0.1356719

lmer1 = lmer(log(mouse_vel_max) ~ one_kT + (1|arena), data = pre_35)
summary(lmer1) #5.589e-02 
anova(lmer1) # 0.05364 .
confint(lmer1) #-0.0003817174 0.04695415
r.squaredGLMM(lmer1) #0.003144662 0.06847891

#-------------- Mouse acc  ----------
lm1 = lmer(log(mouse_acc_q50 ) ~ one_kT + (1|arena), data = pre_30) #
summary(lm1) #4.764e-04
anova(lmer1) # 0.05364 

confint(lm1) # -0.006182202 0.00724364
r.squaredGLMM(lm1) # 0.02606852 0.1059625

#-------------- Mouse acc  ----------
lm1 = lmer(log(mouse_acc_max) ~ one_kT + (1|arena), data = pre_30) #
summary(lm1) #1.402e-02  
anova(lm1) #0.04298 
confint(lm1) #-0.002560181 0.03056659
r.squaredGLMM(lm1) #0.00206456 0.1711675


#------------------------------------
#-----Roach in ISOLATION
roach_solo_30 = roach_solo_results %>% filter(targ_temp <= 30)
# -------- Roach Speed 
lm1 = lm(log(roach_vel_max) ~ one_kT + log(roach_g), data = roach_solo_30)
summary(lm1) #-0.64
anova(lm1) #<2e-16 ***
confint(lm1) # -0.6953134 -0.5941926
r.squaredGLMM(lm1) # 0.746
sum(!is.na(roach_solo_30$roach_vel_max))
sum(!is.na(roach_solo_results$roach_vel_max))
# -------- Roach Speed 
lm1 = lm(log(roach_vel_q50) ~ one_kT + log(roach_g), data = roach_solo_30)
sum(!is.na(roach_solo_30$roach_vel_q50))
summary(lm1) # E = -0.41729, r2 = 0.652, n = 227
anova(lm1)
confint(lm1) #-0.04598563 -0.0164999
r.squaredGLMM(lm1) # 0.746

# -------- Roach acc max
lm1 = lm(log(roach_acc_max) ~ one_kT + log(roach_g), data = roach_solo_30)
summary(lm1) #-0.33330  
anova(lm1) #<2e-16 ***
confint(lm1) # -0.36419871 -0.3024005
r.squaredGLMM(lm1) #0.6754513 0.6754513

# -------- Roach acc q50
lm1 = lm(log(roach_acc_q50 ) ~ one_kT + log(roach_g), data = roach_solo_30)
summary(lm1) # E =-0.140436 , r2 =  0.6801, n = 227
anova(lm1)
confint(lm1) #-0.153514372 -0.12735786
r.squaredGLMM(lm1) #  0.6781851 0.6781851


############### In Pursuit ############## 
#-------------------------------
#--------------Roach ----------
#-------------------------------

#-------------- Roach Speed ----------
lm1 = lmer(log(roach_vel_q50) ~ one_kT + (1|arena) +log(roach_g), data = hunt_30) 
summary(lm1) #0.56
confint(lm1) 
r.squaredGLMM(lm1) 
sum(!is.na(hunt_30$roach_vel_max))
sum(!is.na(hunt_35$roach_vel_max))

#max
lm1 = lmer(log(roach_vel_max) ~ one_kT + (1|arena) + log(roach_g), data = hunt_30) 
summary(lm1) #-0.41932  
confint(lm1) 
r.squaredGLMM(lm1) 
sum(!is.na(hunt_30$roach_vel_max))
sum(!is.na(hunt_35$roach_vel_max))

#-------------- Roach acc  ----------
lm1 = lmer(log(roach_acc_q50) ~ one_kT + (1|animal) + log(roach_g), data = hunt_30) 
summary(lm1) #-0.193372
confint(lm1) 
r.squaredGLMM(lm1) 

lm1 = lmer(log(roach_acc_max) ~ one_kT + (1|arena) + log(roach_g), data = hunt_30) 
summary(lm1) #0.42
confint(lm1) 
r.squaredGLMM(lm1)
lm1 = lm(log(roach_acc_max) ~ one_kT + log(roach_g), data = hunt_30) 
summary(lm1)

#-------------- Mouse Speed ----------
sum(!is.na(hunt_30$mouse_vel_q50))
sum(!is.na(hunt_35$mouse_vel_q50))
sum(!is.na(hunt_35$roach_vel_q50))
# Subset the data frame where roach_vel_q50 is NA but mouse_vel_q50 is not NA
subset_hunt_35 <- hunt_35[is.na(hunt_35$roach_vel_q50) & !is.na(hunt_35$mouse_vel_q50), ]

unique(hunt_35$stage)
hunt_35[]
#median
lm1 = lmer(log(mouse_vel_q50) ~ one_kT + (1|arena) + log(roach_g), data = hunt_30) 
summary(lm1) 
confint(lm1) 
r.squaredGLMM(lm1) 

#max
lm1 = lmer(log(mouse_vel_max) ~ one_kT + (1|arena)+ log(roach_g), data = hunt_30) 
summary(lm1) #-2.674e-01  
confint(lm1) #-0.28686455 -0.2478653
r.squaredGLMM(lm1)  #0.3651136 0.4457613

#-------------- Mouse acc  ----------
lm1 = lmer(log(mouse_acc_q50) ~ one_kT + (1|arena)+ log(roach_g), data = hunt_30) 
summary(lm1) #-7.378e-02
confint(lm1) #-0.081913204 -0.06566617
r.squaredGLMM(lm1)  #0.2125851

lm1 = lmer(log(mouse_acc_max) ~ one_kT + (1|arena)+ log(roach_g), data = hunt_30) 
summary(lm1) #-2.454e-01 
confint(lm1) #-0.26338984 -0.22746948
r.squaredGLMM(lm1)  #.3866628 0.4122375


############ Strikes #####################
geometric_mean_se <- function(data) {
  n <- length(data)
  
  if (n <= 1) {
    stop("Data must have at least two values for calculating standard error of geometric mean.")
  }
  
  geometric_mean <- exp(mean(log(data)))
  geometric_mean_se <- sqrt(sum((log(data) - log(geometric_mean))^2) / (n * (n - 1)))
  
  return(geometric_mean_se)
}

#strikes at room temp
hunt_at_25 = hunt_results %>% filter(targ_temp == "25", stage2 == "thermal_trials", strikes_n > 0)
mean(hunt_at_25$strikes_n, na.rm = T)
exp(mean(log(hunt_at_25$strikes_n), na.rm = T)) #4.394773
geometric_mean_se(hunt_at_25$strikes_n) # 0.04259042
unique(hunt_30$stage2)



# n strikes at temp
lmer1 = lmer(log(strikes_n) ~ one_kT + log(roach_g) + (1|arena), data = hunt_30)
summary(lmer1) # -0.42487    
anova(lmer1)
confint(lmer1) #-0.4715774 -0.3780328
r.squaredGLMM(lmer1) #0.1931972 0.4593612
unique(hunt_30$stage2)
#duration of strikes - median
unique(hunt_30$stage2)
str(hunt_30)

#strike rate
sum(!is.na(hunt_30$strikes_n))
sum(!is.na(hunt_35$strikes_n))
sum(!is.na(hunt_30$elapsed_s))
sum(!is.na(hunt_35$elapsed_s))

lmer1 = lmer(log(strikes_n/elapsed_s) ~ one_kT + log(roach_g) + (1|arena), data = hunt_30)
summary(lmer1) # 0.04001
anova(lmer1)
confint(lmer1) #-0.004831148  0.07514801
r.squaredGLMM(lmer1) #0.01083792 0.04377733
1/exp(mean(log(hunt_30$strikes_n/hunt_30$elapsed_s), na.rm = T)) #1 strike per 3.5 seconds
1/mean(hunt_30$strikes_n/hunt_30$elapsed_s, na.rm = T) #1 strike per 3.9 s

#strike duration median
lmer1 = lmer(log(strike_duration_q50) ~ one_kT + log(roach_g) + (1|arena), data = hunt_30)
summary(lmer1) #  0.03355
anova(lmer1) #0.21467  
confint(lmer1)
r.squaredGLMM(lmer1) 

#strike duration - max
unique(hunt_30$stage2)
str(hunt_30)
lmer1 = lmer(log(strike_duration_max) ~ one_kT + log(roach_g) + (1|arena), data = hunt_30)
summary(lmer1) #  -0.08035
anova(lmer1) #0.000207 ***
confint(lmer1) #-0.12248216 -0.03788255
r.squaredGLMM(lmer1)  #0.01083792 0.04377733


################# Capture #################

#-----Time to Capture
lm1 = lmer(log(elapsed_s) ~ one_kT + (1|arena) + log(roach_g), data = hunt_30) 
summary(lm1) #0.51
confint(lm1) 
r.squaredGLMM(lm1) 

#mouse distance traveled
sum(!is.na(hunt_30$mouse_dist_travel))
sum(!is.na(hunt_35$mouse_dist_travel))
unique(hunt_30$stage2)
hunt_dist = hunt_30 %>% filter(mouse_dist_travel > 0)
lmer1 = lmer(log(mouse_dist_travel) ~ one_kT + log(roach_g) + (1|arena), data = hunt_dist)
summary(lmer1) #  -0.87443 
anova(lmer1) #< 2.2e-16 ***
confint(lmer1) #-1.1352191 -1.0166149
r.squaredGLMM(lmer1)  #0.4574835 0.6194867

#roach distance traveled
unique(hunt_30$stage2)
sum(!is.na(hunt_30$roach_dist_travel))
sum(!is.na(hunt_35$roach_dist_travel))
hunt_dist = hunt_30 %>% filter(roach_dist_travel > 0)
lmer1 = lmer(log(roach_dist_travel) ~ one_kT + log(roach_g) + (1|arena), data = hunt_dist)
summary(lmer1) #   -1.42079     0
anova(lmer1) #< 2.2e-16 ***
confint(lmer1) #-1.5036929 -1.337267
r.squaredGLMM(lmer1)  #0.475813 0.6231971


##########################################################
#########################  Fig 5 #########################
##########################################################
thermal_trials = hunt_results %>% filter(stage2 == "thermal_trials")
training_trials = hunt_results %>% filter(stage2 == "initial_trials")
thermal_trials$targ_temperature = as.factor(thermal_trials$targ_temperature )
thermal_trials$hot_cold = NA
thermal_trials$hot_cold[thermal_trials$targ_temp == "14"] = "cold"
thermal_trials$hot_cold[thermal_trials$targ_temp == "18"] = "cold"
thermal_trials$hot_cold[thermal_trials$targ_temp == "30"] = "hot"
thermal_trials$hot_cold[thermal_trials$targ_temp == "35"] = "hot"
thermal_trials$hot_cold[thermal_trials$targ_temp == "25"] = "room"

#hot vs cold
#time to capture
lmer1 = lmer(log(elapsed_s) ~ trial_at_temp * hot_cold + trial_tot + log(roach_g) + (1|animal), data = thermal_trials)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ hot_cold, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE)) # only 30 C is significant

means <- emmeans(lmer1, specs = ~ hot_cold)
pairwise_comparisons_adj <- pairs(means, adjust = "tukey")
summary(pairwise_comparisons_adj)


#strikes to capture
lmer1 = lmer(log(strikes_n) ~ trial_at_temp * hot_cold + trial_tot + log(roach_g) + (1|animal), data = thermal_trials)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ hot_cold, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE)) # only 30 C is significant

means <- emmeans(lmer1, specs = ~ hot_cold)
pairwise_comparisons_adj <- pairs(means, adjust = "tukey")
summary(pairwise_comparisons_adj)

#distance to travel
dist = thermal_trials %>% filter(mouse_dist_travel > 0)
lmer1 = lmer(log(mouse_dist_travel) ~ trial_at_temp * hot_cold + trial_tot + log(roach_g) + (1|animal), data = dist)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ hot_cold, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE)) # only 30 C is significant

means <- emmeans(lmer1, specs = ~ hot_cold)
pairwise_comparisons_adj <- pairs(means, adjust = "tukey")
summary(pairwise_comparisons_adj)

#time to capture
lmer1 = lmer(log(elapsed_s) ~ trial_at_temp * hot_cold + trial_tot + log(roach_g) + (1|animal), data = thermal_trials)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ hot_cold, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE)) # only 30 C is significant

means <- emmeans(lmer1, specs = ~ hot_cold)
pairwise_comparisons_adj <- pairs(means, adjust = "tukey")
summary(pairwise_comparisons_adj)

#median q50
lmer1 = lmer(log(mouse_vel_q50) ~ trial_at_temp * hot_cold + trial_tot + log(roach_g) + (1|animal), data = thermal_trials)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ hot_cold, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE)) # only 30 C is significant

means <- emmeans(lmer1, specs = ~ hot_cold)
pairwise_comparisons_adj <- pairs(means, adjust = "tukey")
summary(pairwise_comparisons_adj)

#snout_roach_dist_q50
lmer1 = lmer(log(snout_roach_dist_q50) ~ trial_at_temp * hot_cold + trial_tot + log(roach_g) + (1|animal), data = thermal_trials)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ hot_cold, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE)) # only 30 C is significant

means <- emmeans(lmer1, specs = ~ hot_cold)
pairwise_comparisons_adj <- pairs(means, adjust = "tukey")
summary(pairwise_comparisons_adj)




#strike rate
lmer1 = lmer(log(strikes_n/elapsed_s) ~ trial_at_temp * hot_cold + trial_tot + log(roach_g) + (1|animal), data = thermal_trials)
#lmer1 = lmer(log(strikes_n/elapsed_s) ~ hot_cold + log(roach_g) + (1|animal), data = thermal_trials)

summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ hot_cold, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE)) # only 30 C is significant

means <- emmeans(lmer1, specs = ~  hot_cold)
pairwise_comparisons_adj <- pairs(means, adjust = "tukey")
summary(pairwise_comparisons_adj)


#srikes n 
lmer1 = lmer(log(strikes_n) ~ trial_at_temp * hot_cold + trial_tot + log(roach_g) + (1|animal), data = thermal_trials)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ hot_cold, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE)) # only 30 C is significant

means <- emmeans(lmer1, specs = ~  hot_cold)
pairwise_comparisons_adj <- pairs(means, adjust = "tukey")
summary(pairwise_comparisons_adj)

#-------------training

#Tracking
lmer1 = lmer(log(mouse_vel_q50) ~ trial_at_temp_init + log(roach_g) + (1|animal), data = training_trials)
summary(lmer1)
anova(lmer1) # 5.313e-16 ***
r.squaredGLMM(lmer1)

lmer1 = lmer(log(snout_roach_dist_q50) ~ trial_at_temp_init + log(roach_g) + (1|animal), data = training_trials)
summary(lmer1)
anova(lmer1) # 5.313e-16 ***
r.squaredGLMM(lmer1)

#strikes
lmer1 = lmer(log(strikes_n) ~ trial_at_temp_init + log(roach_g) + (1|animal), data = training_trials)
summary(lmer1) 
anova(lmer1) #1.09e-08 ***
r.squaredGLMM(lmer1)

lmer1 = lmer(log(strikes_n/elapsed_s) ~ trial_at_temp_init + log(roach_g) + (1|animal), data = training_trials)
summary(lmer1)
anova(lmer1) #< 2.2e-16 ***
r.squaredGLMM(lmer1)


#capture
lmer1 = lmer(log(mouse_dist_travel) ~ trial_at_temp_init + log(roach_g) + (1|animal), data = (training_trials  %>% filter(mouse_dist_travel > 0)))
summary(lmer1) #2.9e-13 ***
anova(lmer1)
r.squaredGLMM(lmer1)

lmer1 = lmer(log(elapsed_s) ~ trial_at_temp_init + log(roach_g) + (1|animal), data = (training_trials  %>% filter(mouse_dist_travel > 0)))
summary(lmer1)  #< 2e-16 ***
anova(lmer1)
r.squaredGLMM(lmer1)


lmer1 = lmer(log(elapsed_s) ~ trial_at_temp_init + log(roach_g) + (1|animal), 
             data = training_trials)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)


#----------- Thermal trials
#Tracking
lmer1 = lmer(log(mouse_vel_q50) ~ trial_at_temp * targ_temperature + trial_tot + log(roach_g) + (1|animal), data = thermal_trials)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ targ_temperature, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE)) # only 30 C is significant
means <- emmeans(lmer1, specs = ~ targ_temperature)
pairwise_comparisons_adj <- pairs(means, adjust = "tukey")
summary(pairwise_comparisons_adj)

#Tracking
lmer1 = lmer(log(mouse_acc_q50) ~ trial_at_temp * targ_temperature + trial_tot + log(roach_g) + (1|animal), data = thermal_trials)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ targ_temperature, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE)) # only 30 C is significant
means <- emmeans(lmer1, specs = ~ targ_temperature)
pairwise_comparisons_adj <- pairs(means, adjust = "tukey")
summary(pairwise_comparisons_adj)

#distance
lmer1 = lmer(log(snout_roach_dist_q50) ~ trial_at_temp * targ_temperature + trial_tot + log(roach_g) + (1|animal), data = thermal_trials)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ targ_temperature, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE))

means <- emmeans(lmer1, specs = ~ targ_temperature)
pairwise_comparisons_adj <- pairs(means, adjust = "tukey")
summary(pairwise_comparisons_adj)

#strikes
lmer1 = lmer(log(strikes_n) ~ trial_at_temp * targ_temperature + trial_tot + log(roach_g) + (1|animal), data = thermal_trials)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ targ_temperature, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE))

means <- emmeans(lmer1, specs = ~ targ_temperature)
pairwise_comparisons_adj <- pairs(means, adjust = "tukey")
summary(pairwise_comparisons_adj)


lmer1 = lmer(log(strikes_n/elapsed_s) ~ trial_at_temp * targ_temperature  + trial_tot + log(roach_g) + (1|animal), data = thermal_trials)
summary(lmer1)
anova(lmer1) # not significant
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ targ_temperature, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE))

means <- emmeans(lmer1, specs = ~ targ_temperature)
pairwise_comparisons_adj <- pairs(means, adjust = "tukey")
summary(pairwise_comparisons_adj)


#distance to capture 
dist = thermal_trials  %>% filter(mouse_dist_travel > 0)

lmer1 = lmer(log(mouse_dist_travel) ~ trial_at_temp * targ_temperature  + trial_tot  +log(roach_g) + (1|animal), 
             data = dist)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ targ_temperature, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE))

means <- emmeans(lmer1, specs = ~ targ_temperature)
pairwise_comparisons_adj <- pairs(means, adjust = "tukey")
summary(pairwise_comparisons_adj)


#distance to capture 
dist = thermal_trials  %>% filter(mouse_dist_travel > 0)

lmer1 = lmer(log(elapsed_s) ~ trial_at_temp * targ_temperature  + trial_tot  +log(roach_g) + (1|animal), data = thermal_trials)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ targ_temperature, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE))

means <- emmeans(lmer1, specs = ~ targ_temperature)
pairwise_comparisons_adj <- pairs(means, adjust = "tukey")
summary(pairwise_comparisons_adj)


library(effects)
allEffects(lmer1)
all_effs <- allEffects(lmer1)
plot(all_effs)
allEffects(lmer1)[3]


lmer1 = lmer(log(elapsed_s) ~ trial_at_temp * targ_temperature   + trial_tot  +log(roach_g) + (1|animal), 
             data = thermal_trials)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ targ_temperature, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE))


allEffects(lmer1)
all_effs <- allEffects(lmer1)
plot(all_effs)
allEffects(lmer1)[3]

#Tracking


# Striking
lmer1 = lmer(log(strikes_n) ~ trial_at_temp_init + log(roach_g) + (1|animal), data = training_trials)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)
trends <- emtrends(lmer1, ~ targ_temperature, var = "trial_at_temp")
summary(trends, infer = c(TRUE, TRUE))

lmer1 = lmer(log(snout_roach_dist_q50) ~ trial_at_temp_init + log(roach_g) + (1|animal), data = training_trials)
summary(lmer1)
anova(lmer1)
r.squaredGLMM(lmer1)


#####################################################################
#####                     Mismatch           ########################
#####################################################################

mismatch = hunt_results %>% filter(mismatch_trial == T) %>%
  dplyr::select(date, animal, trial_daily, mismatch, targ_temp, trial_temp, trial_at_temp, everything())

mismatch_cold = mismatch %>% filter(targ_temp == 14)
mismatch_warm = mismatch %>% filter(targ_temp == 30) #trial_temp

mismatch_cold_1st = mismatch_cold %>% filter(trial_daily ==  "1")
mismatch_warm_1st = mismatch_warm %>% filter(trial_daily ==  "1")

lmer_mis_cold = lmer(log(elapsed_s) ~  mismatch + (1|animal), data = mismatch_cold)
summary(lmer_mis_cold)
anova(lmer_mis_cold) #1.075e-05 ***
str(anova(lmer_mis_cold))

lmer1 = lmer(log(elapsed_s) ~  mismatch + (1|animal), data = mismatch_cold)
summary(lmer1)
anova(lmer1) #1.075e-05 ***
str(anova(lme1))


#------- Max Cold Speeds  - Check for significance -------

cold_response_vars <- c("roach_vel_q50", "roach_acc_q50", "mouse_vel_q50", "mouse_acc_q50", 
                   "strike_rate", "strikes_n", "elapsed_s")
# Prepare a data frame to store the results
cold_results <- data.frame(Response = character(), P_Value = numeric(), stringsAsFactors = FALSE)

for (response in cold_response_vars) {
  formula <- as.formula(paste("log(", response, ") ~ mismatch + roach_g + (1|animal)", sep = ""))
  model <- lmer(formula, data = mismatch_cold_1st)
  coef_summary <- summary(model)$coefficients
  anova_result <- anova(model)
  rsquared <- r.squaredGLMM(model)
  p_value <- anova_result$"Pr(>F)"[1]  # Adjust index if necessary
  cold_results <- rbind(cold_results, data.frame(Response = response, P_Value = p_value, Coefficients = I(list(coef_summary)), 
                                       R2_Marginal = rsquared[1], R2_Conditional = rsquared[2]))
}
# Print the results table
print(cold_results)

# 2 more
lmer1 = lmer(log(snout_roach_dist_q50) ~ mismatch + roach_g + (1|animal), data = (mismatch_cold_1st))
anova(lmer1) #0.7082
lmer1 = lmer(log(mouse_dist_travel) ~ mismatch + roach_g + (1|animal), data = (mismatch_cold_1st %>% filter(mouse_dist_travel >0)))
anova(lmer1) #0.001039 **

#-------warm mismatch trials --------
warm_response_vars <- c("roach_vel_q50", "roach_acc_q50", "mouse_vel_q50", "mouse_acc_q50", "snout_roach_dist_q50",
                   "strike_rate", "strikes_n", "elapsed_s" ,  "mouse_dist_travel" )

warm_results <- data.frame(Response = character(), P_Value = numeric(), stringsAsFactors = FALSE)

for (response in warm_response_vars) {
  formula <- as.formula(paste("log(", response, ") ~ mismatch + roach_g + (1|animal)", sep = ""))
  model <- lmer(formula, data = mismatch_warm_1st)
  coef_summary <- summary(model)$coefficients
  anova_result <- anova(model)
  rsquared <- r.squaredGLMM(model)
  p_value <- anova_result$"Pr(>F)"[1]  # Adjust index if necessary
  warm_results <- rbind(warm_results, data.frame(Response = response, P_Value = p_value, Coefficients = I(list(coef_summary)), 
                                       R2_Marginal = rsquared[1], R2_Conditional = rsquared[2]))
}
# Print the results table
print(warm_results)
sum(!is.na(mismatch_warm_1st$strikes_n))
sum(!is.na(mismatch_warm$elapsed_s)) + 
sum(!is.na(mismatch_cold$elapsed_s))
sum(!is.na(mismatch_cold$elapsed_s)) + sum(!is.na(mismatch_warm$elapsed_s)) #

sum(!is.na(mismatch_cold_1st$elapsed_s)) + sum(!is.na(mismatch_warm_1st$elapsed_s)) #

lmer1 = lmer(log(strikes_n) ~ mismatch + roach_g + (1|animal), data = (mismatch_warm_1st))
anova(lmer1) #0.4222

#average time to captures
exp(mean(log(mismatch_cold_1st$elapsed_s[mismatch_cold_1st$mismatch == T]), na.rm = T))
exp(mean(log(mismatch_cold_1st$elapsed_s[mismatch_cold_1st$mismatch == F]), na.rm = T))

exp(mean(log(mismatch_warm_1st$elapsed_s[mismatch_warm_1st$mismatch == T]), na.rm = T))
exp(mean(log(mismatch_warm_1st$elapsed_s[mismatch_warm_1st$mismatch == F]), na.rm = T))




check = mismatch_warm_1st %>% filter(animal == "jmg1")
lmer1 = lmer(log(roach_vel_q50) ~ mismatch + roach_g + (1|animal), data = mismatch_warm_1st)
anova(lmer1) #0.432447   
lmer1 = lmer(log(roach_acc_q50) ~ mismatch + roach_g + (1|animal), data = mismatch_warm_1st)
anova(lmer1) #0.32158  
lmer1 = lmer(log(mouse_vel_q50) ~ mismatch + roach_g + (1|animal),  data = mismatch_warm_1st)
anova(lmer1) #0.092450
lmer1 = lmer(log(mouse_acc_q50) ~ mismatch + roach_g + (1|animal),  data = mismatch_warm_1st)
anova(lmer1) #0.7678

lmer1 = lmer(log(strike_rate) ~ mismatch + roach_g + (1|animal), data =  mismatch_warm_1st)
anova(lmer1) #0.1614
lmer1 = lmer(log(strikes_n) ~ mismatch + roach_g + (1|animal), data = mismatch_warm_1st)
anova(lmer1) #.13280  
lmer1 = lmer(log(elapsed_s) ~ mismatch + roach_g + (1|animal), data =mismatch_warm_1st)
anova(lmer1) #0.002884 **
lmer1 = lmer(log(snout_roach_dist_q50) ~ mismatch + roach_g + (1|animal), data = (mismatch_warm_1st %>% filter(snout_roach_dist_q50 >0)))
anova(lmer1) #00.3931
lmer1 = lmer(log(mouse_dist_travel) ~ mismatch + roach_g + (1|animal), data = (mismatch_warm_1st %>% filter(mouse_dist_travel >0)))
anova(lmer1) #0.0003455 ***
