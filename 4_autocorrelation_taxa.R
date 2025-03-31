#install.packages("rasterAutocorr")
library(tidyverse)
library(raster)
library(rasterVis)
library(spdep)
library(gstat)
library(CARBayes)
library(INLA)
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(INLABMA)
library(xtable)
#install.packages("remotes")
#remotes::install_github("AdamWilsonLab/rasterAutocorr")
#library(rasterAutocorr) https://rdrr.io/github/AdamWilsonLab/rasterAutocorr/
library(devtools)
library(rasterAutocorr)
library(gtools)
library(scales)
library(MuMIn)
library(ggnewscale)
set.ZeroPolicyOption(TRUE)


#######################################################
############## Data #################################
######################################################
vert_ras = brick("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Mouse/Richness/vert_ras.grd", overwrite = T)
vert_df = read_csv("/Users/jgradym/Library/CloudStorage/GoogleDrive-jgradym@gmail.com/My Drive/Mouse/Richness/vert_df.csv")
hist(vert_df$npp)
hist(vert_df$precip)
hist(vert_df$log_precip)
plot(log_endo_ecto_rich ~ precip, data = vert_df)
plot(log_endo_ecto_rich ~ log_precip, data = vert_df)

vert_ras = DftoRas(vert_df)

#######################################################
############## Model Selection #################################
######################################################

# Recreate the model with na.action set to na.fail

# compare log and no log of precipitation and npp
vert_df_clean <- na.omit(vert_df[, c("log_endo_ecto_rich", "one_kT", "precip", "elevation_range", "elevation_sd", "npp")])
vert_df_clean2 <- na.omit(vert_df[, c("log_endo_ecto_rich", "one_kT", "log_precip", "elevation_range", "elevation_sd", "log_npp")])

full_model <- lm(log_endo_ecto_rich ~ one_kT + precip + elevation_range  + npp, 
                 data = vert_df_clean, na.action = na.fail)
full_model2 <- lm(log_endo_ecto_rich ~ one_kT + log_precip + elevation_range  + log_npp, 
                 data = vert_df_clean2, na.action = na.fail)

residuals1 <- residuals(full_model)
residuals2 <- residuals(full_model2)
ks.test(residuals1, "pnorm", mean = mean(residuals1), sd = sd(residuals1))
ks.test(residuals2, "pnorm", mean = mean(residuals2), sd = sd(residuals2))

library(nortest)
ad.test(residuals1)  # For model with precip
ad.test(residuals2)  # For model with log_precip


par(mfrow = c(1,2))
qqnorm(residuals1, main = "Q-Q Plot: Model with Precip")
qqline(residuals1)
qqnorm(residuals2, main = "Q-Q Plot: Model with Log Precip")
qqline(residuals2)

library(lmtest)
bptest(full_model)  # Raw precip
bptest(full_model2)  # Log precip

# Perform model selection with dredge
model_selection <- dredge(full_model, trace = TRUE)  # `trace = TRUE` shows progress
head(model_selection)
model_selection2 <- dredge(full_model2, trace = TRUE)  # `trace = TRUE` shows progress
head(model_selection2)


model_selection_bic <- dredge(full_model, rank = "BIC", trace = TRUE)
head(model_selection_bic)
best_model <- get.models(model_selection_bic, subset = 1)[[1]]
summary(best_model)
library(car)
vif(full_model)

library(glmnet)
predictors_matrix <- model.matrix(full_model)[, -1]  # Remove intercept for glmnet
response <- vert_df_clean$log_endo_ecto_rich
lasso_model <- cv.glmnet(predictors_matrix, response, alpha = 1)  # Lasso
print(coef(lasso_model, s = "lambda.min"))  # Coefficients at optimal lambda


# Standardize variables in the model
standardized_model <- lm(scale(log_endo_ecto_rich) ~ scale(one_kT) + scale(precip) + 
                           scale(elevation_range) + scale(elevation_sd) + 
                           scale(npp), 
                         data = vert_df_clean)
summary(standardized_model)


library(rsq)
partial_r2 <- rsq.partial(standardized_model, adj = TRUE)
print(partial_r2)
partial_r2 <- rsq.partial(full_model, adj = TRUE)
print(partial_r2)
library(relaimpo)
rel_imp <- calc.relimp(standardized_model, type = "lmg")
print(rel_imp)


rel_imp <- calc.relimp(full_model, type = "lmg", rela = F)
print(rel_imp)


#######################################################
############## Endo Ecto #######################
######################################################
#Subset data
vert_df_short = vert_df %>% dplyr::select(x, y, temp, one_kT, log_endo_ecto_rich,  log_precip, log_npp, elevation_range)
vert_ras_short = subset(vert_ras, c("temp", "one_kT", "log_endo_ecto_rich", "log_precip","log_npp", "elevation_range"))

# Linear fit
lm1_endo_ecto = lm(log_endo_ecto_rich ~one_kT + log_precip + elevation_range + log_npp, data = vert_df ,na.action = na.exclude)
lm1 = lm1_endo_ecto
lm1_endo_ecto_temp = lm(log_endo_ecto_rich ~one_kT, data = vert_df ,na.action = na.exclude)
summary(lm1)
confint(lm1)

vert_df_short$predicted <- predict(lm1, na.action = na.exclude)
vert_df_short$residuals <- vert_df_short$predicted  - vert_df_short$log_endo_ecto_rich
#vert_df_short$id <- 1:ncell(vert_ras_short) 
vert_df_short$id <- 1:nrow(vert_df_short) 
#vert_df_short$id2 <- 1:ncell(vert_ras_short)

#---------  shorthand ------
mm = vert_df_short
rr = vert_ras_short
d = vert_ras_short
mm$miss = is.na(mm$one_kT)|is.na(mm$log_endo_ecto_rich)
coord <- coordinates(vert_ras_short[[2]])

names(d)
str(mm)  
lm1
#------------ define neighbors based on distance

# get cell size
dst <- res(d)
# find neighbors
nb <- dnearneigh(as.matrix(mm[!mm$miss,c("x","y")]), 0, max(dst)*1.5,
                 row.names = mm$id[!mm$miss])

## now drPin cells with no neighbors and recompute neighbors
no_neigh = attr(nb,"region.id")[card(nb) == 0]
mm$no_neigh = mm$id %in% no_neigh
mm$keep = !mm$miss & !mm$no_neigh
#?"%in%"
mm2 = mm[mm$keep,]
mm2$id2 = 1:nrow(mm2)
length(mm2$id2)
# find neighbors
nb2 <- dnearneigh(as.matrix(mm2[,c("x","y")]), 0, max(dst)*1.5,
                  row.names = mm2$id2)
str(mm2)
# moran's I test in spdep
mtmc_endo_ecto = moran.mc(mm2$residuals,
                nb2listw(nb2, style = "S"),
                nsim = 1000,
                alternative = "greater",
                na.action = na.exclude,
                zero.policy = T)

str(mm2)

lm.fit_endo_ecto = inla(log_endo_ecto_rich ~ one_kT + log_precip + log_npp + elevation_range, family = "gaussian",
              data = mm2,
              control.compute = list(dic = T),
              control.predictor = list(compute = TRUE, link = 1)
)
lm.fit = lm.fit_endo_ecto
mm2$lm_Prediction = lm.fit$summary.fitted.values$mean                
summary(lm.fit)

nb_tf = tempfile()
nb2INLA(nb2, file = nb_tf)

library(lmerTest)
lm1 = lm(log_endo_ecto_rich ~ one_kT + log_precip +  log_npp + elevation_range,data = mm2)
summary(lm1)
confint(lm1)

###--------- Fit BYM2 model -----------
prior.iid = c(1,0.01)
prior.besag = c(1,0.001)
initial.iid = 4
initial.besag = 3
formula1.bym = log_endo_ecto_rich ~ f(id2, model = "bym2",
                                      graph = nb_tf,
                                      param = c(prior.iid, prior.besag),
                                      initial = c(initial.iid, initial.besag),
                                      adjust.for.con.comp = T,
                                      scale.model = T,
                                      constr=T) + one_kT + log_precip +  log_npp + elevation_range
bym.fit_endo_ecto = inla(formula1.bym,
               family = "gaussian",
               data = mm2,
               control.predictor = list(compute = TRUE, link = 1),
               control.compute = list(dic = TRUE, cpo = TRUE),
               control.inla = list(print.joint.hyper = TRUE),
               verbose = F)
summary(bym.fit_endo_ecto) #0.661
bym.fit = bym.fit_endo_ecto
#-------- R2 Calculations -------------

# Extract fixed-effect means and spatial effect predictions
fixed_effects_mean <- bym.fit$summary.fixed$mean
spatial_effects_mean <- bym.fit$summary.random$id2$mean

# Predictor matrix including the intercept for fixed effects prediction
predictors <- mm2[, c("one_kT", "log_precip", "log_npp", "elevation_range")]
predictors <- cbind(Intercept = 1, predictors)  # Adding intercept

# Fixed Effects Prediction (without spatial effects)
fixed_prediction <- as.matrix(predictors) %*% fixed_effects_mean

# Full Model Prediction (Fixed Effects + Spatial Effects)
total_prediction <- fixed_prediction + spatial_effects_mean[match(mm2$id2, bym.fit$summary.random$id2$ID)]

# Compute the Total Variance in the observed response variable
var_total <- var(mm2$log_endo_ecto_rich, na.rm = TRUE)

# Full Model R2: Variance explained by both Fixed and Spatial Effects
residual_pure_error <- mm2$log_endo_ecto_rich - total_prediction
full_model_r2 <- 1 - (var(residual_pure_error, na.rm = TRUE) / var_total)

# Fixed Effects R2: Variance explained by Fixed Effects only
residual_fixed <- mm2$log_endo_ecto_rich - fixed_prediction
fixed_effects_r2 <- 1 - (var(residual_fixed, na.rm = TRUE) / var_total)

# Temperature-Only Prediction with intercept
intercept_coef <- bym.fit$summary.fixed["(Intercept)", "mean"]
temp_effect_coef <- bym.fit$summary.fixed["one_kT", "mean"]
temp_prediction <- intercept_coef + (temp_effect_coef * mm2$one_kT)

# Ensure no missing values in temperature-only prediction and observed data
temp_prediction <- temp_prediction[!is.na(temp_prediction)]
observed <- mm2$log_endo_ecto_rich[!is.na(temp_prediction)]

# Temperature-Only R2: Variance explained by the temperature effect alone
residual_temp <- observed - temp_prediction
temperature_r2 <- 1 - (var(residual_temp, na.rm = TRUE) / var(observed, na.rm = TRUE))

# Spatial Effects R2: Direct variance explained by Spatial Effects
spatial_effects_predictions <- spatial_effects_mean[match(mm2$id2, bym.fit$summary.random$id2$ID)]
var_spatial_effects <- var(spatial_effects_predictions, na.rm = TRUE)
spatial_effects_r2 <- var_spatial_effects / var_total

# Pure Error R2: Unexplained variance in the Full Model
pure_error_r2 <- 1 - full_model_r2


# Create a two-column data frame
r2_df_endo_ecto <- tibble::tibble(
  Component = c("Full Model R2", "Fixed Effects R2", "Temperature Only R2", 
                "Spatial Effects R2", "Pure Error R2"),
  R2_Value = c(full_model_r2, fixed_effects_r2, temperature_r2, 
               spatial_effects_r2, pure_error_r2)
)

# Print the data frame
print(r2_df_endo_ecto)



#######################################################
############## Shrew Richness #######################
######################################################
#Subset data
names(vert_df)
vert_df_short = vert_df %>% dplyr::select(x, y, temp, one_kT, log_soric_crocid_rich,  log_precip, log_npp, elevation_range)
vert_ras_short = subset(vert_ras, c("temp", "one_kT", "log_soric_crocid_rich", "log_precip","log_npp", "elevation_range"))

# Linear fit
lm1_soric_crocid = lm(log_soric_crocid_rich ~one_kT + log_precip + elevation_range + log_npp, data = vert_df ,na.action = na.exclude)
lm1 = lm1_soric_crocid
summary(lm1)
lm1_soric_crocid_temp = lm(log_soric_crocid_rich ~ one_kT, data = vert_df ,na.action = na.exclude)
summary(lm1_soric_crocid_temp )
confint(lm1_soric_crocid_temp)

vert_df_short$predicted <- predict(lm1, na.action = na.exclude)
vert_df_short$residuals <- vert_df_short$predicted  - vert_df_short$log_soric_crocid_rich
#vert_df_short$id <- 1:ncell(vert_ras_short) 
vert_df_short$id <- 1:nrow(vert_df_short) 
#vert_df_short$id2 <- 1:ncell(vert_ras_short)

#---------  shorthand ------
mm = vert_df_short
rr = vert_ras_short
d = vert_ras_short
mm$miss = is.na(mm$one_kT)|is.na(mm$log_soric_crocid_rich)
coord <- coordinates(vert_ras_short[[2]])

names(d)
str(mm)  
lm1
#------------ define neighbors based on distance

# get cell size
dst <- res(d)
# find neighbors
nb <- dnearneigh(as.matrix(mm[!mm$miss,c("x","y")]), 0, max(dst)*1.5,
                 row.names = mm$id[!mm$miss])

## now drPin cells with no neighbors and recompute neighbors
no_neigh = attr(nb,"region.id")[card(nb) == 0]
mm$no_neigh = mm$id %in% no_neigh
mm$keep = !mm$miss & !mm$no_neigh
#?"%in%"
mm2 = mm[mm$keep,]
mm2$id2 = 1:nrow(mm2)
length(mm2$id2)
# find neighbors
nb2 <- dnearneigh(as.matrix(mm2[,c("x","y")]), 0, max(dst)*1.5,
                  row.names = mm2$id2)
str(mm2)
# moran's I test in spdep
mtmc_soric_crocid = moran.mc(mm2$residuals,
                          nb2listw(nb2, style = "S"),
                          nsim = 1000,
                          alternative = "greater",
                          na.action = na.exclude,
                          zero.policy = T)

str(mm2)

lm.fit_soric_crocid = inla(log_soric_crocid_rich ~ one_kT + log_precip + log_npp + elevation_range, family = "gaussian",
                        data = mm2,
                        control.compute = list(dic = T),
                        control.predictor = list(compute = TRUE, link = 1)
)
lm.fit = lm.fit_soric_crocid
mm2$lm_Prediction = lm.fit$summary.fitted.values$mean                
summary(lm.fit)

nb_tf = tempfile()
nb2INLA(nb2, file = nb_tf)

library(lmerTest)
lm1 = lm(log_soric_crocid_rich ~ one_kT + log_precip +  log_npp + elevation_range,data = mm2)
summary(lm1)
confint(lm1)



###--------- Fit BYM2 model -----------
prior.iid = c(1,0.01)
prior.besag = c(1,0.001)
initial.iid = 4
initial.besag = 3
formula1.bym = log_soric_crocid_rich ~ f(id2, model = "bym2",
                                      graph = nb_tf,
                                      param = c(prior.iid, prior.besag),
                                      initial = c(initial.iid, initial.besag),
                                      adjust.for.con.comp = T,
                                      scale.model = T,
                                      constr=T) + one_kT + log_precip +  log_npp + elevation_range
bym.fit_soric_crocid = inla(formula1.bym,
                         family = "gaussian",
                         data = mm2,
                         control.predictor = list(compute = TRUE, link = 1),
                         control.compute = list(dic = TRUE, cpo = TRUE),
                         control.inla = list(print.joint.hyper = TRUE),
                         verbose = F)
summary(bym.fit_soric_crocid) #0.661
bym.fit = bym.fit_soric_crocid
#-------- R2 Calculations -------------

# Extract fixed-effect means and spatial effect predictions
fixed_effects_mean <- bym.fit$summary.fixed$mean
spatial_effects_mean <- bym.fit$summary.random$id2$mean

# Predictor matrix including the intercept for fixed effects prediction
predictors <- mm2[, c("one_kT", "log_precip", "log_npp", "elevation_range")]
predictors <- cbind(Intercept = 1, predictors)  # Adding intercept

# Fixed Effects Prediction (without spatial effects)
fixed_prediction <- as.matrix(predictors) %*% fixed_effects_mean

# Full Model Prediction (Fixed Effects + Spatial Effects)
total_prediction <- fixed_prediction + spatial_effects_mean[match(mm2$id2, bym.fit$summary.random$id2$ID)]

# Compute the Total Variance in the observed response variable
var_total <- var(mm2$log_soric_crocid_rich, na.rm = TRUE)

# Full Model R2: Variance explained by both Fixed and Spatial Effects
residual_pure_error <- mm2$log_soric_crocid_rich - total_prediction
full_model_r2 <- 1 - (var(residual_pure_error, na.rm = TRUE) / var_total)

# Fixed Effects R2: Variance explained by Fixed Effects only
residual_fixed <- mm2$log_soric_crocid_rich - fixed_prediction
fixed_effects_r2 <- 1 - (var(residual_fixed, na.rm = TRUE) / var_total)

# Temperature-Only Prediction with intercept
intercept_coef <- bym.fit$summary.fixed["(Intercept)", "mean"]
temp_effect_coef <- bym.fit$summary.fixed["one_kT", "mean"]
temp_prediction <- intercept_coef + (temp_effect_coef * mm2$one_kT)

# Ensure no missing values in temperature-only prediction and observed data
temp_prediction <- temp_prediction[!is.na(temp_prediction)]
observed <- mm2$log_soric_crocid_rich[!is.na(temp_prediction)]

# Temperature-Only R2: Variance explained by the temperature effect alone
residual_temp <- observed - temp_prediction
temperature_r2 <- 1 - (var(residual_temp, na.rm = TRUE) / var(observed, na.rm = TRUE))

# Spatial Effects R2: Direct variance explained by Spatial Effects
spatial_effects_predictions <- spatial_effects_mean[match(mm2$id2, bym.fit$summary.random$id2$ID)]
var_spatial_effects <- var(spatial_effects_predictions, na.rm = TRUE)
spatial_effects_r2 <- var_spatial_effects / var_total

# Pure Error R2: Unexplained variance in the Full Model
pure_error_r2 <- 1 - full_model_r2


# Create a two-column data frame
r2_df_soric_crocid <- tibble::tibble(
  Component = c("Full Model R2", "Fixed Effects R2", "Temperature Only R2", 
                "Spatial Effects R2", "Pure Error R2"),
  R2_Value = c(full_model_r2, fixed_effects_r2, temperature_r2, 
               spatial_effects_r2, pure_error_r2)
)

# Print the data frame
print(r2_df_soric_crocid)

###################################################
#-----------Turtles
###################################################
plot_land_log(vert_ras[["turtles_richness"]])
#Subset data
vert_df_short = vert_df %>% dplyr::select(x, y, temp, one_kT, log_turtle_rich,  log_precip, log_npp, elevation_range)
vert_ras_short = subset(vert_ras, c("temp", "one_kT", "log_turtle_rich", "log_precip","log_npp", "elevation_range"))

# Linear fit

lm1_turtle = lm(log_turtle_rich ~one_kT + log_precip + elevation_range + log_npp, data = vert_df ,na.action = na.exclude)
lm1 = lm1_turtle
lm1_turtle_temp = lm(log_turtle_rich ~one_kT, data = vert_df ,na.action = na.exclude)

summary(lm1)
confint(lm1)

############## Spatial Analysis #######################

vert_df_short$predicted <- predict(lm1, na.action = na.exclude)
vert_df_short$residuals <- vert_df_short$predicted  - vert_df_short$log_turtle_rich
#vert_df_short$id <- 1:ncell(vert_ras_short) 
vert_df_short$id <- 1:nrow(vert_df_short) 
#vert_df_short$id2 <- 1:ncell(vert_ras_short)

#---------  shorthand ------
mm = vert_df_short
rr = vert_ras_short
d = vert_ras_short
mm$miss = is.na(mm$one_kT)|is.na(mm$log_turtle_rich)
coord <- coordinates(vert_ras_short[[2]])

names(d)
str(mm)  
lm1

#------------ define neighbors based on distance
# get cell size
dst <- res(d)
# find neighbors
nb <- dnearneigh(as.matrix(mm[!mm$miss,c("x","y")]), 0, max(dst)*1.5,
                 row.names = mm$id[!mm$miss])

## now drPin cells with no neighbors and recompute neighbors
no_neigh = attr(nb,"region.id")[card(nb) == 0]
mm$no_neigh = mm$id %in% no_neigh
mm$keep = !mm$miss & !mm$no_neigh

mm2 = mm[mm$keep,]
mm2$id2 = 1:nrow(mm2)
length(mm2$id2)

# find neighbors
nb2 <- dnearneigh(as.matrix(mm2[,c("x","y")]), 0, max(dst)*1.5,
                  row.names = mm2$id2)
str(mm2)

# moran's I test in spdep
mtmc_turtle = moran.mc(mm2$residuals,
                nb2listw(nb2, style = "S"),
                nsim = 1000,
                alternative = "greater",
                na.action = na.exclude,
                zero.policy = T)

str(mm2)

lm.fit_turtle = inla(log_turtle_rich ~ one_kT + log_precip + log_npp + elevation_range, family = "gaussian",
              data = mm2,
              control.compute = list(dic = T),
              control.predictor = list(compute = TRUE, link = 1)
)
lm.fit = lm.fit_turtle
mm2$lm_Prediction = lm.fit$summary.fitted.values$mean                
summary(lm.fit)

nb_tf = tempfile()
nb2INLA(nb2, file = nb_tf)

library(lmerTest)
lm1 = lm(log_turtle_rich ~ one_kT + log_precip +  log_npp + elevation_range,data = mm2)
summary(lm1)
confint(lm1)

###--------- Fit BYM2 model -----------
prior.iid = c(1,0.01)
prior.besag = c(1,0.001)
initial.iid = 4
initial.besag = 3
formula1.bym = log_turtle_rich ~ f(id2, model = "bym2",
                                      graph = nb_tf,
                                      param = c(prior.iid, prior.besag),
                                      initial = c(initial.iid, initial.besag),
                                      adjust.for.con.comp = T,
                                      scale.model = T,
                                      constr=T) + one_kT + log_precip +  log_npp + elevation_range
bym.fit_turtle = inla(formula1.bym,
               family = "gaussian",
               data = mm2,
               control.predictor = list(compute = TRUE, link = 1),
               control.compute = list(dic = TRUE, cpo = TRUE),
               control.inla = list(print.joint.hyper = TRUE),
               verbose = F)
summary(bym.fit_turtle) #0.661
bym.fit = bym.fit_turtle

#-------- R2 Calculations -------------

# Extract fixed-effect means and spatial effect predictions
fixed_effects_mean <- bym.fit$summary.fixed$mean
spatial_effects_mean <- bym.fit$summary.random$id2$mean

# Predictor matrix including the intercept for fixed effects prediction
predictors <- mm2[, c("one_kT", "log_precip", "log_npp", "elevation_range")]
predictors <- cbind(Intercept = 1, predictors)  # Adding intercept

# Fixed Effects Prediction (without spatial effects)
fixed_prediction <- as.matrix(predictors) %*% fixed_effects_mean

# Full Model Prediction (Fixed Effects + Spatial Effects)
total_prediction <- fixed_prediction + spatial_effects_mean[match(mm2$id2, bym.fit$summary.random$id2$ID)]

# Compute the Total Variance in the observed response variable
var_total <- var(mm2$log_turtle_rich, na.rm = TRUE)

# Full Model R2: Variance explained by both Fixed and Spatial Effects
residual_pure_error <- mm2$log_turtle_rich - total_prediction
full_model_r2 <- 1 - (var(residual_pure_error, na.rm = TRUE) / var_total)

# Fixed Effects R2: Variance explained by Fixed Effects only
residual_fixed <- mm2$log_turtle_rich - fixed_prediction
fixed_effects_r2 <- 1 - (var(residual_fixed, na.rm = TRUE) / var_total)

# Temperature-Only Prediction with intercept
intercept_coef <- bym.fit$summary.fixed["(Intercept)", "mean"]
temp_effect_coef <- bym.fit$summary.fixed["one_kT", "mean"]
temp_prediction <- intercept_coef + (temp_effect_coef * mm2$one_kT)

# Ensure no missing values in temperature-only prediction and observed data
temp_prediction <- temp_prediction[!is.na(temp_prediction)]
observed <- mm2$log_turtle_rich[!is.na(temp_prediction)]

# Temperature-Only R2: Variance explained by the temperature effect alone
residual_temp <- observed - temp_prediction
temperature_r2 <- 1 - (var(residual_temp, na.rm = TRUE) / var(observed, na.rm = TRUE))

# Spatial Effects R2: Direct variance explained by Spatial Effects
spatial_effects_predictions <- spatial_effects_mean[match(mm2$id2, bym.fit$summary.random$id2$ID)]
var_spatial_effects <- var(spatial_effects_predictions, na.rm = TRUE)
spatial_effects_r2 <- var_spatial_effects / var_total

# Pure Error R2: Unexplained variance in the Full Model
pure_error_r2 <- 1 - full_model_r2


# Create a two-column data frame
r2_df_turtle <- tibble::tibble(
  Component = c("Full Model R2", "Fixed Effects R2", "Temperature Only R2", 
                "Spatial Effects R2", "Pure Error R2"),
  R2_Value = c(full_model_r2, fixed_effects_r2, temperature_r2, 
               spatial_effects_r2, pure_error_r2)
)

# Print the data frame
print(r2_df_turtle)
summary(bym.fit_turtle) #0.661


# squamates


#######################################################
############## Squamates #######################
######################################################
#Subset data
names(vert_ras)
vert_df_short = vert_df %>% dplyr::select(x, y, temp, one_kT, log_squamate_rich,  log_precip, log_npp, elevation_range)
vert_ras_short = subset(vert_ras, c("temp", "one_kT", "log_squamate_rich", "log_precip","log_npp", "elevation_range"))
# Linear fit
lm1_squam = lm(log_squamate_rich ~one_kT + log_precip + elevation_range + log_npp, data = vert_df ,na.action = na.exclude)
lm1_squam_temp = lm(log_squamate_rich ~one_kT, data = vert_df ,na.action = na.exclude)
lm1 = lm1_squam 
summary(lm1)
confint(lm1)
vert_df_short$predicted <- predict(lm1, na.action = na.exclude)
vert_df_short$residuals <- vert_df_short$predicted  - vert_df_short$log_squamate_rich
#vert_df_short$id <- 1:ncell(vert_ras_short) 
vert_df_short$id <- 1:nrow(vert_df_short) 
#vert_df_short$id2 <- 1:ncell(vert_ras_short)

#---------  shorthand ------
mm = vert_df_short
rr = vert_ras_short
d = vert_ras_short
mm$miss = is.na(mm$one_kT)|is.na(mm$log_squamate_rich)
coord <- coordinates(vert_ras_short[[2]])

names(d)
str(mm)  
lm1

#------------ define neighbors based on distance
# get cell size
dst <- res(d)
# find neighbors
nb <- dnearneigh(as.matrix(mm[!mm$miss,c("x","y")]), 0, max(dst)*1.5,
                 row.names = mm$id[!mm$miss])

## now drPin cells with no neighbors and recompute neighbors
no_neigh = attr(nb,"region.id")[card(nb) == 0]
mm$no_neigh = mm$id %in% no_neigh
mm$keep = !mm$miss & !mm$no_neigh
#?"%in%"
mm2 = mm[mm$keep,]
mm2$id2 = 1:nrow(mm2)
length(mm2$id2)
# find neighbors
nb2 <- dnearneigh(as.matrix(mm2[,c("x","y")]), 0, max(dst)*1.5,
                  row.names = mm2$id2)
str(mm2)
# moran's I test in spdep
mtmc_squam = moran.mc(mm2$residuals,
                nb2listw(nb2, style = "S"),
                nsim = 1000,
                alternative = "greater",
                na.action = na.exclude,
                zero.policy = T)

str(mm2)

lm.fit_squam = inla(log_squamate_rich ~ one_kT + log_precip + log_npp + elevation_range, family = "gaussian",
              data = mm2,
              control.compute = list(dic = T),
              control.predictor = list(compute = TRUE, link = 1)
)
lm.fit = lm.fit_squam
mm2$lm_Prediction = lm.fit$summary.fitted.values$mean                
summary(lm.fit)

nb_tf = tempfile()
nb2INLA(nb2, file = nb_tf)

library(lmerTest)
lm1 = lm(log_squamate_rich ~ one_kT + log_precip +  log_npp + elevation_range,data = mm2)
summary(lm1)
confint(lm1)

###--------- Fit BYM2 model -----------
prior.iid = c(1,0.01)
prior.besag = c(1,0.001)
initial.iid = 4
initial.besag = 3
formula1.bym = log_squamate_rich ~ f(id2, model = "bym2",
                                   graph = nb_tf,
                                   param = c(prior.iid, prior.besag),
                                   initial = c(initial.iid, initial.besag),
                                   adjust.for.con.comp = T,
                                   scale.model = T,
                                   constr=T) + one_kT + log_precip +  log_npp + elevation_range
bym.fit_squam = inla(formula1.bym,
               family = "gaussian",
               data = mm2,
               control.predictor = list(compute = TRUE, link = 1),
               control.compute = list(dic = TRUE, cpo = TRUE),
               control.inla = list(print.joint.hyper = TRUE),
               verbose = F)
summary(bym.fit_squam) #0.661
bym.fit = bym.fit_squam
#-------- R2 Calculations -------------

# Extract fixed-effect means and spatial effect predictions
fixed_effects_mean <- bym.fit$summary.fixed$mean
spatial_effects_mean <- bym.fit$summary.random$id2$mean

# Predictor matrix including the intercept for fixed effects prediction
predictors <- mm2[, c("one_kT", "log_precip", "log_npp", "elevation_range")]
predictors <- cbind(Intercept = 1, predictors)  # Adding intercept

# Fixed Effects Prediction (without spatial effects)
fixed_prediction <- as.matrix(predictors) %*% fixed_effects_mean

# Full Model Prediction (Fixed Effects + Spatial Effects)
total_prediction <- fixed_prediction + spatial_effects_mean[match(mm2$id2, bym.fit$summary.random$id2$ID)]

# Compute the Total Variance in the observed response variable
var_total <- var(mm2$log_squamate_rich, na.rm = TRUE)

# Full Model R2: Variance explained by both Fixed and Spatial Effects
residual_pure_error <- mm2$log_squamate_rich - total_prediction
full_model_r2 <- 1 - (var(residual_pure_error, na.rm = TRUE) / var_total)

# Fixed Effects R2: Variance explained by Fixed Effects only
residual_fixed <- mm2$log_squamate_rich - fixed_prediction
fixed_effects_r2 <- 1 - (var(residual_fixed, na.rm = TRUE) / var_total)

# Temperature-Only Prediction with intercept
intercept_coef <- bym.fit$summary.fixed["(Intercept)", "mean"]
temp_effect_coef <- bym.fit$summary.fixed["one_kT", "mean"]
temp_prediction <- intercept_coef + (temp_effect_coef * mm2$one_kT)

# Ensure no missing values in temperature-only prediction and observed data
temp_prediction <- temp_prediction[!is.na(temp_prediction)]
observed <- mm2$log_squamate_rich[!is.na(temp_prediction)]

# Temperature-Only R2: Variance explained by the temperature effect alone
residual_temp <- observed - temp_prediction
temperature_r2 <- 1 - (var(residual_temp, na.rm = TRUE) / var(observed, na.rm = TRUE))

# Spatial Effects R2: Direct variance explained by Spatial Effects
spatial_effects_predictions <- spatial_effects_mean[match(mm2$id2, bym.fit$summary.random$id2$ID)]
var_spatial_effects <- var(spatial_effects_predictions, na.rm = TRUE)
spatial_effects_r2 <- var_spatial_effects / var_total

# Pure Error R2: Unexplained variance in the Full Model
pure_error_r2 <- 1 - full_model_r2


# Create a two-column data frame
r2_df_squam <- tibble::tibble(
  Component = c("Full Model R2", "Fixed Effects R2", "Temperature Only R2", 
                "Spatial Effects R2", "Pure Error R2"),
  R2_Value = c(full_model_r2, fixed_effects_r2, temperature_r2, 
               spatial_effects_r2, pure_error_r2)
)

# Print the data frame
print(r2_df_squam)



#######################################################
############## Salamanders #######################
######################################################
#Subset data
names(vert_ras)
vert_df_short = vert_df %>% dplyr::select(x, y, temp, one_kT, log_salamander_rich,  log_precip, log_npp, elevation_range)
vert_ras_short = subset(vert_ras, c("temp", "one_kT", "log_salamander_rich", "log_precip","log_npp", "elevation_range"))
# Linear fit
lm1_sal = lm(log_salamander_rich ~ one_kT + log_precip + elevation_range + log_npp, data = vert_df ,na.action = na.exclude)
lm1_sal_temp = lm(log_salamander_rich ~ one_kT, data = vert_df ,na.action = na.exclude)
lm1 = lm1_sal
summary(lm1)
confint(lm1)
vert_df_short$predicted <- predict(lm1, na.action = na.exclude)
vert_df_short$residuals <- vert_df_short$predicted  - vert_df_short$log_salamander_rich
#vert_df_short$id <- 1:ncell(vert_ras_short) 
vert_df_short$id <- 1:nrow(vert_df_short) 
#vert_df_short$id2 <- 1:ncell(vert_ras_short)

#---------  shorthand ------
mm = vert_df_short
rr = vert_ras_short
d = vert_ras_short
mm$miss = is.na(mm$one_kT)|is.na(mm$log_salamander_rich)
coord <- coordinates(vert_ras_short[[2]])

names(d)
str(mm)  
lm1

#------------ define neighbors based on distance
# get cell size
dst <- res(d)
# find neighbors
nb <- dnearneigh(as.matrix(mm[!mm$miss,c("x","y")]), 0, max(dst)*1.5,
                 row.names = mm$id[!mm$miss])

## now drPin cells with no neighbors and recompute neighbors
no_neigh = attr(nb,"region.id")[card(nb) == 0]
mm$no_neigh = mm$id %in% no_neigh
mm$keep = !mm$miss & !mm$no_neigh
#?"%in%"
mm2 = mm[mm$keep,]
mm2$id2 = 1:nrow(mm2)
length(mm2$id2)

c# find neighbors
nb2 <- dnearneigh(as.matrix(mm2[,c("x","y")]), 0, max(dst)*1.5,
                  row.names = mm2$id2)
str(mm2)
# moran's I test in spdep
mtmc_salamander = moran.mc(mm2$residuals,
                     nb2listw(nb2, style = "S"),
                     nsim = 1000,
                     alternative = "greater",
                     na.action = na.exclude,
                     zero.policy = T)

str(mm2)

lm.fit_sal = inla(log_salamander_rich ~ one_kT + log_precip + log_npp + elevation_range, family = "gaussian",
              data = mm2,
              control.compute = list(dic = T),
              control.predictor = list(compute = TRUE, link = 1)
)
lm.fit = lm.fit_sal
mm2$lm_Prediction = lm.fit$summary.fitted.values$mean                
summary(lm.fit)

nb_tf = tempfile()
nb2INLA(nb2, file = nb_tf)

library(lmerTest)
lm1 = lm(log_salamander_rich ~ one_kT + log_precip +  log_npp + elevation_range,data = mm2)
summary(lm1)
confint(lm1)

###--------- Fit BYM2 model -----------
prior.iid = c(1,0.01)
prior.besag = c(1,0.001)
initial.iid = 4
initial.besag = 3
formula1.bym = log_salamander_rich ~ f(id2, model = "bym2",
                                 graph = nb_tf,
                                 param = c(prior.iid, prior.besag),
                                 initial = c(initial.iid, initial.besag),
                                 adjust.for.con.comp = T,
                                 scale.model = T,
                                 constr=T) + one_kT + log_precip +  log_npp + elevation_range
bym.fit_salamander= inla(formula1.bym,
                    family = "gaussian",
                    data = mm2,
                    control.predictor = list(compute = TRUE, link = 1),
                    control.compute = list(dic = TRUE, cpo = TRUE),
                    control.inla = list(print.joint.hyper = TRUE),
                    verbose = F)
summary(bym.fit_salamander) #0.661
bym.fit = bym.fit_salamander
#-------- R2 Calculations -------------

# Extract fixed-effect means and spatial effect predictions
fixed_effects_mean <- bym.fit$summary.fixed$mean
spatial_effects_mean <- bym.fit$summary.random$id2$mean

# Predictor matrix including the intercept for fixed effects prediction
predictors <- mm2[, c("one_kT", "log_precip", "log_npp", "elevation_range")]
predictors <- cbind(Intercept = 1, predictors)  # Adding intercept

# Fixed Effects Prediction (without spatial effects)
fixed_prediction <- as.matrix(predictors) %*% fixed_effects_mean

# Full Model Prediction (Fixed Effects + Spatial Effects)
total_prediction <- fixed_prediction + spatial_effects_mean[match(mm2$id2, bym.fit$summary.random$id2$ID)]

# Compute the Total Variance in the observed response variable
var_total <- var(mm2$log_salamander_rich, na.rm = TRUE)

# Full Model R2: Variance explained by both Fixed and Spatial Effects
residual_pure_error <- mm2$log_salamander_rich - total_prediction
full_model_r2 <- 1 - (var(residual_pure_error, na.rm = TRUE) / var_total)

# Fixed Effects R2: Variance explained by Fixed Effects only
residual_fixed <- mm2$log_salamander_rich - fixed_prediction
fixed_effects_r2 <- 1 - (var(residual_fixed, na.rm = TRUE) / var_total)

# Temperature-Only Prediction with intercept
intercept_coef <- bym.fit$summary.fixed["(Intercept)", "mean"]
temp_effect_coef <- bym.fit$summary.fixed["one_kT", "mean"]
temp_prediction <- intercept_coef + (temp_effect_coef * mm2$one_kT)

# Ensure no missing values in temperature-only prediction and observed data
temp_prediction <- temp_prediction[!is.na(temp_prediction)]
observed <- mm2$log_salamander_rich[!is.na(temp_prediction)]

# Temperature-Only R2: Variance explained by the temperature effect alone
residual_temp <- observed - temp_prediction
temperature_r2 <- 1 - (var(residual_temp, na.rm = TRUE) / var(observed, na.rm = TRUE))

# Spatial Effects R2: Direct variance explained by Spatial Effects
spatial_effects_predictions <- spatial_effects_mean[match(mm2$id2, bym.fit$summary.random$id2$ID)]
var_spatial_effects <- var(spatial_effects_predictions, na.rm = TRUE)
spatial_effects_r2 <- var_spatial_effects / var_total

# Pure Error R2: Unexplained variance in the Full Model
pure_error_r2 <- 1 - full_model_r2


# Create a two-column data frame
r2_df_salamander<- tibble::tibble(
  Component = c("Full Model R2", "Fixed Effects R2", "Temperature Only R2", 
                "Spatial Effects R2", "Pure Error R2"),
  R2_Value = c(full_model_r2, fixed_effects_r2, temperature_r2, 
               spatial_effects_r2, pure_error_r2)
)
r2_df_salamander


# Print the data frame
print(r2_df_salamander)


r2_df_endo_ecto
r2_df_soric_crocid
r2_df_squam
r2_df_turtle
r2_df_salamander
r2_df_frog

# List of R² dataframes with their corresponding taxon names
r2_dfs <- list(
  "Endo_Ecto" = r2_df_endo_ecto,
  "Soric_Crocid" = r2_df_soric_crocid,
  "Squamates" = r2_df_squam,
  "Turtles" = r2_df_turtle,
  "Salamanders" = r2_df_salamander,
  "Frogs" = r2_df_frog
)

# Initialize an empty dataframe
final_r2_df <- data.frame(Taxon = character(), stringsAsFactors = FALSE)

# Loop through each R² dataframe and reshape
for (taxon in names(r2_dfs)) {
  df <- r2_dfs[[taxon]]
  
  # Convert to wide format
  df_wide <- t(df$R2_Value)  # Transpose values
  colnames(df_wide) <- df$Component  # Use Component names as column names
  df_wide <- as.data.frame(df_wide)  # Convert back to dataframe
  df_wide$Taxon <- taxon  # Add Taxon column
  
  # Bind rows together
  final_r2_df <- rbind(final_r2_df, df_wide)
}

# Reorder columns to have Taxon first
final_r2_df <- final_r2_df[, c("Taxon", names(final_r2_df)[names(final_r2_df) != "Taxon"])]
# Round all numeric values to 3 decimal places
final_r2_df[, -1] <- round(final_r2_df[, -1], 2)

# Print final result
print(final_r2_df)

# Print final result
print(final_r2_df)
write_csv(final_r2_df, "~/Downloads/bayes_r2.csv")

summary(lm1_endo_ecto)
summary(lm1_endo_ecto_temp)
summary(lm.fit_endo_ecto)


summary(bym.fit_endo_ecto)

summary(lm1_frog)
summary(lm1_frog_temp)
summary(bym.fit_frog)

summary(lm1_squam)
summary(lm1_squam_temp)
summary(bym.fit_squam)

summary(lm1_sal)
summary(lm1_sal_temp)
summary(bym.fit_salamander)

summary(lm1_turtle)
summary(lm1_turtle_temp)
summary(bym.fit_turtle)

# List of INLA models
bym_models <- list(
  "Endo_Ecto" = bym.fit_endo_ecto,
  "Shrew_Ratio" = bym.fit_soric_crocid,
  "Squamates" = bym.fit_squam,
  "Turtles" = bym.fit_turtle,
  "Frogs" = bym.fit_frog,
  "Salamanders" = bym.fit_salamander
)

# Create an empty dataframe
bayes_results <- data.frame(
  Taxon = character(),
  Estimate_BYM2 = numeric(),
  Lower_95_BYM2 = numeric(),
  Upper_95_BYM2 = numeric(),
  stringsAsFactors = FALSE
)

# Loop through models and extract one_kT values
for (model_name in names(bym_models)) {
  
  model <- bym_models[[model_name]]
  
  # Extract values from INLA model summary
  model_summary <- summary(model)$fixed
  estimate <- model_summary["one_kT", "mean"]
  lower_95 <- model_summary["one_kT", "0.025quant"]
  upper_95 <- model_summary["one_kT", "0.975quant"]
  
  # Append to dataframe
  bayes_results<- rbind(bayes_results, data.frame(
    Taxon = model_name,
    E_BYM2 = estimate,
    Lower_95_BYM2 = lower_95,
    Upper_95_BYM2 = upper_95
  ))
}

# Print results
print(bayes_results)
final_r2_df[, -1] <- round(final_r2_df[, -1], 2)

write_csv(bayes_results, "~/Downloads/bayes_results.csv")

# Create an empty dataframe with proper structure
lm_results_df <- data.frame(
  Taxon = character(),
  E_LM = numeric(),
  Lower_95_LM = numeric(),
  Upper_95_LM = numeric(),
  R2_LM = numeric(),
  E_LM_Temp = numeric(),
  Lower_95_LM_Temp = numeric(),
  Upper_95_LM_Temp = numeric(),
  Adjusted_R2_LM_Temp = numeric(),
  stringsAsFactors = FALSE
)

# Create an empty dataframe with proper structure
lm_results_df <- data.frame(
  Taxon = character(),
  E_LM = numeric(),
  Lower_95_LM = numeric(),
  Upper_95_LM = numeric(),
  R2_LM = numeric(),
  E_LM_Temp = numeric(),
  Lower_95_LM_Temp = numeric(),
  Upper_95_LM_Temp = numeric(),
  R2_LM_Temp = numeric(),
  stringsAsFactors = FALSE
)

# Temporary storage for results
temp_results <- list()

# First, process all **non-temp** models
for (model_name in names(lm_models)) {
  
  if (!grepl("_Temp$", model_name)) {  # Only process base models first
    model <- lm_models[[model_name]]
    
    # Extract values safely
    estimate <- coef(model)["one_kT"]
    conf_int <- tryCatch(confint(model)["one_kT", ], error = function(e) c(NA, NA))
    lower_95 <- conf_int[1]
    upper_95 <- conf_int[2]
    r2 <- summary(model)$r.squared  # Use regular R² instead of adjusted R²
    
    # Store the base taxon results
    temp_results[[model_name]] <- list(
      Taxon = model_name,
      E_LM = estimate,
      Lower_95_LM = lower_95,
      Upper_95_LM = upper_95,
      R2_LM = r2,  # Regular R²
      E_LM_Temp = NA,
      Lower_95_LM_Temp = NA,
      Upper_95_LM_Temp = NA,
      R2_LM_Temp = NA
    )
  }
}

# Process all **_Temp** models and merge into existing base rows
for (model_name in names(lm_models)) {
  
  if (grepl("_Temp$", model_name)) {  # Only process _Temp models
    base_name <- sub("_Temp$", "", model_name)  # Get the base model name
    
    model <- lm_models[[model_name]]
    
    # Extract values safely
    estimate <- coef(model)["one_kT"]
    conf_int <- tryCatch(confint(model)["one_kT", ], error = function(e) c(NA, NA))
    lower_95 <- conf_int[1]
    upper_95 <- conf_int[2]
    r2 <- summary(model)$r.squared  # Regular R²
    
    # Ensure base model exists before merging
    if (base_name %in% names(temp_results)) {
      temp_results[[base_name]]$E_LM_Temp <- estimate
      temp_results[[base_name]]$Lower_95_LM_Temp <- lower_95
      temp_results[[base_name]]$Upper_95_LM_Temp <- upper_95
      temp_results[[base_name]]$R2_LM_Temp <- r2
    } else {
      # If base model was missing, create an entry for it
      temp_results[[base_name]] <- list(
        Taxon = base_name,
        E_LM = NA,
        Lower_95_LM = NA,
        Upper_95_LM = NA,
        R2_LM = NA,
        E_LM_Temp = estimate,
        Lower_95_LM_Temp = lower_95,
        Upper_95_LM_Temp = upper_95,
        R2_LM_Temp = r2
      )
    }
  }
}

# Convert list to dataframe
lm_results_df <- do.call(rbind, lapply(temp_results, as.data.frame))

# Apply absolute values and round to 3 decimal places (except "Taxon")
lm_results_df[, -1] <- round(abs(lm_results_df[, -1]), 3)


# Print final dataframe
print(lm_results_df)

# Reset row names
rownames(lm_results_df) <- NULL

# Print final dataframe
print(lm_results_df)

write_csv(lm_results_df, "~/Downloads/lm_results.csv")
