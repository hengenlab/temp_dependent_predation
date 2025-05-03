#-----------------------------#
# 1. Load Required Libraries  #
#-----------------------------#
library(tidyverse)
library(raster)
library(spdep)
library(gstat)
library(INLA)
#install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(MuMIn)
library(rsq)
library(relaimpo)
library(car)
library(units)
set.ZeroPolicyOption(TRUE)

#-----------------------------#
# 2. Set File Paths  - Update #
#-----------------------------#

path <- "/Users/jgradym/Desktop/Predation_Data"
github_path = "/Users/jgradym/Documents/GitHub/mouse_capture"

#-----------------------------#
# 3. Load Data                #
#-----------------------------#
vert_ras = brick(file.path(path, "Spatial_data/vert_ras.grd"))
vert_df = read_csv(file.path(path, "Spatial_data/vert_df.csv"))
source(file.path(github_path, "3_spatial_functions.R"))


#-----------------------------#
# 4. Data Cleaning & Transform#
#-----------------------------#

# Consider log and no log
vert_df_clean <- na.omit(vert_df[, c("log_endo_ecto_rich", "one_kT", "precip", "log_precip", "elevation_range", "elevation_sd", "npp", "log_npp")])

#-----------------------------#
# 5. Model Exploration        #
#    (Linear vs Log, RMSE,    #
#     Adj R2 Comparison)      #
#-----------------------------#
model_npp      <- lm(log_endo_ecto_rich ~ one_kT + precip + elevation_range + npp, data = vert_df_clean)
model_log_npp  <- lm(log_endo_ecto_rich ~ one_kT + precip + elevation_range + log_npp, data = vert_df_clean)

model_precip      <- lm(log(endo_ecto_rich) ~ precip, data = vert_df)
model_log_precip  <- lm(log(endo_ecto_rich) ~ log(precip), data = vert_df)

# Collect fit statistics
# Collect fit statistics
adjr2_npp       <- summary(model_npp)$adj.r.squared
adjr2_log_npp   <- summary(model_log_npp)$adj.r.squared
rmse_npp        <- sqrt(mean(resid(model_npp)^2))
rmse_log_npp    <- sqrt(mean(resid(model_log_npp)^2))
aic_npp         <- AIC(model_npp)
aic_log_npp     <- AIC(model_log_npp)

adjr2_precip     <- summary(model_precip)$adj.r.squared
adjr2_log_precip <- summary(model_log_precip)$adj.r.squared
rmse_precip      <- sqrt(mean(resid(model_precip)^2))
rmse_log_precip  <- sqrt(mean(resid(model_log_precip)^2))
aic_precip       <- AIC(model_precip)
aic_log_precip   <- AIC(model_log_precip)


# Compare
comparison_table <- data.frame(
  Model = c("Raw NPP", "Log(NPP)", "Raw Precip", "Log(Precip)"),
  Adj_R2 = c(adjr2_npp, adjr2_log_npp, adjr2_precip, adjr2_log_precip),
  RMSE   = c(rmse_npp, rmse_log_npp, rmse_precip, rmse_log_precip),
  AIC    = c(aic_npp, aic_log_npp, aic_precip, aic_log_precip)
)

comparison_table # Overall, minimal difference, log slightly better and easier to interpret

#update: use log only for npp and precipitation
vert_df_clean <- na.omit(vert_df[, c("log_endo_ecto_rich", "one_kT", "log_precip",  "elevation_range", "log_npp")])

#-----------------------------#
# 6. Model Selection &        #
#    Multicollinearity Checks #
#-----------------------------#

lm1 = lm(log_endo_ecto_rich ~ one_kT + log_precip + elevation_range + log_npp, data = vert_df_clean,  na.action = na.fail)
model_selection <- dredge(lm1, trace = TRUE)  # `trace = TRUE` shows progress'
print(model_selection)


#-- check for multicollinearity: low - values < 2.5
vif(lm1)

#-----------------------------#
# 7. Partial R2 & Predictor   #
#    Importance Analysis      #
#-----------------------------#

# Standardize all predictors and response
standardized_model <- lm(scale(log_endo_ecto_rich) ~ 
                           scale(one_kT) + 
                           scale(log_precip) + 
                           scale(elevation_range) + 
                           scale(log_npp), 
                         data = vert_df_clean)

# Calculate partial R² for each predictor
partial_r2_std <- rsq.partial(standardized_model, adj = TRUE)
print(partial_r2_std)

# Relative Importance
rel_imp <- calc.relimp(standardized_model, type = "lmg")
print(rel_imp)

#-----------------------------#
# 8. Model Fitting Functions  #
#    - Linear Model           #
#    - BYM2 Spatial Model     #
#    - Summary Functions      #
#-----------------------------#

# Linear model fit and summary
lm_summary_fit <- function(response_var) {
  pred_vars <- c("one_kT", "log_precip", "log_npp", "elevation_range")
  full_vars <- c("x", "y", "temp", pred_vars, response_var)
  cc <- complete.cases(vert_df[, full_vars])
  mm <- vert_df[cc, ] %>% dplyr::select(all_of(full_vars))
  
  # Fit model
  formula_str <- paste(response_var, "~", paste(pred_vars, collapse = " + "))
  lm_fit <- lm(as.formula(formula_str), data = mm)
  
  # Summarize
  s <- summary(lm_fit)
  coefs <- as.data.frame(s$coefficients)
  colnames(coefs) <- c("Estimate", "Std.Error", "t.value", "p.value")
  confint_lm <- as.data.frame(confint(lm_fit))
  colnames(confint_lm) <- c("CI_2.5", "CI_97.5")
  coefs <- cbind(coefs, confint_lm)
  coefs$Predictor <- rownames(coefs)
  coefs <- dplyr::relocate(coefs, Predictor)
  
  list(summary = coefs, fit = lm_fit, data = mm)
}

# Example: Fit and summarize (single function call)
lm_out <- lm_summary_fit("log_endo_ecto_rich")

# Get table for reporting
print(lm_out$summary)


# Spatial model (BYM2) model fit and summary
bym_fit <- function(response_var) {
  pred_vars <- c("one_kT", "log_precip", "log_npp", "elevation_range")
  # 1. Start with full grid, like vert_df_short
  mm <- vert_df %>% dplyr::select(x, y, temp, all_of(pred_vars), all_of(response_var))
  mm$id <- 1:nrow(mm)
  
  # 2. Flag missings for model predictors and response (just like manual!)
  mm$miss <- is.na(mm$one_kT) | is.na(mm[[response_var]])
  
  # 3. Build neighbors based on non-missing cells only
  dst <- raster::res(vert_ras)
  nb <- spdep::dnearneigh(as.matrix(mm[!mm$miss, c("x", "y")]), 0, max(dst)*1.5, row.names = mm$id[!mm$miss])
  
  # 4. Find cells with no neighbors
  no_neigh <- attr(nb, "region.id")[spdep::card(nb) == 0]
  mm$no_neigh <- mm$id %in% no_neigh
  
  # 5. Keep cells with data and at least one neighbor
  mm$keep <- !mm$miss & !mm$no_neigh
  mm2 <- mm[mm$keep, ]
  mm2$id2 <- 1:nrow(mm2)
  
  # 6. Neighbors for mm2 only
  nb2 <- spdep::dnearneigh(as.matrix(mm2[, c("x", "y")]), 0, max(dst)*1.5, row.names = mm2$id2)
  
  # 7. Write neighbor file
  nb_tf <- tempfile()
  spdep::nb2INLA(nb2, file = nb_tf)
  
  # 8. Fit BYM2 model
  prior.iid <- c(1, 0.01)
  prior.besag <- c(1, 0.001)
  initial.iid <- 4
  initial.besag <- 3
  bym_formula <- as.formula(
    paste0(response_var, " ~ f(id2, model = 'bym2', graph = nb_tf, scale.model = TRUE, 
    hyper = list(
      prec = list(prior = 'pc.prec', param = c(1, 0.01)),
      phi = list(prior = 'pc', param = c(0.5, 2/3))
    )) + ", 
           paste(pred_vars, collapse = " + "))
  )
  bym.fit <- INLA::inla(bym_formula, family = "gaussian", data = mm2,
                        control.predictor = list(compute = TRUE, link = 1),
                        control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
                        control.inla = list(print.joint.hyper = TRUE),
                        verbose = FALSE)
  return(list(fit = bym.fit, mm2 = mm2))
}

bym_summary_table <- function(fit_result) {
  fit_result$fit$summary.fixed %>%
    as_tibble(rownames = "Predictor")
}


# BYM2 Stats

bym_model_stats <- function(bym_fit, mm2, response_var = "log_endo_ecto_rich") {
  # 1. Extract fixed and spatial effects
  fixed_effects_mean <- bym_fit$fit$summary.fixed$mean
  spatial_effects_mean <- bym_fit$fit$summary.random$id2$mean
  
  # 2. Build predictor matrix (with intercept)
  predictors <- mm2[, c("one_kT", "log_precip", "log_npp", "elevation_range")]
  predictors <- cbind(Intercept = 1, predictors)
  predictors <- as.data.frame(lapply(predictors, as.numeric))  # <- ensure numeric
  
  # 3. Fixed prediction (no spatial)
  fixed_prediction <- as.numeric(as.matrix(predictors) %*% fixed_effects_mean)
  
  # 4. Full model prediction (fixed + spatial)
  spatial_map <- match(mm2$id2, bym_fit$fit$summary.random$id2$ID)
  total_prediction <- fixed_prediction + spatial_effects_mean[spatial_map]
  
  # 5. Total variance
  obs <- mm2[[response_var]]
  var_total <- var(obs, na.rm = TRUE)
  
  # 6. R² calculations
  residual_pure_error <- obs - total_prediction
  full_model_r2 <- 1 - (var(residual_pure_error, na.rm = TRUE) / var_total)
  
  residual_fixed <- obs - fixed_prediction
  fixed_effects_r2 <- 1 - (var(residual_fixed, na.rm = TRUE) / var_total)
  
  # 7. Temperature-only
  intercept_coef <- bym_fit$fit$summary.fixed["(Intercept)", "mean"]
  temp_effect_coef <- bym_fit$fit$summary.fixed["one_kT", "mean"]
  temp_prediction <- intercept_coef + temp_effect_coef * mm2$one_kT
  
  temp_valid <- !is.na(temp_prediction)
  residual_temp <- obs[temp_valid] - temp_prediction[temp_valid]
  temperature_r2 <- 1 - (var(residual_temp, na.rm = TRUE) / var(obs[temp_valid], na.rm = TRUE))
  
  # 8. Spatial effect
  spatial_effects_predictions <- spatial_effects_mean[spatial_map]
  var_spatial_effects <- var(spatial_effects_predictions, na.rm = TRUE)
  spatial_effects_r2 <- var_spatial_effects / var_total
  
  # 9. Pure error R² (unexplained)
  pure_error_r2 <- 1 - full_model_r2
  
  # 10. Other Bayesian/model stats
  dic <- bym_fit$fit$dic$dic
  waic <- if (!is.null(bym_fit$fit$waic)) bym_fit$fit$waic$waic else NA
  
  tibble::tibble(
    Full_Model_R2 = full_model_r2,
    Fixed_Effects_R2 = fixed_effects_r2,
    Temperature_Only_R2 = temperature_r2,
    Spatial_Effects_R2 = spatial_effects_r2,
    Pure_Error_R2 = pure_error_r2,
    DIC = dic,
    WAIC = waic,
    n = nrow(mm2)
  )
}



#-----------------------------#
# 9. Batch Model Fitting      #
#    Across Taxa (Loop)       #
#-----------------------------#
taxa <- c(
  "log_endo_ecto_rich",
  "log_soric_crocid_rich",
  "log_squamate_rich",
  "log_turtle_rich",
  "log_salamander_rich",
  "log_frog_rich",
  "log_mammal_rich",
  "log_bird_rich"
)

taxon_names <- c(
  "Endo_Ecto",
  "Shrew_Ratio",
  "Squamates",
  "Turtles",
  "Salamanders",
  "Frogs",
  "Mammals",
  "Birds"
)


# Linear Model Fit Across Taxa
lm_model_stats <- function(lm_out, response_var) {
  s <- summary(lm_out$fit)
  
  # Compute temperature-only R²
  obs <- lm_out$data[[response_var]]
  temp_pred <- predict(lm(lm_out$data[[response_var]] ~ lm_out$data$one_kT))
  temp_r2 <- 1 - var(obs - temp_pred) / var(obs)
  
  tibble::tibble(
    r_squared = s$r.squared,
    adj_r_squared = s$adj.r.squared,
    AIC = AIC(lm_out$fit),
    resid_se = s$sigma,
    temp_only_r2 = temp_r2,
    n = nrow(lm_out$data)
  )
}

# LM Results
lm_results <- list()

for (i in seq_along(taxa)) {
  response_var <- taxa[i]
  name <- taxon_names[i]
  lm_out <- lm_summary_fit(response_var)
  lm_stats <- lm_model_stats(lm_out, response_var)
  coef_lm <- lm_out$summary %>% filter(Predictor == "one_kT")
  lm_results[[name]] <- tibble(
    Taxon = name,
    E_LM = coef_lm$Estimate,
    Lower_95 = coef_lm$CI_2.5,
    Upper_95 = coef_lm$CI_97.5,
    R2 = lm_stats$r_squared,
    Temp_Only_R2 = lm_stats$temp_only_r2,
    n = lm_stats$n
  )
}


# BYM2 Fit Across Taxa
bym_results <- list()


for (i in seq_along(taxa)) {
  response_var <- taxa[i]
  name <- taxon_names[i]
  
  # BYM2 fit
  cat("Running BYM2 for", name, "\n")
  bym_fit_result <- bym_fit(response_var)
  
  # Get processed data for R² (matches the final mm2 used in bym_fit)
  # Recreate mm2 here to feed into the stats function
  pred_vars <- c("one_kT", "log_precip", "log_npp", "elevation_range")
  mm <- vert_df %>% dplyr::select(x, y, temp, all_of(pred_vars), all_of(response_var))
  mm$id <- 1:nrow(mm)
  mm$miss <- is.na(mm$one_kT) | is.na(mm[[response_var]])
  dst <- raster::res(vert_ras)
  nb <- spdep::dnearneigh(as.matrix(mm[!mm$miss, c("x", "y")]), 0, max(dst)*1.5, row.names = mm$id[!mm$miss])
  no_neigh <- attr(nb, "region.id")[spdep::card(nb) == 0]
  mm$no_neigh <- mm$id %in% no_neigh
  mm$keep <- !mm$miss & !mm$no_neigh
  mm2 <- mm[mm$keep, ]
  mm2$id2 <- 1:nrow(mm2)
  
  # BYM2: get stats and coefficient table
  bym_stats <- bym_model_stats(bym_fit_result, mm2, response_var)
  bym_coef <- bym_summary_table(bym_fit_result)
  one_kT_row <- bym_coef %>% filter(Predictor == "one_kT")
  
  bym_results[[name]] <- tibble(
    Taxon = name,
    E_BYM2 = one_kT_row$mean,
    Lower_95_BYM2 = one_kT_row$`0.025quant`,
    Upper_95_BYM2 = one_kT_row$`0.975quant`,
    Full_Model_R2 = bym_stats$Full_Model_R2,
    Fixed_Effects_R2 = bym_stats$Fixed_Effects_R2,
    Temperature_Only_R2 = bym_stats$Temperature_Only_R2,
    Spatial_Effects_R2 = bym_stats$Spatial_Effects_R2,
    Pure_Error_R2 = bym_stats$Pure_Error_R2,
    DIC = bym_stats$DIC,
    WAIC = bym_stats$WAIC,
    n = bym_stats$n
  )
  
}



#-----------------------------#
# 10. Consolidate & Export    #
#     Results Tables          #
#-----------------------------#

# Combine results into a table
bym_table <- bind_rows(bym_results)


# Format BYM2 table
bym_table <- bym_table %>%
  mutate(across(
    setdiff(names(.)[sapply(., is.numeric)], c("DIC", "WAIC", "n")),
    ~ signif(.x, 3)
  )) %>%
  mutate(across(any_of(c("DIC", "WAIC", "n")), ~ round(.x, 0)))
bym_table

# Likewise, format LM table
lm_table <- bind_rows(lm_results)

lm_table <- lm_table %>%
  mutate(across(setdiff(names(.)[sapply(., is.numeric)], "n"), ~ signif(.x, 3))) %>%
  mutate(across("n", ~ round(.x, 0)))
lm_table 

#-- Save
write_csv(bym_table, "~/Downloads/bym2_summary_table.csv")
write_csv(lm_table, "~/Downloads/lm_summary_table.csv")



################################################
#------- Visualize --------------------
###############################################

# Endo/ecto results for plotting. First fit the BYM2 model
bym_fit_endo_ecto <- bym_fit("log_endo_ecto_rich")
bym.fit <- bym_fit_endo_ecto$fit
mm2 <- bym_fit_endo_ecto$mm2


mml <- mm2 %>%
  dplyr::mutate(
    Observed = as.numeric(log_endo_ecto_rich),
    bym_Prediction = bym.fit$summary.fitted.values$mean,
    bym_LinearPrediction = bym_Prediction - bym.fit$summary.random$id2$mean[1:nrow(mm2)],
    bym_SpatialEffects = bym.fit$summary.random$id2$mean[match(mm2$id2, bym.fit$summary.random$id2$ID)],
    bym_PureError = bym.fit$summary.random$id2$mean[1:nrow(mm2)] - bym_SpatialEffects,
    bym_TempPrediction = bym.fit$summary.fixed["(Intercept)", "mean"] +
      bym.fit$summary.fixed["one_kT", "mean"] * one_kT
  ) %>%
  dplyr::select(x, y, id, Observed, bym_Prediction, bym_LinearPrediction, 
                bym_SpatialEffects, bym_PureError, bym_TempPrediction) %>%
  tidyr::pivot_longer(cols = -c(x, y, id), names_to = "variable", values_to = "val")

mml$variable <- factor(mml$variable, levels = c(
  "Observed", "bym_Prediction", "bym_LinearPrediction",
  "bym_TempPrediction", "bym_SpatialEffects", "bym_PureError"
))



p <- ggplot(mml, aes(x = x, y = y)) +
  geom_tile(aes(fill = val)) +
  geom_sf(data = ocean_plot, inherit.aes = FALSE, color = "black", fill = NA, lwd = 0.12) +
  scale_fill_gradientn(
    colors = c("#053061", "#2166ac", "#4393c3", "#92c5de", "#d1e5f0",
               "#f7f7f7", "#fddbc7", "#f4a582", "#d6604d", "#b2182b", "#67001f"),
    na.value = "white",
    name = "Richness Ratio",
    breaks = log(c(0.25, 1, 4, 16, 64)),
    labels = c("1/4", "1", "4", "16", "64")
  ) +
  facet_wrap(~ variable, ncol = 2, scales = "fixed", labeller = as_labeller(c(
    "Observed" = "Observed",
    "bym_TempPrediction" = "Thermal Prediction",
    "bym_Prediction" = "Full Model Prediction",
    "bym_SpatialEffects" = "Spatially Structured Residuals",
    "bym_LinearPrediction" = "Fixed Effects Prediction",
    "bym_PureError" = "Non-Spatial Residuals"
  ))) +
  theme(
    panel.grid = element_blank(),
    panel.background = element_rect(fill = "white"),
    strip.background = element_rect(fill = "white"),
    strip.text = element_text(size = 11, color = "black"),
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank()
  ) +
  coord_sf()

p
