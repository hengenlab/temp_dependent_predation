#-----------------------------#
# 1. Load Required Libraries  #
#-----------------------------#
library(tidyverse)
library(raster)
library(spdep)
library(gstat)
library(INLA)
# install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE)
library(MuMIn)
library(rsq)
library(relaimpo)
library(car)
library(units)
set.ZeroPolicyOption(TRUE)

#-----------------------------#
# 2. Set File Paths - Update  #
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
vert_df_clean <- na.omit(vert_df[, c("log_endo_ecto_rich", "one_kT", "precip", "log_precip", "elevation_range", "elevation_sd", "npp", "log_npp")])

#-----------------------------#
# 5. Model Exploration        #
#    (Log vs Raw NPP/Precip)  #
#-----------------------------#
model_npp      <- lm(log_endo_ecto_rich ~ one_kT + precip + elevation_range + npp, data = vert_df_clean)
model_log_npp  <- lm(log_endo_ecto_rich ~ one_kT + precip + elevation_range + log_npp, data = vert_df_clean)
model_precip      <- lm(log(endo_ecto_rich) ~ precip, data = vert_df)
model_log_precip  <- lm(log(endo_ecto_rich) ~ log(precip), data = vert_df)

# Compare model performance
comparison_table <- data.frame(
  Model = c("NPP", "Log(NPP)", "Precip", "Log(Precip)"),
  Adj_R2 = c(
    summary(model_npp)$adj.r.squared,
    summary(model_log_npp)$adj.r.squared,
    summary(model_precip)$adj.r.squared,
    summary(model_log_precip)$adj.r.squared
  ),
  RMSE = c(
    sqrt(mean(resid(model_npp)^2)),
    sqrt(mean(resid(model_log_npp)^2)),
    sqrt(mean(resid(model_precip)^2)),
    sqrt(mean(resid(model_log_precip)^2))
  ),
  AIC = c(
    AIC(model_npp),
    AIC(model_log_npp),
    AIC(model_precip),
    AIC(model_log_precip)
  )
)
comparison_table  # Minimal differences. Use log for simplicity and interpretation

# Update cleaned dataset to use only log-transformed vars
vert_df_clean <- na.omit(vert_df[, c("log_endo_ecto_rich", "one_kT", "log_precip",  "elevation_range", "log_npp")])

#-----------------------------#
# 6. Model Selection &        #
#    Multicollinearity Check  #
#-----------------------------#
lm1 = lm(log_endo_ecto_rich ~ one_kT + log_precip + elevation_range + log_npp, data = vert_df_clean,  na.action = na.fail)
model_selection <- dredge(lm1, trace = TRUE)
print(model_selection)

# VIF for multicollinearity (low: < 2.5)
vif(lm1)

#-----------------------------#
# 7. Partial R² & Importance  #
#-----------------------------#
standardized_model <- lm(scale(log_endo_ecto_rich) ~ 
                           scale(one_kT) + 
                           scale(log_precip) + 
                           scale(elevation_range) + 
                           scale(log_npp), 
                         data = vert_df_clean)

# Partial R² (adjusted)
partial_r2_std <- rsq.partial(standardized_model, adj = TRUE)
print(partial_r2_std)

# Relative importance (lmg metric)
rel_imp <- calc.relimp(standardized_model, type = "lmg")
print(rel_imp)

#-----------------------------#
# 8. Define Taxa for Modeling #
#-----------------------------#
# 1. Define taxa and taxon display names
taxa <- c(
  "log_endo_ecto_rich",
  "log_endo_rich", 
  "log_ecto_rich",
  "log_soric_crocid_rich",
  "log_squamate_rich",
  "log_turtle_rich",
  "log_crocodile_rich",
  "log_frog_rich",
  "log_salamander_rich",
  "log_caecilian_rich",
  "log_mammal_rich",
  "log_bird_rich", 
  "log_amphibian_rich",
  "log_reptile_rich"
)

taxon_names <- c(
  "Endo_Ecto",
  "Endotherms",
  "Ectotherms",
  "Shrew_Ratio",
  "Squamates",
  "Turtles",
  "Crocodiles",
  "Frogs",
  "Salamanders",
  "Caecilians",
  "Mammals",
  "Birds",
  "Amphibians", 
  "Reptiles"
)
#-----------------------------#
# 9. Linear Model Functions   #
#    - Fit Function           #
#    - R² Extraction          #
#    - Summary Results        #
#-----------------------------#

lm_fit <- function(response_var) {
  pred_vars <- c("one_kT", "log_precip", "log_npp", "elevation_range")
  vars_needed <- c(pred_vars, response_var)
  lm_data <- vert_df %>% dplyr::select(all_of(vars_needed)) %>% drop_na()
  
  observed <- lm_data[[response_var]]
  lm_full <- lm(observed ~ one_kT + log_precip + log_npp + elevation_range, data = lm_data)
  lm_temp <- lm(observed ~ one_kT, data = lm_data)
  
  list(
    lm_full = lm_full,
    lm_temp = lm_temp,
    observed = observed,
    data = lm_data
  )
}


lm_r2 <- function(lm_full, lm_temp) {
  r2_full <- summary(lm_full)$r.squared
  r2_temp <- summary(lm_temp)$r.squared
  list(R2_Full = r2_full, R2_Temp = r2_temp)
}

lm_results <- function(lm_fits, taxon_name) {
  lm_full <- lm_fits$lm_full
  lm_temp <- lm_fits$lm_temp
  observed <- lm_fits$observed
  data <- lm_fits$data 
  r2 <- lm_r2(lm_full, lm_temp)
  ci <- confint(lm_full, level = 0.95)
  
  tibble(
    Taxon = taxon_name,
    E_LM = coef(lm_full)["one_kT"],
    Lower_95_LM = ci["one_kT", 1],
    Upper_95_LM = ci["one_kT", 2],
    R2_Full = r2$R2_Full,
    R2_Temp = r2$R2_Temp,
    n = nrow(data)
  )
}

#-----------------------------#
# 10. Run Linear Model Loop   #
#-----------------------------#

lm_fits <- list()
lm_stats <- list()

for (i in seq_along(taxa)) {
  taxon_var <- taxa[i]
  taxon_name <- taxon_names[i]
  
  lm_fits[[taxon_name]] <- lm_fit(taxon_var)
  lm_stats[[taxon_name]] <- lm_results(lm_fits[[taxon_name]], taxon_name)
}

lm_table <- bind_rows(lm_stats) %>%
  mutate(across(setdiff(names(.)[sapply(., is.numeric)], "n"), ~ signif(.x, 3))) %>%
  mutate(across("n", ~ round(.x, 0)))
lm_table
#-----------------------------#
# 11. BYM2 Spatial Model      #
#    - Fit Function           #
#    - R² Extraction          #
#    - Summary Results        #
#-----------------------------#

bym_fit <- function(response_var) {
  pred_vars <- c("one_kT", "log_precip", "log_npp", "elevation_range")
  mm <- vert_df %>% dplyr::select(x, y, temp, all_of(pred_vars), all_of(response_var))
  mm$id <- 1:nrow(mm)
  mm$miss <- is.na(mm$one_kT) | is.na(mm[[response_var]])
  
  dst <- raster::res(vert_ras)
  nb <- spdep::dnearneigh(as.matrix(mm[!mm$miss, c("x", "y")]), 0, max(dst) * 1.5, row.names = mm$id[!mm$miss])
  no_neigh <- attr(nb, "region.id")[spdep::card(nb) == 0]
  mm$no_neigh <- mm$id %in% no_neigh
  mm$keep <- !mm$miss & !mm$no_neigh
  mm2 <- mm[mm$keep, ]
  mm2$id2 <- 1:nrow(mm2)
  
  nb2 <- spdep::dnearneigh(as.matrix(mm2[, c("x", "y")]), 0, max(dst) * 1.5, row.names = mm2$id2)
  nb_tf <- tempfile()
  spdep::nb2INLA(nb2, file = nb_tf)
  
  formula_str <- paste0(
    response_var, " ~ f(id2, model = 'bym2', graph = nb_tf, scale.model = TRUE, ",
    "hyper = list(prec = list(prior = 'pc.prec', param = c(1, 0.01)), ",
    "phi = list(prior = 'pc', param = c(0.5, 2/3)))) + ",
    paste(pred_vars, collapse = " + ")
  )
  bym_model <- as.formula(formula_str)
  
  fit <- INLA::inla(
    bym_model,
    family = "gaussian",
    data = mm2,
    control.predictor = list(compute = TRUE, link = 1),
    control.compute = list(dic = TRUE, waic = TRUE, cpo = TRUE),
    control.inla = list(print.joint.hyper = TRUE),
    verbose = FALSE
  )
  
  list(fit = fit, mm2 = mm2)
}
bym_r2 <- function(bym_fit, mm2, response_var = "log_endo_ecto_rich") {
  fixed_effects_mean <- bym_fit$fit$summary.fixed$mean
  spatial_effects_mean <- bym_fit$fit$summary.random$id2$mean
  
  predictors <- mm2[, c("one_kT", "log_precip", "log_npp", "elevation_range")]
  predictors <- cbind(Intercept = 1, predictors)
  predictors <- as.data.frame(lapply(predictors, as.numeric))
  
  fixed_prediction <- as.numeric(as.matrix(predictors) %*% fixed_effects_mean)
  spatial_map <- match(mm2$id2, bym_fit$fit$summary.random$id2$ID)
  total_prediction <- fixed_prediction + spatial_effects_mean[spatial_map]
  
  obs <- mm2[[response_var]]
  var_total <- var(obs, na.rm = TRUE)
  
  residual_pure_error <- obs - total_prediction
  full_model_r2 <- 1 - (var(residual_pure_error, na.rm = TRUE) / var_total)
  
  residual_fixed <- obs - fixed_prediction
  fixed_effects_r2 <- 1 - (var(residual_fixed, na.rm = TRUE) / var_total)
  
  intercept_coef <- bym_fit$fit$summary.fixed["(Intercept)", "mean"]
  temp_effect_coef <- bym_fit$fit$summary.fixed["one_kT", "mean"]
  temp_prediction <- intercept_coef + temp_effect_coef * mm2$one_kT
  temp_valid <- !is.na(temp_prediction)
  residual_temp <- obs[temp_valid] - temp_prediction[temp_valid]
  temperature_r2 <- 1 - (var(residual_temp, na.rm = TRUE) / var(obs[temp_valid], na.rm = TRUE))
  
  spatial_effects_predictions <- spatial_effects_mean[spatial_map]
  var_spatial_effects <- var(spatial_effects_predictions, na.rm = TRUE)
  spatial_effects_r2 <- var_spatial_effects / var_total
  
  pure_error_r2 <- 1 - full_model_r2
  
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

bym_results <- function(fit_result, response_var, taxon_name) {
  fit <- fit_result$fit
  mm2 <- fit_result$mm2
  
  stats <- bym_r2(fit_result, mm2, response_var)
  
  coef_table <- fit$summary.fixed %>% as_tibble(rownames = "Predictor")
  one_kT_row <- coef_table %>% filter(Predictor == "one_kT")
  
  tibble(
    Taxon = taxon_name,
    E_BYM2 = one_kT_row$mean,
    Lower_95_BYM2 = one_kT_row$`0.025quant`,
    Upper_95_BYM2 = one_kT_row$`0.975quant`,
    Full_Model_R2 = stats$Full_Model_R2,
    Fixed_Effects_R2 = stats$Fixed_Effects_R2,
    Temperature_Only_R2 = stats$Temperature_Only_R2,
    Spatial_Effects_R2 = stats$Spatial_Effects_R2,
    Pure_Error_R2 = stats$Pure_Error_R2,
    DIC = stats$DIC,
    WAIC = stats$WAIC,
    n = stats$n
  )
}

#-----------------------------#
# 12. Run BYM2 Model Loop     #
#-----------------------------#
bym_fits <- list()

for (i in seq_along(taxa)) {
  response_var <- taxa[i]
  name <- taxon_names[i]
  
  cat("Fitting model for", name, "\n")
  bym_fits[[name]] <- bym_fit(response_var)
}

bym_stats <- list()

for (i in seq_along(taxa)) {
  response_var <- taxa[i]
  name <- taxon_names[i]
  
  fit_result <- bym_fits[[name]]  # <- already run model
  bym_stats[[name]] <- bym_results(fit_result, response_var, name)
}

bym_table <- bind_rows(bym_stats) %>%
  mutate(across(
    setdiff(names(.)[sapply(., is.numeric)], c("DIC", "WAIC", "n")),
    ~ signif(.x, 3)
  )) %>%
  mutate(across(any_of(c("DIC", "WAIC", "n")), ~ round(.x, 0)))

bym_table


#------------------------------------------#
# 13. Normality, Linearity and Residuals   #
#------------------------------------------#

# 1. Define taxa and taxon display names
taxa <- c(
  "log_endo_ecto_rich",
  "log_endo_rich", 
  "log_ecto_rich"
)

taxon_names <- c(
  "Endo_Ecto",
  "Endotherms",
  "Ectotherms"
)

get_resid_stats <- function(residuals, observed, model_type, taxon_name) {
  residuals <- residuals[!is.na(residuals)]
  observed <- observed[!is.na(observed)]
  
  skew <- e1071::skewness(residuals)
  kurt <- e1071::kurtosis(residuals)  # excess kurtosis: 0 = normal
  resid_z <- scale(residuals)[,1]
  qq_rmse <- sqrt(mean((sort(resid_z) - qnorm(ppoints(length(resid_z))))^2))
  
  tibble(
    Taxon = taxon_name,
    Model = model_type,
    Skew = skew,
    Kurtosis = kurt,
    QQ_RMSE = qq_rmse
  )
}


# BYM2 residuals
extract_bym_diagnostics <- function(fit_result, response_var, taxon_name) {
  fit <- fit_result$fit
  mm2 <- fit_result$mm2
  
  observed <- mm2[[response_var]]
  predicted <- fit$summary.fitted.values$mean
  residuals <- observed - predicted
  
  get_resid_stats(residuals, observed, "BYM", taxon_name)
}

# Linear model residuals
extract_lm_diagnostics <- function(response_var, taxon_name) {
  lm_data <- vert_df %>%
    dplyr::select(one_kT, log_precip, log_npp, elevation_range, all_of(response_var)) %>%
    drop_na()
  
  observed <- lm_data[[response_var]]
  lm_full <- lm(observed ~ one_kT + log_precip + log_npp + elevation_range, data = lm_data)
  lm_temp <- lm(observed ~ one_kT, data = lm_data)
  
  residuals <- residuals(lm_full)
  
  get_resid_stats(residuals, observed, "LM",  taxon_name)
}


resid_combined <- list()

for (i in seq_along(taxa)) {
  response_var <- taxa[i]
  taxon_name <- taxon_names[i]
  
  resid_combined[[paste0(taxon_name, "_BYM")]] <- extract_bym_diagnostics(bym_fits[[taxon_name]], response_var, taxon_name)
  resid_combined[[paste0(taxon_name, "_LM")]]  <- extract_lm_diagnostics(response_var, taxon_name)
}

resid_table <- bind_rows(resid_combined) %>%
  mutate(across(where(is.numeric), ~ signif(.x, 3))) %>%
  # arrange(Taxon, desc(Model)) %>%
  mutate(Taxon = forcats::fct_relevel(Taxon, "Endo_Ecto")) %>%
  arrange(Model, Taxon)  # optional: BYM before LM

resid_table
#write_csv(resid_table, "~/Downloads/normalitytable.csv")

#------ Evaluate how much better endo/ecto is with respect to linearity and normality of residualas

# Split reference values by model
ref_vals <- resid_table %>%
  filter(Taxon == "Endo_Ecto") %>%
  dplyr::select(Model, Skew, Kurtosis, QQ_RMSE) %>%
  rename_with(~ paste0("ref_", .x), -Model)

# Join reference values by model
resid_pct_worse <- resid_table %>%
  filter(Taxon != "Endo_Ecto") %>%
  left_join(ref_vals, by = "Model") %>%
  mutate(
    `Skew (% Worse)` = 100 * (abs(Skew) - abs(ref_Skew)) / abs(ref_Skew),
    `Kurtosis (% Worse)` = 100 * (Kurtosis - ref_Kurtosis) / ref_Kurtosis,
    `QQ_RMSE (% Worse)` = 100 * (QQ_RMSE - ref_QQ_RMSE) / ref_QQ_RMSE
  ) %>%
  mutate(Order = ifelse(Taxon == "Endotherms", 1, 2)) %>% 
  mutate(across(where(is.numeric), ~ signif(.x, 3))) %>%
  arrange(Order) %>%
  dplyr::select(Taxon, Model, `Skew (% Worse)`, `Kurtosis (% Worse)`, `QQ_RMSE (% Worse)`)
resid_pct_worse
#write_csv(resid_pct_worse, "~/Downloads/resid_pct_worse.csv")

##----------QQ plots
#  Extract residuals for QQ plotting (LM & BYM)
#     - Generate reference lines based on Q1 & Q3
#     - Used to assess normality visually per model/taxon

# Define taxa and names
taxa <- c("log_endo_ecto_rich", "log_endo_rich", "log_ecto_rich")
taxon_names <- c("Endo_Ecto", "Endotherms", "Ectotherms")

# Initialize list
qq_list <- list()

# Loop over taxa
for (i in seq_along(taxa)) {
  taxon_var <- taxa[i]
  taxon_name <- taxon_names[i]
  
  # BYM2 residuals
  mm2_bym <- bym_fits[[taxon_name]]$mm2
  fit_bym <- bym_fits[[taxon_name]]$fit
  res_bym <- mm2_bym[[taxon_var]] - fit_bym$summary.fitted.values$mean
  
  # LM residuals
  lm_data <- lm_fit(taxon_var)$data
  lm_full <- lm_fit(taxon_var)$lm_full
  res_lm <- residuals(lm_full)
  
  # Combine into tidy format
  qq_list[[paste0(taxon_name, "_BYM")]] <- tibble(
    Taxon = taxon_name,
    Model = "BYM",
    Residual = res_bym
  )
  
  qq_list[[paste0(taxon_name, "_LM")]] <- tibble(
    Taxon = taxon_name,
    Model = "LM",
    Residual = res_lm
  )
}

# lines through Q1 and Q3
qq_lines <- qq_df %>%
  group_by(Taxon, Model) %>%
  summarise(
    y = quantile(Residual, probs = c(0.25, 0.75), na.rm = TRUE),
    x = qnorm(c(0.25, 0.75)),
    .groups = "drop"
  ) %>%
  summarise(
    slope = diff(y) / diff(x),
    intercept = y[1] - (diff(y) / diff(x)) * x[1],
    .by = c(Taxon, Model)
  )

qq_df <- left_join(qq_df, qq_lines, by = c("Taxon", "Model"))
qq_df$Taxon <- factor(qq_df$Taxon, levels = c("Endo_Ecto", "Endotherms", "Ectotherms"))
qq_df$Model <- factor(qq_df$Model, levels = c("LM", "BYM"))


ggplot(qq_df, aes(sample = Residual)) +
  stat_qq(size = 0.6) +
  facet_grid(Model ~ Taxon) +
  theme_plot +
  labs(title = "QQ Plots") +
  geom_abline(aes(slope = slope, intercept = intercept), color = "firebrick3", linetype = "dashed") +
  scale_x_continuous(breaks = seq(-3, 3, 2), name = "Theoretical Quantiles") +
  scale_y_continuous(breaks = seq(-3, 3, 2), name = "Sample Residuals") +
  theme(strip.text.x = element_text(size = 14),  # Taxon labels (top)
        strip.text.y = element_text(size = 14)) +
  facet_grid( Model ~ Taxon, labeller = labeller(
    Taxon = c(
      "Endo_Ecto" = "Endo/Ecto",   # or "Endo:Ecto"
      "Endotherms" = "Endotherms",
      "Ectotherms" = "Ectotherms"),
    Model = c(
      "LM" = "Linear Model",
      "BYM" = "BYM2 Model")))



################################################
#------- 14 Visualize Model Fit ---------------
###############################################

# Extract stored fit and data from bym_fits object
bym.fit <- bym_fits[["Endo_Ecto"]]$fit
mm2 <- bym_fits[["Endo_Ecto"]]$mm2

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


ocean_plot2 <- rbind(ocean) # remove lakes from world polygons, too high resolution
p <- ggplot(mml, aes(x = x, y = y)) +
  geom_tile(aes(fill = val)) +
  geom_sf(data = ocean_plot2, inherit.aes = FALSE, color = "black", fill = NA, lwd = 0.12) +
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