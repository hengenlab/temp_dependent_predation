library(lme4)
library(lmerTest)
library(tidyverse)
library(MuMIn)
library(broom)
library(emmeans)
library(broom.mixed)
library(performance)

# Paths - Update
path <- "/Users/jgradym/Desktop/Predation_Data"
github_path = "/Users/jgradym/Documents/GitHub/mouse_capture"


#load("/Users/jgradym/Downloads/mouse_capture_datasets_250109/RData/starter.RData")
#----------- Load objects -------------------
load(file.path(path,"Experim_data/capture_data.RData"))

#------ n trials
sum(!is.na(prehunt_results$mouse_vel_q50)) #2261
sum(!is.na(hunt_results$roach_vel_q50)) #2026
sum(!is.na(roach_solo_results$roach_vel_q50)) #262
#Total trials =  2261 + 2026 + 262 = 4549

#########################################################
#----------------Model Selection ------------------------
#########################################################

hunt_results_clean <- hunt_30 %>% 
  filter(!is.na(log(elapsed_s)), !is.na(one_kT), !is.na(trial_daily), !is.na(roach_vel_q50), !is.na(strikes_n),
         !is.na(days_at_temp), !is.na(roach_g), !is.na(mouse_g), !is.na(week_day),
         !is.na(animal), !is.na(person_scorer), !is.na(person_trial), 
         !is.na(sex), !is.na(chamber))

lmer1 = lmer(log(elapsed_s) ~ one_kT + trial_daily + days_at_temp + log(roach_g) + log(mouse_g)+ sex + (1|animal) + (1|person_scorer) + (1|person_trial)  + (1|chamber), data = hunt_results_clean, na.action = na.fail)
# Identify top fixed effects predictor variables
ms_lmer1 =  MuMIn::dredge(lmer1)
head(ms_lmer1, 10)
# Identify top random  effects  variables
lmerTest::step(lmer1)

# apply for other response variables
lmer2 = lmer(log(roach_vel_q50) ~ one_kT + trial_daily + days_at_temp + log(roach_g) + log(mouse_g)+ sex +(1|animal) + (1|person_scorer) + (1|person_trial) +  (1|chamber), data = hunt_results_clean, na.action = na.fail)
ms_lmer2 = MuMIn::dredge(lmer2)
head(ms_lmer2, 10)
lmerTest::step(lmer2)

lmer3 = lmer(log(strikes_n) ~ one_kT + trial_daily + days_at_temp + log(roach_g) + log(mouse_g)+ sex +(1|animal) + (1|person_scorer) + (1|person_trial) + (1|chamber), data = hunt_results_clean, na.action = na.fail)
ms_lmer3 = MuMIn::dredge(lmer3)
head(ms_lmer3, 10)
lmerTest::step(lmer3)

##########################################################
##------------ Fig 4 Analysis ---------------------------
#--- Calculate thermal senstiivty (E) and stats----------
##########################################################
clean_model_data <- function(data, vars) {
  data %>%
    dplyr::filter(
      !if_any(all_of(vars), ~ is.na(.) | is.infinite(.))
    )
}

# Mouse isolation: pre_hunt30
iso_mouse_vars <- list(
  list(var = "mouse_vel_q50", formula = log(mouse_vel_q50) ~ one_kT + (1|arena), model = "lmer"),
  list(var = "mouse_vel_max", formula = log(mouse_vel_max) ~ one_kT + (1|arena), model = "lmer"),
  list(var = "mouse_acc_q50", formula = log(mouse_acc_q50) ~ one_kT + (1|arena), model = "lmer"),
  list(var = "mouse_acc_max", formula = log(mouse_acc_max) ~ one_kT + (1|arena), model = "lmer")
)


# Roach isolation: roach_solo_30
iso_roach_vars <- list(
  list(var = "roach_vel_max", formula = log(roach_vel_max) ~ one_kT + log(roach_g), model = "lm"),
  list(var = "roach_vel_q50", formula = log(roach_vel_q50) ~ one_kT + log(roach_g), model = "lm"),
  list(var = "roach_acc_max", formula = log(roach_acc_max) ~ one_kT + log(roach_g), model = "lm"),
  list(var = "roach_acc_q50", formula = log(roach_acc_q50) ~ one_kT + log(roach_g), model = "lm")
)

pursuit_vars <- list(
  list(var = "snout_roach_dist_q50", formula = log(snout_roach_dist_q50) ~ one_kT + log(roach_g) + (1|animal), model = "lmer"),
  list(var = "strike_rate", formula = log(strike_rate) ~ one_kT + log(roach_g) + (1|animal), model = "lmer"),
  list(var = "strikes_n", formula = log(strikes_n) ~ one_kT + log(roach_g) + (1|animal), model = "lmer"),
  list(var = "elapsed_s", formula = log(elapsed_s) ~ one_kT + log(roach_g) + (1|animal), model = "lmer"),
  list(var = "mouse_dist_travel", formula = log(mouse_dist_travel) ~ one_kT + log(roach_g) + (1|animal), model = "lmer"),
  list(var = "roach_dist_travel", formula = log(roach_dist_travel) ~ one_kT + log(roach_g) + (1|animal), model = "lmer"),
  list(var = "mouse_vel_q50", formula = log(mouse_vel_q50) ~ one_kT + log(roach_g) + (1|arena), model = "lmer"),
  list(var = "mouse_vel_max", formula = log(mouse_vel_max) ~ one_kT + log(roach_g) + (1|arena), model = "lmer"),
  list(var = "mouse_acc_q50", formula = log(mouse_acc_q50) ~ one_kT + log(roach_g) + (1|arena), model = "lmer"),
  list(var = "mouse_acc_max", formula = log(mouse_acc_max) ~ one_kT + log(roach_g) + (1|arena), model = "lmer"),
  list(var = "roach_vel_q50", formula = log(roach_vel_q50) ~ one_kT + log(roach_g) + (1|arena), model = "lmer"),
  list(var = "roach_vel_max", formula = log(roach_vel_max) ~ one_kT + log(roach_g) + (1|arena), model = "lmer"),
  list(var = "roach_acc_q50", formula = log(roach_acc_q50) ~ one_kT + log(roach_g) + (1|animal), model = "lmer"),
  list(var = "roach_acc_max", formula = log(roach_acc_max) ~ one_kT + log(roach_g) + (1|arena), model = "lmer")
)
# Functions to tit and summarize a linear (lm) or mixed-effects model (lmer) for a single variable
#' Filters data for NA, infinite, or non-positive values before fitting.
#' Returns a tidy one-row summary table for the effect of one_kT, including confidence intervals, p-value, n, and both R² types.
#' @param formula Model formula (log(y) ~ predictors + (1|random)).
#' @param data Data frame to model.
#' @param varname Name of the variable being modeled (for summary table).
#' @param context Label for the model context ("in_pursuit" or "in_isolation").
#' @return Tidy data frame summarizing the slope of one_kT.

get_lmer_stats <- function(formula, data, varname, context) {
  vars <- all.vars(formula)
  response <- all.vars(formula[[2]])[1]   # TAKE ONLY THE FIRST VARIABLE
  data_clean <- data %>%
    dplyr::filter(
      !if_any(all_of(vars), ~ is.na(.) | is.infinite(.)) &
        .data[[response]] > 0
    )
  fit <- lmer(formula, data = data_clean)
  tidy_fit <- broom.mixed::tidy(fit, effects = "fixed", conf.int = TRUE)
  r2s <- performance::r2_nakagawa(fit)
  tidy_fit %>%
    filter(term == "one_kT") %>%
    mutate(
      var = varname,
      n = nrow(data_clean),
      r2_marginal = r2s$R2_marginal,
      r2_conditional = r2s$R2_conditional,
      model_type = "lmer",
      context = context
    ) %>%
    dplyr::select(var, context, term, estimate, conf.low, conf.high, std.error, statistic, p.value, n,
                  r2_marginal, r2_conditional, model_type)
}


get_lm_stats <- function(formula, data, varname, context) {
  vars <- all.vars(formula)
  response <- all.vars(formula[[2]])[1]  # always pulls out the true response variable name
  data_clean <- data %>%
    dplyr::filter(
      !if_any(all_of(vars), ~ is.na(.) | is.infinite(.)) &
        .data[[response]] > 0
    )
  fit <- lm(formula, data = data_clean)
  tidy_fit <- broom::tidy(fit, conf.int = TRUE)
  r2_val <- summary(fit)$r.squared
  tidy_fit %>%
    filter(term == "one_kT") %>%
    mutate(
      var = varname,
      n = nrow(data_clean),
      r2_marginal = r2_val,
      r2_conditional = NA_real_,
      model_type = "lm",
      context = context
    ) %>%
    dplyr::select(var, context, term, estimate, conf.low, conf.high, std.error, statistic, p.value, n,
                  r2_marginal, r2_conditional, model_type)
}

# Apply function

# Mouse in isolation (mixed models)
lmer_mouse_iso_summary <- bind_rows(
  lapply(iso_mouse_vars, function(x) get_lmer_stats(x$formula, pre_hunt30, x$var, "in_isolation"))
)

# Roach in isolation (linear models, NO random effect)
lm_roach_iso_summary <- bind_rows(
  lapply(iso_roach_vars, function(x) get_lm_stats(x$formula, roach_solo_30, x$var, "in_isolation"))
)


# Pursuit (mixed models)
lmer_pursuit_summary <- bind_rows(
  lapply(pursuit_vars, function(x) get_lmer_stats(x$formula, hunt_30, x$var, "in_pursuit"))
)


# Combine all summaries
final_summary <- bind_rows( lm_roach_iso_summary, lmer_mouse_iso_summary,lmer_pursuit_summary) %>% dplyr::select(var, context, everything())
final_summary <- final_summary %>%
  dplyr::mutate(
    dplyr::across(
      .cols = c(estimate, conf.low, conf.high, std.error, statistic, p.value, r2_marginal, r2_conditional),
      ~ signif(., 3)
    )
  )
final_summary
#write_csv(final_summary, "~/Downloads/fig4_summary.csv")



##########################################################
#--------------- Analyses for Figure 5 -------------------------------
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

response_vars <- c("elapsed_s", "strikes_n", "strike_rate", "mouse_dist_travel", "mouse_vel_q50", "snout_roach_dist_q50")


#' Model and contrast hot vs. cold trial effects for multiple response variables
#'
#' For each variable in response_vars, this loop:
#'   - Filters the data for non-missing, positive values and includes only "hot" and "cold" trials.
#'   - Fits a mixed-effects model: log(response) ~ trial temperature * hot/cold + covariates + (1|animal).
#'   - Estimates the marginal means for hot vs. cold using emmeans.
#'   - Calculates the pairwise contrast (difference) between hot and cold, including confidence intervals, t-value, p-value, and degrees of freedom.
#'   - Records the number of hot and cold trials for each variable.
#'   - Stores results in a tidy summary table with one row per response variable.
#' The final table (results_table) gives:
#'   - The estimated log difference (and CI) between hot and cold trials for each response,
#'   - t-statistic, degrees of freedom, p-value,
#'   - n for each group,
#'   - Columns rounded to 3 significant digits for reporting.
#'
#' Results are ordered by a specified variable list and saved as a CSV 
results_list <- list()
for (response in response_vars) {
  dat <- thermal_trials %>%
    filter(!is.na(.data[[response]]), .data[[response]] > 0, hot_cold %in% c("hot", "cold"))
  # Skip variable if there is not enough data
  if(nrow(dat) == 0) next
  
  formula <- as.formula(paste0("log(", response, ") ~ trial_at_temp * hot_cold + log(roach_g) + (1|animal)"))
  m <- lmer(formula, data = dat)
  means <- emmeans(m, specs = ~ hot_cold)
  contrast_tab <- summary(contrast(means, method = "pairwise"), infer = c(TRUE, TRUE))
  
  # Grab any row that has both hot and cold in the contrast
  comp_row <- contrast_tab %>% filter(grepl("hot", contrast) & grepl("cold", contrast))
  if (nrow(comp_row) == 0) comp_row <- contrast_tab[1, ]
  
  n_hot <- sum(dat$hot_cold == "hot")
  n_cold <- sum(dat$hot_cold == "cold")
  
  results_list[[response]] <- tibble(
    response_var = response,
    contrast = comp_row$contrast,
    estimate = comp_row$estimate,
    lower.CL = comp_row$lower.CL,
    upper.CL = comp_row$upper.CL,
    t.ratio = comp_row$t.ratio,
    df = comp_row$df,
    p_value = comp_row$p.value,
    n_hot = n_hot,
    n_cold = n_cold
  )
}

results_table <- bind_rows(results_list)
results_table <- results_table %>%
  mutate(
    estimate = signif(estimate, 3),
    lower.CL = signif(lower.CL, 3),
    upper.CL = signif(upper.CL, 3),
    t.ratio = signif(t.ratio, 3),
    p_value = signif(p_value, 3),
    df = round(df)
  )
desired_order <- c(
  "snout_roach_dist_q50",
  "mouse_vel_q50",
  "strike_rate",
  "strikes_n",
  "elapsed_s",
  "mouse_dist_travel"
)

results_table <- results_table %>%
  mutate(response_var = factor(response_var, levels = desired_order)) %>%
  arrange(response_var)
print(results_table, n = nrow(results_table))
#write_csv(results_table, "~/Downloads/thermal_results_table.csv")


#------------- Initial trials regression fits --------------
#' Fit and summarize initial training trial regressions for multiple response variables
#'
#' For each variable in response_vars, this code:
#'   - Optionally filters out zeros for variables where log-transform requires positive values.
#'   - Fits a mixed-effects regression: log(response) ~ initial trial temperature + log(roach_g) + (1|animal)
#'   - Extracts the coefficient (beta) for initial trial temperature, along with standard error, t-value, p-value, and degrees of freedom.
#'   - Computes 95% confidence intervals for the effect using broom.mixed::tidy.
#'   - Calculates both marginal and conditional R² for model fit.
#'   - Records sample size (n) used for each variable.
#'   - Stores all results in a tidy table, one row per response variable.
#'
#' The final table (results_table) summarizes:
#'   - Effect size and CI for trial temperature,
#'   - t-statistic, degrees of freedom, p-value,
#'   - Marginal and conditional R²,
#'   - n (sample size) for each model,
#'   - All values rounded for clarity.
#'
response_vars <- c("mouse_vel_q50", "snout_roach_dist_q50", "strikes_n", "strikes_n_per_s", "mouse_dist_travel", "elapsed_s")

# Create strikes_n_per_s column for convenience
training_trials <- training_trials %>%
  mutate(strikes_n_per_s = strikes_n / elapsed_s)

results_list <- list()

for (response in response_vars) {
  # Filter out zeros for certain variables if needed
  dat <- training_trials
  if (response %in% c("mouse_dist_travel", "elapsed_s")) {
    dat <- dat %>% filter(mouse_dist_travel > 0)
  }
  
  # Build the formula
  formula <- as.formula(paste0("log(", response, ") ~ trial_at_temp_init + log(roach_g) + (1|animal)"))
  
  # Fit model
  m <- lmer(formula, data = dat)
  smry <- summary(m)
  anova_tab <- anova(m)
  rsq <- suppressWarnings(r.squaredGLMM(m))
  
  # Get coefficient info for trial_at_temp_init
  coefs <- smry$coefficients
  beta <- coefs["trial_at_temp_init", "Estimate"]
  se <- coefs["trial_at_temp_init", "Std. Error"]
  tval <- coefs["trial_at_temp_init", "t value"]
  df <- smry$coefficients["trial_at_temp_init", "df"] %||% anova_tab["trial_at_temp_init", "DenDf"] %||% nrow(dat) - length(fixef(m))
  
  # Use 95% CI from broom.mixed::tidy for lmer objects (now using dplyr::select)
  ci <- tryCatch({
    broom.mixed::tidy(m, effects = "fixed", conf.int = TRUE) %>%
      filter(term == "trial_at_temp_init") %>%
      dplyr::select(conf.low, conf.high)
  }, error = function(e) tibble(conf.low = NA, conf.high = NA))
  
  pval <- tryCatch(anova_tab["trial_at_temp_init", "Pr(>F)"], error = function(e) NA)
  
  # R² (marginal, conditional)
  r2_marg <- signif(rsq[1], 3)
  r2_cond <- signif(rsq[2], 3)
  
  results_list[[response]] <- tibble(
    response_var = response,
    beta = signif(beta, 3),
    lower.CL = signif(ci$conf.low, 3),
    upper.CL = signif(ci$conf.high, 3),
    t = signif(tval, 3),
    df = round(as.numeric(df)),
    p_value = signif(pval, 3),
    R2_marg = r2_marg,
    R2_cond = r2_cond,
    n = nrow(dat)
  )
}

results_table <- bind_rows(results_list)
print(results_table, n = nrow(results_table))
#write_csv(results_table, "~/Downloads/training_trials_regression_results.csv")

#---------------------------------------------------------
#--------- Change in mean rates from 14C to 35 C --------
#---------------------------------------------------------

get_modeled_mean <- function(lmer_formula, data, var_label, transform = exp, raw_transform = identity, mean_label = "modeled_mean") {
  # Find all variables
  vars <- all.vars(lmer_formula)
  # Find the response variable
  response <- all.vars(lmer_formula[[2]])[1]
  # Clean data
  data_clean <- data %>%
    dplyr::filter(
      !if_any(all_of(vars), ~ is.na(.) | is.infinite(.)) &
        .data[[response]] > 0
    )
  fit <- lmer(lmer_formula, data = data_clean)
  emm <- summary(emmeans(fit, ~1, data = data_clean))  # <--- pass data_clean here!
  tibble::tibble(
    variable      = var_label,
    modeled_mean  = transform(emm$emmean),
    modeled_SE    = if ("SE" %in% names(emm)) transform(emm$SE) else NA_real_,
    lower_CI      = transform(emm$lower.CL),
    upper_CI      = transform(emm$upper.CL),
    raw_mean      = raw_transform(mean(data_clean[[response]], na.rm = TRUE))
  )
}

# Snout-roach distance (in cm)
dist_stats <- get_modeled_mean(
  log(snout_roach_dist_q50) ~ one_kT + log(roach_g) + (1|animal),
  hunt_30,
  var_label = "snout_roach_dist_q50",
  transform = exp
)

# Strike interval (mean 1/rate, with CIs)
strike_stats <- get_modeled_mean(
  log(strike_rate) ~ one_kT + log(roach_g) + (1|animal),
  hunt_30,
  var_label = "strike_interval",
  transform = function(x) 1/exp(x),
  raw_transform = function(x) 1/mean(x, na.rm = TRUE)
)
get_fold_stats <- function(varname, data_14, data_35) {
  # Clean and fit for both temps
  filter_clean <- function(df, var) {
    df %>% dplyr::filter(
      !is.na(.data[[var]]),
      is.finite(.data[[var]]),
      .data[[var]] > 0,
      !is.na(roach_g),
      is.finite(roach_g)
    )
  }
  d14 <- filter_clean(data_14, varname)
  d35 <- filter_clean(data_35, varname)
  fit_14 <- lmer(as.formula(paste0("log(", varname, ") ~ one_kT + log(roach_g) + (1|animal)")), data = d14)
  fit_35 <- lmer(as.formula(paste0("log(", varname, ") ~ one_kT + log(roach_g) + (1|animal)")), data = d35)
  emm_14 <- summary(emmeans(fit_14, ~1))
  emm_35 <- summary(emmeans(fit_35, ~1))
  
  tibble::tibble(
    variable      = varname,
    temp          = c("14", "35"),
    modeled_mean  = c(exp(emm_14$emmean), exp(emm_35$emmean)),
    modeled_SE    = c(exp(emm_14$SE), exp(emm_35$SE)),
    lower_CI      = c(exp(emm_14$lower.CL), exp(emm_35$lower.CL)),
    upper_CI      = c(exp(emm_14$upper.CL), exp(emm_35$upper.CL)),
    raw_mean      = c(mean(d14[[varname]], na.rm = TRUE), mean(d35[[varname]], na.rm = TRUE)),
    fold_increase = c(NA, exp(emm_35$emmean) / exp(emm_14$emmean))
  )
}
# Modeled means for hunt_30
mean_results <- dplyr::bind_rows(
  get_modeled_mean(log(snout_roach_dist_q50) ~ one_kT + log(roach_g) + (1|animal), hunt_30, "snout_roach_dist_q50", exp),
  get_modeled_mean(log(strike_rate) ~ one_kT + log(roach_g) + (1|animal), hunt_30, "strike_interval", function(x) 1/exp(x), function(x) 1/mean(x, na.rm = TRUE))
) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = c(modeled_mean, modeled_SE, lower_CI, upper_CI, raw_mean),
      ~ signif(., 3)
    )
  )

# XFold change from 14C to 35C
fold_results <- dplyr::bind_rows(
  get_fold_stats("strikes_n", filter(hunt_30, targ_temp == "14"), filter(hunt_35, targ_temp == "35")),
  get_fold_stats("elapsed_s", filter(hunt_30, targ_temp == "14"), filter(hunt_35, targ_temp == "35")),
  get_fold_stats("mouse_dist_travel", filter(hunt_30, targ_temp == "14"), filter(hunt_35, targ_temp == "35"))
) %>%
  dplyr::mutate(
    dplyr::across(
      .cols = c(modeled_mean, modeled_SE, lower_CI, upper_CI, raw_mean),
      ~ signif(., 3)
    )
  )
print(mean_results)
print(fold_results)



#####################################################################
#####                     Mismatch Analysis          ########################
#####################################################################

# Mismatch trials
mismatch = hunt_results %>% filter(mismatch_trial == T) %>%
  dplyr::select(date, animal, trial_daily, mismatch, targ_temp, trial_temp, trial_at_temp, everything())

mismatch_cold = mismatch %>% filter(targ_temp == 14)
mismatch_warm = mismatch %>% filter(targ_temp == 30) #trial_temp

# Compare 1st trial of day only
mismatch_cold_1st = mismatch_cold %>% filter(trial_daily ==  "1")
mismatch_warm_1st = mismatch_warm %>% filter(trial_daily ==  "1")

# Function to extract p-values and Cohen's d (with confidence intervals) for mismatch effects
#'#' This function loops over a set of response variables and fits mixed-effects models 
#' (with repeated measures) for "cold" and "warm" datasets separately. For each model, 
#' it extracts t statistics, degrees of freedom, exact p-values, and effect size 
#' (Cohen's d with CI). All numerical results are formatted to three significant digits 
#' for reporting
#' 
extract_p_values_and_cohens_d_with_ci <- function(response_vars, data_cold, data_warm, conf_level = 0.95) {
  results_list <- list()
  
  cohens_d <- function(model, data, conf_level = 0.95) {
    
    # Estimate marginal means for mismatch factor
    emm <- emmeans(model, ~ mismatch)
    means <- summary(emm)$emmean
    
    # Pooled SD from model residuals (robust to sample size differences)
    pooled_sd <- sqrt(mean(resid(model)^2))
    
    # Cohen's d: standardized mean difference (model-adjusted means)
    d_value <- (means[2] - means[1]) / pooled_sd
   
     # Group sizes
    n1 <- sum(data$mismatch == TRUE)
    n2 <- sum(data$mismatch == FALSE)
   
     # Standard error and confidence interval for Cohen's d
    se_d <- sqrt(((n1 + n2) / (n1 * n2)) + (d_value^2 / (2 * (n1 + n2))))
    alpha <- 1 - conf_level
    ci_lower <- d_value - qnorm(1 - alpha / 2) * se_d
    ci_upper <- d_value + qnorm(1 - alpha / 2) * se_d
   
     # Format CI as string with 3 significant digits for each bound
    return(list(effect_size = d_value, ci = paste0("(", format(signif(ci_lower, 3), trim = TRUE), 
                                                   ", ", format(signif(ci_upper, 3), trim = TRUE), ")")))
  }
  # Loop over each response variable
  for (response in response_vars) {
   
     # Build the formula string for the mixed-effects model
    formula <- as.formula(paste("log(pmax(", response, ", 1e-5)) ~ mismatch + log(roach_g) + (1|arena)", sep = ""))
    
    tryCatch({
    
        # Cold model
      model_cold <- lmer(formula, data = data_cold, REML = TRUE)
      cold_summary <- summary(model_cold)$coefficients["mismatchTRUE", ]
      t_cold <- cold_summary["t value"]
     
       # Degrees of freedom: n - number of fixed effects
      df_cold <- model_cold@devcomp$dims["n"] - length(fixef(model_cold))  # or use lmerTest to extract df
      p_cold <- cold_summary["Pr(>|t|)"]
      d_cold <- cohens_d(model_cold, data_cold)
      
      # Warm model
      model_warm <- lmer(formula, data = data_warm, REML = TRUE)
      warm_summary <- summary(model_warm)$coefficients["mismatchTRUE", ]
      t_warm <- warm_summary["t value"]
      df_warm <- model_warm@devcomp$dims["n"] - length(fixef(model_warm))
      p_warm <- warm_summary["Pr(>|t|)"]
      d_warm <- cohens_d(model_warm, data_warm)
      
      # Store result
      results_list[[response]] <- data.frame(
        response_var = response,
        t_cold = round(t_cold, 3),
        df_cold = df_cold,
        p_cold = format(signif(p_cold, 3), trim = TRUE),
        d_cold = format(signif(d_cold$effect_size, 3), trim = TRUE),
        d_CI_cold = d_cold$ci,
        t_warm = round(t_warm, 3),
        df_warm = df_warm,
        p_warm = format(signif(p_warm, 3), trim = TRUE),
        d_warm = format(signif(d_warm$effect_size, 3), trim = TRUE),
        d_CI_warm = d_warm$ci
      )
    }, error = function(e) {
      # If model fails, fill row with NAs
      results_list[[response]] <- data.frame(
        response_var = response,
        t_cold = NA, df_cold = NA, p_cold = NA, d_cold = NA, d_CI_cold = NA,
        t_warm = NA, df_warm = NA, p_warm = NA, d_warm = NA, d_CI_warm = NA
      )
    })
  }
  
  # Combine all rows to a single tibble 
  if (length(results_list) > 0) {
    results_df <- do.call(rbind, results_list)
    return(as_tibble(results_df))
  } else {
    return(NULL)
  }
}

# Response Variables
response_vars <- c("roach_vel_q50", "roach_vel_max", "roach_acc_q50", "roach_acc_max", "mouse_vel_q50", "mouse_vel_max", "mouse_acc_q50", "mouse_acc_max", "snout_roach_dist_q50", "strike_rate", "strikes_n", "elapsed_s", "mouse_dist_travel", "roach_dist_travel")

# Mismatch results table
mismatch_table <- extract_p_values_and_cohens_d_with_ci(response_vars, mismatch_cold_1st, mismatch_warm_1st)
print(mismatch_table)
#write_csv(mismatch_table, "~/Downloads/mismatch.csv")


