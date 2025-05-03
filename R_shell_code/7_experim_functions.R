library(tidyverse)
library(colorspace)

# Plotting arguments
temp_breaks = c(10, 20, 30, 40)
temp_limits = c(10, 40)
trial_labels = c("0", "20", "40", "60")
temp_scale <- c("14" = '#2c7bb6',"18" = '#abd9e9', "25" = 'gray70', "30" = '#fdae61', "35" = '#d7191c')
temp_scale2 <- c("13" = '#2c7bb6', "18" = '#abd9e9', "25" = 'gray70', "30" = '#fdae61', "35" = '#d7191c') #red runners coolest temperature is 13 C
temp_scale3 = c("14" = "#2c7bb6","18" = "#abd9e9", "25" = "gray40" , "30" = "#fdae61", "35" = "#d7191c") #darken the gray for visibility

temp_scale_dark = darken(temp_scale, 0.2)
temp_col <- darken(temp_scale, 0.6)
ecto_col = "dodgerblue3"
endo_col = "firebrick"

#Plotting Functions

theme_plot <- function(size1 = 18, size2 = 14, font = "Helvetica", color = "black", width  =.5, ratio = 0.75) { #small size1 = 16, large size1 =24
  theme(
    panel.grid = element_blank(), 
    aspect.ratio = ratio,
    axis.text = element_text(size = size1, color = "black"), 
    axis.ticks.length = unit(0.2, "cm"),
    axis.ticks = element_line(colour = "black"),
    axis.title = element_text(size = size1),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.x.top = element_text(margin = margin(b = 5)),
    plot.title = element_text(size = size1, hjust = 0.5),
    panel.border = element_rect(colour = color, fill = NA, linewidth = width),
    panel.background = element_blank(),
    strip.background = element_blank(),
    legend.position = "right",
    legend.key = element_rect(fill = "transparent"),
    legend.title = element_text(size = size2),
    legend.text = element_text(size = size2 - 2), # Slightly smaller than other texts
    text = element_text(family = font)
  )
}

theme_plot_small <- theme(panel.grid = element_blank(), # Plotting theme - default sucks, though theme_bw and theme_classic are useful in a pinch
                          aspect.ratio = .75,
                          axis.text = element_text(size = 16, color = "black"), 
                          axis.ticks.length = unit(0.2, "cm"),
                          axis.ticks = element_line(colour = "black"),
                          axis.title = element_text(size = 16),
                          axis.title.y = element_text(margin = margin(r = 10)),
                          axis.title.x = element_text(margin = margin(t = 10)),
                          axis.title.x.top = element_text(margin = margin(b = 5)),
                          plot.title = element_text(size = 16, hjust = 0.5),
                          panel.border = element_rect(colour = "black", fill = NA, linewidth = 1),
                          panel.background = element_blank(),
                          strip.background = element_blank(),
                          legend.position = "right",
                          legend.key = element_rect(fill = "transparent"),
                          legend.title = element_text(size = 12),
                          legend.text = element_text(size = 14),
                          text = element_text(family = "Helvetica"))

theme_no_x <- theme(axis.title.x = element_blank(),
                    axis.text.x = element_blank(),
                    axis.ticks.x = element_blank())
theme_no_y <- theme(axis.title.y = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank())

theme_reg = theme(axis.text = element_text(size = 16, color = "black"), 
                  axis.ticks.length = unit(0.2, "cm"),
                  axis.ticks = element_line(colour = "black"),
                  axis.title = element_text(size = 16),
                  axis.title.y = element_text(margin = margin(r = 10)),
                  axis.title.x = element_text(margin = margin(t = 10)),
                  axis.title.x.top = element_text(margin = margin(b = 5)))
scale_x_reg = scale_x_continuous(breaks = temp_breaks, limits = temp_limits, 
                                 name = expression("Ambient Temperature (ºC)")) 


#-----------Save figure function, with crop function ------------
save_fun = function(p = p, location, width = 10.25, height = 8) {
  # Assuming 'set_panel_size' and 'unit' are part of your plotting logic and 'save_path' is defined elsewhere.
  p1 <- set_panel_size(p, width = unit(width, "cm"), height = unit(height, "cm"))
  
  # Use the svg() function for SVG output
  svg(filename = file.path(save_path, location), width = width/2.54, height = height/2.54) # Dimensions in inches
  grid.newpage()
  grid.draw(p1)
  dev.off()
}
#------------Function to use mixed model (lmer) and generate predicted fit with SE and conf ban

temp_fun <- function(response, df, roach_g = TRUE) {
  # Construct newdata and model based on the roach_g setting
  if (roach_g) {
    roach_g_median <- rep(median(df$roach_g, na.rm = TRUE), length(df$roach_g))
    trial_temp <- seq(from = min(df$trial_temp), to = max(df$trial_temp), length.out = length(df$trial_temp))
    one_kT <- 1 / (8.617e-5 * (trial_temp + 273.15))
    newdata <- tibble(one_kT, response, roach_g = roach_g_median, animal = df$animal, trial_temp)
    lmer_x <- lme4::lmer(log(response) ~ log(roach_g) + one_kT + (1 | animal), data = df)
  } else {
    trial_temp <- seq(from = min(df$trial_temp), to = max(df$trial_temp), length.out = length(df$trial_temp))
    one_kT <- 1 / (8.617e-5 * (trial_temp + 273.15))
    newdata <- tibble(one_kT, response, animal = df$animal, trial_temp)
    lmer_x <- lme4::lmer(log(response) ~ one_kT + (1 | animal), data = df)
  }
  
  # Define prediction function
  pred_fun <- function(mm) { 
    predict(mm, newdata = newdata, re.form = NA, type = 'response')
  }
  
  # Generate predictions and confidence intervals
  newdata$predict <- exp(predict(lmer_x, newdata = newdata, re.form = NA))
  boot <- bootMer(lmer_x, FUN = pred_fun, nsim = 100, type = 'parametric', use.u = TRUE)
  predBootB <- t(apply(boot$t, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.975)))
  newdata$lci <- as.numeric(exp(predBootB[, 1]))  # Lower confidence interval
  newdata$uci <- as.numeric(exp(predBootB[, 2]))  # Upper confidence interval
  
  return(newdata)
}

plot_temp_col <- function(df = df1, y_name, response, breaks, limits, labels, col, height = 8, line_limit = 30, 
                          position = "left", theme_x = theme_reg, theme_y = theme_reg, scale_x = scale_x_reg,  
                          add_smooth = TRUE, box = FALSE) {
  p <- ggplot() +
    labs(color = "Temperature") +
    scale_x_continuous(name = "Ambient Temperature (ºC)", limits = temp_limits, breaks = temp_breaks) +
    scale_color_manual(values = temp_col) +
    scale_fill_manual(values = temp_scale) +
    scale_y_log10(name = y_name, limits = limits, breaks = breaks, labels = labels, position = position) + 
    geom_point(data = df, aes(x = trial_temp_plot, y = response, fill = as.factor(targ_temp)),
               color = "black", shape = 21, size = 2.25, stroke = 0.5, show.legend = FALSE) +
    geom_ribbon(data = newdata %>% filter(trial_temp <= line_limit), 
                aes(x = trial_temp, ymax = uci, ymin = lci), fill = col, color = NA, alpha = 0.25, show.legend = FALSE) +
    geom_line(data = newdata %>% filter(trial_temp <= line_limit), 
              aes(y = predict, x = trial_temp), color = col, show.legend = FALSE, linewidth = 1.25) +
    theme_y + theme_x + theme_plot() +
    theme(dim)
  # Add boxplot conditionally
  if (box) {
    p <- p + geom_boxplot(data = df, aes(x = targ_temp, y = response, color = as.factor(targ_temp)),
                          fill = NA, outlier.shape = NA, show.legend = FALSE)
  }
  # Add smooth line conditionally
  if (add_smooth) {
    p <- p + stat_smooth(data = df, aes(x = trial_temp, y = !!response), 
                         color = col, fill = col, linewidth = 1, linetype = "dashed", se = FALSE)
  }
  return(p)
}

#initial trials 
trial_init_fun = function(response, df) {
  pred_fun <- function(mm) { 
    predict(mm, newdata = newdata, re.form = NA, type = 'response')
  }
  lmer_x = lme4::lmer(log(response) ~ log(trial_at_temp_init)  + (1|animal), data = df) 
  trial_at_temp_init = seq(from = min(df$trial_at_temp_init), to = max(df$trial_at_temp_init), length.out = length(df$trial_at_temp_init))
  animal = df$animal
  newdata = df %>% dplyr::select(trial_at_temp_init, animal) #%>% distinct() # preserves original structure
  newdata$predict = exp(predict(lmer_x, newdata = newdata, re.form = NA))
  boot <- bootMer(lmer_x, FUN = pred_fun, nsim = 100, type = 'parametric', use.u = T)# use.u = TRUE, # need to use use.u = TRUE, otherwise it spits out prediction intervals
  predBootB <- t(apply(boot$t, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.975)))
  newdata$lci = as.numeric(exp(predBootB[,1]))# lower bound of the confidence band
  newdata$uci =  as.numeric(exp(predBootB[,2]))# upper bound of the confidence band
  return(newdata)
}

plot_trial_init = function(df = df,  y_name, ratio = 1, x_name = trial_name,  x_labels = trial_labels, newdata = newdata, response,
                           y_breaks, y_limits, y_labels, col, height = 5, theme_x = theme_reg, theme_y = theme_reg, scale_x = scale_x_trial) {
  ggplot() +
    scale_color_manual(values = temp_scale) +
    labs(color = "Temperature") +
    geom_point(data = df, aes(x = trial_at_temp_init, y = response), color = "gray70", 
               shape = 21, size = 1, stroke = .5, show.legend = F) +
    scale_y_log10(name = y_name, limits = y_limits, breaks = y_breaks, labels =y_labels) + 
    new_scale_color() +
    scale_fill_manual(values = temp_scale) +
    scale_color_manual(values = temp_scale_dark) +
    geom_ribbon(data = newdata, aes(x = trial_at_temp_init, ymax = uci, ymin = lci), fill =  "gray40",
                color = NA, alpha = 0.25, show.legend = F) +
    geom_line(data = newdata, aes(x = trial_at_temp_init, y = predict), color =  "gray40",
              linewidth = 1, show.legend = F) +
    scale_x_continuous(name = x_name, breaks = c(0, 20, 40,  60), labels = x_labels) + #range(df_35$trial_tot, na.rm = T)
    theme_y  + theme_x  + theme_plot(ratio = ratio, size1  =12)
}

#-------changes over trial - mixed model fit for all temps excluding initial trials

trial_temp_fun = function(response, df) {
  pred_fun <- function(mm) { 
    predict(mm, newdata = newdata, re.form = NA, type = 'response')
  }
  lmer_x = lme4::lmer(log(response) ~ trial_at_temp * targ_temperature + (1|animal), data = df) 
  trial_at_temp = seq(from = min(df$trial_at_temp), to = max(df$trial_at_temp), length.out = length(df$trial_at_temp))
  animal = df$animal
  targ_temperature = df$targ_temperature
  newdata = df %>% dplyr::select(targ_temperature, trial_at_temp, animal) #%>% distinct() # preserves original structure
  newdata$predict = exp(predict(lmer_x, newdata = newdata, re.form = NA))
  boot <- bootMer(lmer_x, FUN = pred_fun, nsim = 100, type = 'parametric', use.u = T)# use.u = TRUE, # need to use use.u = TRUE, otherwise it spits out prediction intervals
  predBootB <- t(apply(boot$t, MARGIN = 2, FUN = quantile, probs = c(0.025, 0.975)))
  newdata$lci = as.numeric(exp(predBootB[,1]))# lower bound of the confidence band
  newdata$uci =  as.numeric(exp(predBootB[,2]))# upper bound of the confidence band
  return(newdata)
}

plot_trial_temp = function(df = df,  y_name, x_name = trial_name, trial_breaks = t_breaks, ratio = 1, x_labels = t_labels, newdata = newdata, response,
                           y_breaks, y_limits, y_labels, col, height = 5, theme_x = theme_reg, theme_y = theme_reg, scale_x = scale_x_trial) {
  ggplot() +
    labs(color = "Temperature") +
    scale_fill_manual(values = temp_scale) +
    scale_color_manual(values = temp_scale) +
    geom_point(data = df, aes(x = trial_at_temp, y = response, color = targ_temperature), 
               shape = 21, size = 1, stroke = .5, fill = NA, show.legend = F) +
    scale_y_log10(name = y_name, limits = y_limits, breaks = y_breaks, labels =y_labels) + 
    new_scale_color() +
    scale_color_manual(values = temp_scale3) +
    geom_ribbon(data = newdata, aes(x = trial_at_temp, ymax = uci, ymin = lci, fill = targ_temperature), 
                color = NA, alpha = 0.25, show.legend = F) +
    geom_line(data = newdata, aes(x = trial_at_temp, y = predict, color = targ_temperature),
              linewidth = 1, show.legend = F) +
    scale_color_manual(values = temp_scale3) +
    scale_x_continuous(name = x_name, breaks = trial_breaks, labels = x_labels) + #range(df_35$trial_tot, na.rm = T)
    theme_y  + theme_x  + theme_plot(ratio = ratio, size1 = 12)
}


NA_outliers_temp <- function(df, columns_to_change, coef = 6) {
  # Function to compute bounds within each targ_temp group and apply coef
  df <- df %>% 
    group_by(targ_temp) %>%
    mutate(across(all_of(columns_to_change), ~ {
      data <- .x
      Q1 <- quantile(data, 0.25, na.rm = TRUE)
      Q3 <- quantile(data, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - coef * IQR
      upper_bound <- Q3 + coef * IQR
      
      # Print bounds for each targ_temp and column
      cat(sprintf("targ_temp: %s, Column: %s, Lower bound: %f, Upper bound: %f\n",
                  unique(targ_temp), cur_column(), lower_bound, upper_bound))
      
      # Replace outliers with NA
      ifelse(data < lower_bound | data > upper_bound, NA_real_, data)
    })) %>%
    ungroup()
  
  return(df)
}


NA_outliers_trial <- function(df, columns_to_change, coef = 1.5) {
  # Function to compute bounds within each trial group and apply coef
  df <- df %>% 
    group_by(id) %>%
    mutate(across(all_of(columns_to_change), ~ {
      data <- .x
      Q1 <- quantile(data, 0.25, na.rm = TRUE)
      Q3 <- quantile(data, 0.75, na.rm = TRUE)
      IQR <- Q3 - Q1
      lower_bound <- Q1 - coef * IQR
      upper_bound <- Q3 + coef * IQR
      
      # Print bounds for each trial and column
      cat(sprintf("id: %s, Column: %s, Lower bound: %f, Upper bound: %f\n",
                  unique(id), cur_column(), lower_bound, upper_bound))
      
      # Replace outliers with NA
      ifelse(data < lower_bound | data > upper_bound, NA_real_, data)
    })) %>%
    ungroup()
  
  return(df)
}

#--------- Mouse-Red Runner Contact Functions


##############################################################
##############################################################
##############################################################

n_contact <- function(test_vector, n = 10, skip = TRUE) {
  len <- length(test_vector)
  result_vector <- rep(NA, len)  # Initialize the result vector with NAs
  sequence_number <- 0  # Initialize sequence number
  in_sequence <- FALSE
  sequence_start <- NA  # Track start of current NA sequence
  first_sequence_skipped <- !skip  # If skip is FALSE, act as if first sequence is already skipped
  
  for (i in 1:len) {
    if (is.na(test_vector[i])) {
      if (!in_sequence) {  # Start of a new NA sequence
        in_sequence <- TRUE
        sequence_start <- i
      }
      
      # At end of vector or end of sequence, check length and mark if needed
      if (i == len || !is.na(test_vector[i + 1])) {
        if (in_sequence && (i - sequence_start + 1) >= n) {
          if (skip && !first_sequence_skipped && sequence_start == 1) {
            first_sequence_skipped <- TRUE  # Skip first sequence
          } else {
            sequence_number <- sequence_number + 1  # Increment sequence number
            result_vector[sequence_start:i] <- sequence_number
          }
        }
        in_sequence <- FALSE
      }
    }
  }
  
  return(result_vector)
}

#--------------Contact Fucntion
is_contact <- function(vec, n = 3, skip = TRUE) {
  len <- length(vec)
  result <- rep(0, len)  # Initialize the result vector with 0s
  count_na <- 0  # Counter for consecutive NAs
  skip_first_sequence <- skip && is.na(vec[1])  # Determine if the first sequence should be skipped
  first_na_sequence <- TRUE  # Flag to indicate the first sequence of NAs
  
  for (i in 1:len) {
    if (is.na(vec[i])) {
      count_na <- count_na + 1  # Increment NA count
      if (i == len && count_na >= n) {  # If at the end of the vector and have a qualifying NA sequence
        if (!skip_first_sequence || (skip_first_sequence && !first_na_sequence)) {
          result[(i - count_na + 1):i] <- 1
        }
      }
    } else {
      if (count_na >= n) {  # If the NA count meets or exceeds 'n'
        if (!skip_first_sequence || (skip_first_sequence && !first_na_sequence)) {
          result[(i - count_na):(i - 1)] <- 1
        }
      }
      if (first_na_sequence && count_na > 0) {
        first_na_sequence <- FALSE  # Update flag after the first NA sequence is processed
      }
      count_na <- 0  # Reset NA count
    }
  }
  
  return(result)
}

