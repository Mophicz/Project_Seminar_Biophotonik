# ==============================================================================
# Polished MCC Analysis: Method Comparison (Individual Plots)
# Description: Calculates Z-scores and generates individual MCC curves.
#              UPDATED: Ported from base R to ggplot2 for style consistency.
#              UPDATED: Evaluates absolute Z-scores (|Z|) across thresholds.
#              UPDATED: Identifies and plots the optimal threshold line.
#              UPDATED: Exact aesthetic match to 04_ROC.R and 05_PR.R.
# ==============================================================================

rm(list = ls())

library(ggplot2)
library(dplyr)
library(here)
library(readr)
library(tidyr)

# ==============================================================================
# 1. Setup & Data Loading
# ==============================================================================

data_path <- here("data", "df_sim_4.txt")
output_dir <- "figures"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

df <- read_table(data_path)

# ==============================================================================
# 2. Statistical Calculations (Vectorized)
# ==============================================================================

message("Calculating Z-scores for all methods...")

# Helper functions
calc_trim_params <- function(x, trim = 0.1) {
  t_mean <- mean(x, trim = trim, na.rm = TRUE)
  q_limits <- quantile(x, probs = c(trim, 1 - trim), na.rm = TRUE)
  trim_dat <- x[x >= q_limits[1] & x <= q_limits[2]]
  t_sd <- sd(trim_dat, na.rm = TRUE)
  return(list(mean = t_mean, sd = t_sd))
}

calc_wins_params <- function(x, trim = 0.1) {
  q_limits <- quantile(x, probs = c(trim, 1 - trim), na.rm = TRUE)
  wins_dat <- pmax(pmin(x, q_limits[2]), q_limits[1])
  return(list(mean = mean(wins_dat), sd = sd(wins_dat)))
}

# Calculate Stats
df_calc <- df %>%
  group_by(conc) %>%
  mutate(
    z_mean   = (signal_out - mean(signal_out)) / sd(signal_out),
    z_median = (signal_out - median(signal_out)) / mad(signal_out),
    z_iqr    = (signal_out - median(signal_out)) / (IQR(signal_out) / 1.349),
    
    trim_stats = list(calc_trim_params(signal_out, trim = 0.1)),
    z_trim     = (signal_out - trim_stats[[1]]$mean) / trim_stats[[1]]$sd,
    
    wins_stats = list(calc_wins_params(signal_out, trim = 0.1)),
    z_wins     = (signal_out - wins_stats[[1]]$mean) / wins_stats[[1]]$sd
  ) %>%
  ungroup()

# ==============================================================================
# 3. Visualization Setup & MCC Logic
# ==============================================================================

plot_configs <- list(
  list(col = "z_mean",   label = "Mean/SD",         color = "firebrick",       file = "06_mcc_mean_sd.pdf"),
  list(col = "z_median", label = "Median/MAD",      color = "dodgerblue4",     file = "06_mcc_median_mad.pdf"),
  list(col = "z_iqr",    label = "Median/IQR",      color = "black",           file = "06_mcc_median_iqr.pdf"),
  list(col = "z_trim",   label = "Trimmed Mean",    color = "darkorchid4",     file = "06_mcc_trimmed.pdf"),
  list(col = "z_wins",   label = "Winsorized Mean", color = "orange",          file = "06_mcc_winsorized.pdf")
)

theme_report <- function() {
  theme_minimal(base_family = "Arial", base_size = 10) +
    theme(
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8), 
      
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.5),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.5), 
      panel.grid.minor = element_blank(),
      
      plot.margin = margin(t = 15, r = 20, b = 5, l = 5)
    )
}

generate_single_mcc <- function(truth, score_raw, config) {
  
  # MCC requires absolute scores to test two-tailed thresholds
  score <- abs(score_raw)
  thresholds <- sort(unique(score))
  
  # Pre-allocate MCC vector
  mcc_vals <- numeric(length(thresholds))
  
  # Calculate MCC for every threshold
  for (i in seq_along(thresholds)) {
    pred <- score >= thresholds[i]
    
    TP <- sum(pred & truth)
    FP <- sum(pred & !truth)
    FN <- sum(!pred & truth)
    TN <- sum(!pred & !truth)
    
    # Cast to numeric to prevent integer overflow in large datasets
    denom <- as.numeric(TP + FP) * as.numeric(TP + FN) * as.numeric(TN + FP) * as.numeric(TN + FN)
    
    if (denom > 0) {
      mcc_vals[i] <- (TP * TN - FP * FN) / sqrt(denom)
    } else {
      mcc_vals[i] <- NA # Set to NA if undefined, matching standard behavior
    }
  }
  
  plot_data <- data.frame(
    threshold = thresholds,
    mcc = mcc_vals
  ) %>% drop_na()
  
  # Identify optimal threshold
  idx_opt_mcc <- which.max(plot_data$mcc)
  thresh_opt_mcc <- plot_data$threshold[idx_opt_mcc]
  max_mcc_val <- plot_data$mcc[idx_opt_mcc]
  
  p <- ggplot(plot_data, aes(x = threshold, y = mcc)) +
    
    # 1. Optimal Threshold Line
    geom_vline(xintercept = thresh_opt_mcc, linetype = "dashed", color = "grey60") +
    
    # 2. MCC Curve
    geom_path(color = config$color, linewidth = 1.2) +
    
    # 4. Axes & Limits
    scale_x_continuous(limits = c(0, 5), expand = expansion(mult = 0.03), name = "Threshold") +
    scale_y_continuous(limits = c(-1, 1), expand = expansion(mult = 0.03), name = "MCC") +
    
    # Note: coord_fixed is removed here because X and Y scales are drastically different
    coord_cartesian(clip = "off") +
    
    theme_report()
  
  return(p)
}

# ==============================================================================
# 4. Loop & Export
# ==============================================================================

message("Generating individual MCC plots...")

for (cfg in plot_configs) {
  
  message(paste("Processing:", cfg$label))
  
  p <- generate_single_mcc(
    truth = df_calc$is_outlier,
    score_raw = df_calc[[cfg$col]],
    config = cfg
  )
  
  save_path <- file.path(output_dir, cfg$file)
  ggsave(save_path, plot = p, device = cairo_pdf, width = 2.1, height = 2.1)
}

message("All MCC curves exported successfully.")