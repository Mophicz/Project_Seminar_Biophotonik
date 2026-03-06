# ==============================================================================
# Polished MCC Analysis: Method Comparison 
# Description: Calculates Z-scores and generates two combined MCC curve plots:
#              1. Mean/SD vs Median/MAD
#              2. Median/IQR, Trimmed Mean (10%), Winsorized Mean (10%)
#              UPDATED: Evaluates absolute Z-scores (|Z|) across thresholds.
#              UPDATED: Identifies and plots the optimal threshold lines.
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

message("Calculating Z-scores for all 5 methods...")

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

df_calc <- df %>%
  group_by(conc) %>%
  mutate(
    z_mean   = (signal_out - mean(signal_out, na.rm = TRUE)) / sd(signal_out, na.rm = TRUE),
    z_median = (signal_out - median(signal_out, na.rm = TRUE)) / mad(signal_out, na.rm = TRUE),
    z_iqr    = (signal_out - median(signal_out, na.rm = TRUE)) / (IQR(signal_out, na.rm = TRUE) / 1.349),
    
    trim_stats = list(calc_trim_params(signal_out, trim = 0.1)),
    z_trim     = (signal_out - trim_stats[[1]]$mean) / trim_stats[[1]]$sd,
    
    wins_stats = list(calc_wins_params(signal_out, trim = 0.1)),
    z_wins     = (signal_out - wins_stats[[1]]$mean) / wins_stats[[1]]$sd
  ) %>%
  ungroup()

# ==============================================================================
# 3. Visualization Setup & MCC Logic
# ==============================================================================

# Group 1: The classic vs robust comparison
plot1_configs <- list(
  list(col = "z_mean",   label = "Mean/SD",    color = "firebrick"),
  list(col = "z_median", label = "Median/MAD", color = "dodgerblue4")
)

# Group 2: The alternative robust methods
plot2_configs <- list(
  list(col = "z_iqr",    label = "Median/IQR",      color = "black"),
  list(col = "z_trim",   label = "Trimmed Mean",    color = "darkorchid4"),
  list(col = "z_wins",   label = "Winsorized Mean", color = "orange")
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
      
      plot.margin = margin(t = 15, r = 15, b = 5, l = 5),
      
      # Legend styling for a tight paper column
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0)
    )
}

# Function to purely calculate MCC data without plotting
calculate_mcc_data <- function(truth, score_raw, label) {
  score <- abs(score_raw)
  thresholds <- sort(unique(score))
  mcc_vals <- numeric(length(thresholds))
  
  for (i in seq_along(thresholds)) {
    pred <- score >= thresholds[i]
    
    TP <- sum(pred & truth)
    FP <- sum(pred & !truth)
    FN <- sum(!pred & truth)
    TN <- sum(!pred & !truth)
    
    denom <- as.numeric(TP + FP) * as.numeric(TP + FN) * as.numeric(TN + FP) * as.numeric(TN + FN)
    
    if (denom > 0) {
      mcc_vals[i] <- (TP * TN - FP * FN) / sqrt(denom)
    } else {
      mcc_vals[i] <- NA 
    }
  }
  
  data.frame(
    threshold = thresholds,
    mcc = mcc_vals,
    Method = label
  ) %>% drop_na()
}

# ==============================================================================
# 4. Generate Plot Function & Export
# ==============================================================================

generate_mcc_plot <- function(config_list, output_filename) {
  
  # Extract colors for ggplot scale dynamically based on the provided configs
  method_colors <- setNames(sapply(config_list, `[[`, "color"), 
                            sapply(config_list, `[[`, "label"))
  
  # Calculate data
  all_mcc_data <- lapply(config_list, function(cfg) {
    calculate_mcc_data(truth = df_calc$is_outlier, 
                       score_raw = df_calc[[cfg$col]], 
                       label = cfg$label)
  }) %>% bind_rows()
  
  # Identify optimal threshold for each method (to draw vertical lines)
  opt_thresholds <- all_mcc_data %>%
    group_by(Method) %>%
    slice_max(mcc, n = 1, with_ties = FALSE) %>%
    ungroup()
  
  p <- ggplot(all_mcc_data, aes(x = threshold, y = mcc, color = Method)) +
    
    # 1. Optimal Threshold Lines (dashed)
    geom_vline(data = opt_thresholds, 
               aes(xintercept = threshold, color = Method), 
               linetype = "dashed", alpha = 0.6) +
    
    # 2. MCC Curves
    geom_path(linewidth = 1.0) +
    
    # 3. Colors and Scales
    scale_color_manual(values = method_colors) +
    scale_x_continuous(limits = c(0, 5), expand = expansion(mult = 0.03), name = "Threshold") +
    scale_y_continuous(limits = c(-1, 1), expand = expansion(mult = 0.03), name = "MCC") +
    
    coord_cartesian(clip = "off") +
    theme_report()
  
  save_path <- file.path(output_dir, output_filename)
  ggsave(save_path, plot = p, device = cairo_pdf, width = 3.4, height = 2.8)
  
  message(paste("   -> Exported successfully to:", save_path))
}

message("Generating combined MCC plots...")

generate_mcc_plot(plot1_configs, "06_mcc_mean_median.pdf")
generate_mcc_plot(plot2_configs, "06_mcc_robust_others.pdf")

message("All MCC workflows complete.")