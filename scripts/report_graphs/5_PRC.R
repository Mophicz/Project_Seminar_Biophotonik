# ==============================================================================
# Polished PR Analysis: Method Comparison (Individual Plots)
# Description: Calculates Z-scores and generates individual PR curves.
#              UPDATED: Ported from base R (PRROC) to ggplot2.
#              UPDATED: Exact aesthetic match to 4_ROC.R (sizing, fonts, layout).
#              UPDATED: Added a baseline chance line (prevalence).
# ==============================================================================

rm(list = ls())

library(ggplot2)
library(dplyr)
library(here)
library(readr)
library(PRROC)
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
# 3. Visualization Setup
# ==============================================================================

# Standardized colors to match ROC analysis exactly
plot_configs <- list(
  list(col = "z_mean",   label = "Mean/SD",         color = "firebrick",       file = "05_pr_mean_sd.pdf"),
  list(col = "z_median", label = "Median/MAD",      color = "dodgerblue4",     file = "05_pr_median_mad.pdf"),
  list(col = "z_iqr",    label = "Median/IQR",      color = "black",           file = "05_pr_median_iqr.pdf"),
  list(col = "z_trim",   label = "Trimmed Mean",    color = "darkorchid4",     file = "05_pr_trimmed.pdf"),
  list(col = "z_wins",   label = "Winsorized Mean", color = "orange",          file = "05_pr_winsorized.pdf")
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

generate_single_pr <- function(response, predictor, config) {
  
  # PRROC requires separate vectors for the positive (class 0) and negative (class 1) classes
  scores_outlier <- predictor[response == TRUE]
  scores_normal  <- predictor[response == FALSE]
  
  # Calculate PR Curve
  pr <- pr.curve(
    scores.class0 = scores_outlier,
    scores.class1 = scores_normal,
    curve = TRUE
  )
  
  # Extract Data for ggplot
  # PRROC curve matrix: Col 1 = Recall, Col 2 = Precision, Col 3 = Threshold
  plot_data <- data.frame(
    recall = pr$curve[, 1],
    precision = pr$curve[, 2]
  )
  
  # The baseline for a PR curve is the prevalence of positive cases
  baseline_chance <- mean(response == TRUE)
  
  label_text <- paste0("AUPRC = ", round(pr$auc.integral, 3))
  
  p <- ggplot(plot_data, aes(x = recall, y = precision)) +
    
    # 1. Horizontal Baseline (Chance) Line
    geom_hline(yintercept = baseline_chance, linetype = "dashed", color = "grey60") +
    
    # 2. PR Curve Line
    geom_path(color = config$color, linewidth = 1.2) +
    
    # 3. AUPRC Label
    annotate("text", x = 0.5, y = 0.15, label = label_text, 
             size = 2.5, color = config$color, family = "Arial") +
    
    # 4. Axes & Limits (Unlike ROC, Recall goes 0 to 1 normally)
    scale_x_continuous(limits = c(0, 1), expand = expansion(mult = 0.03), name = "Recall") +
    scale_y_continuous(limits = c(0, 1), expand = expansion(mult = 0.03), name = "Precision") +
    
    # 5. Aspect Ratio & Clipping
    coord_fixed(ratio = 1, clip = "off") +
    
    theme_report()
  
  return(p)
}

# ==============================================================================
# 4. Loop & Export
# ==============================================================================

message("Generating individual PR plots...")

for (cfg in plot_configs) {
  
  message(paste("Processing:", cfg$label))
  
  p <- generate_single_pr(
    response = df_calc$is_outlier,
    predictor = df_calc[[cfg$col]],
    config = cfg
  )
  
  save_path <- file.path(output_dir, cfg$file)
  ggsave(save_path, plot = p, device = cairo_pdf, width = 2.1, height = 2.1)
}

message("All PR curves exported successfully.")