# ==============================================================================
# Polished ROC Analysis: Method Comparison (Individual Plots)
# Description: Calculates Z-scores and generates individual ROC plots.
#              FIX: Removed plot titles (redundant with LaTeX subcaptions).
# ==============================================================================

rm(list = ls())

library(ggplot2)
library(dplyr)
library(here)
library(readr)
library(pROC)
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

plot_configs <- list(
  list(col = "z_mean",   label = "Mean/SD",         color = "black",       file = "06_roc_01_mean_sd.pdf"),
  list(col = "z_median", label = "Median/MAD",      color = "dodgerblue4", file = "06_roc_02_median_mad.pdf"),
  list(col = "z_iqr",    label = "Median/IQR",      color = "#2ecc71",     file = "06_roc_03_median_iqr.pdf"),
  list(col = "z_trim",   label = "Trimmed Mean",    color = "firebrick",   file = "06_roc_04_trimmed.pdf"),
  list(col = "z_wins",   label = "Winsorized Mean", color = "orange",      file = "06_roc_05_winsorized.pdf")
)

theme_report <- function() {
  theme_minimal(base_size = 14) +
    theme(
      axis.title = element_text(face = "bold", size = 12),
      panel.grid.minor = element_blank(),
      # plot.title removed here as it is no longer used
      plot.margin = margin(t = 5, r = 5, b = 5, l = 5)
    )
}

generate_single_roc <- function(response, predictor, config) {
  
  # Calculate ROC
  r <- roc(response, predictor, direction = "<", quiet = TRUE)
  auc_val <- round(auc(r), 3)
  
  # Extract Data
  plot_data <- data.frame(
    spec = r$specificities,
    sens = r$sensitivities
  )
  
  label_text <- paste0("AUC = ", auc_val)
  
  p <- ggplot(plot_data, aes(x = spec, y = sens)) +
    
    # 1. Diagonal (Chance) Line
    geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1), 
                 linetype = "dashed", color = "grey60") +
    
    # 2. ROC Curve Line (No shading)
    geom_path(color = config$color, linewidth = 1.2) +
    
    # 3. AUC Label
    annotate("text", x = 0.25, y = 0.15, label = label_text, 
             size = 5, fontface = "bold", color = config$color) +
    
    # 4. Axes & Limits
    scale_x_reverse(limits = c(1, 0), expand = c(0, 0), name = "Specificity") +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0), name = "Sensitivity") +
    
    # 5. Aspect Ratio & Clipping
    coord_fixed(ratio = 1, clip = "off") +
    
    # REMOVED TITLE
    # labs(title = config$label) + 
    theme_report()
  
  return(p)
}

# ==============================================================================
# 4. Loop & Export
# ==============================================================================

message("Generating individual ROC plots...")

for (cfg in plot_configs) {
  
  message(paste("Processing:", cfg$label))
  
  p <- generate_single_roc(
    response = df_calc$is_outlier,
    predictor = df_calc[[cfg$col]],
    config = cfg
  )
  
  save_path <- file.path(output_dir, cfg$file)
  ggsave(save_path, plot = p, device = "pdf", width = 4, height = 4)
}

# ==============================================================================
# 5. Sigma 3 Statistics Table
# ==============================================================================

message("\n--- Performance at Threshold = 3 ---")

stats_table <- do.call(rbind, lapply(plot_configs, function(cfg) {
  
  r <- roc(df_calc$is_outlier, df_calc[[cfg$col]], direction = "<", quiet = TRUE)
  
  res <- coords(r, x = 3, input = "threshold", 
                ret = c("specificity", "sensitivity", "accuracy"),
                transpose = FALSE)
  
  if(nrow(res) > 1) res <- res[1, ]
  
  data.frame(
    Method = cfg$label,
    Specificity = round(res$specificity, 4),
    Sensitivity = round(res$sensitivity, 4),
    Accuracy = round(res$accuracy, 4)
  )
}))

print(stats_table)