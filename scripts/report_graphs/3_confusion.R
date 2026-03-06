# ==============================================================================
# Polished Confusion Matrix Analysis: Z-Score Threshold Evaluation
# Description: Evaluates classification performance (Threshold = +/- 3) 
#              by plotting confusion matrices for all 5 methods (10% trim).
#              UPDATED: Scaled for composite assembly (width = 2, height = 2)
#              UPDATED: Removed the outer black frame for cleaner tile display.
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
# 2. Statistical Calculations & Predictions
# ==============================================================================

message("Calculating Z-scores and applying threshold (|Z| > 3)...")

threshold <- 3

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
    z_wins     = (signal_out - wins_stats[[1]]$mean) / wins_stats[[1]]$sd,
    
    pred_mean   = abs(z_mean) > threshold,
    pred_median = abs(z_median) > threshold,
    pred_iqr    = abs(z_iqr) > threshold,
    pred_trim   = abs(z_trim) > threshold,
    pred_wins   = abs(z_wins) > threshold
  ) %>%
  ungroup()

# ==============================================================================
# 3. Visualization Setup
# ==============================================================================

plot_configs <- list(
  list(col = "pred_mean",   label = "Mean/SD",         color = "firebrick",   file = "03_conf_mean_sd.pdf"),
  list(col = "pred_median", label = "Median/MAD",      color = "dodgerblue4", file = "03_conf_median_mad.pdf"),
  list(col = "pred_iqr",    label = "Median/IQR",      color = "black",       file = "03_conf_median_iqr.pdf"),
  list(col = "pred_trim",   label = "Trimmed Mean",    color = "darkorchid4", file = "03_conf_trimmed.pdf"),
  list(col = "pred_wins",   label = "Winsorized Mean", color = "orange",      file = "03_conf_winsorized.pdf")
)

theme_report <- function() {
  theme_minimal(base_family = "Arial", base_size = 10) +
    theme(
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8), 
      
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      
      panel.border = element_blank(),
      panel.grid = element_blank(), 
      legend.position = "none"
    )
}

generate_conf_matrix <- function(data, truth_col, pred_col, config) {
  
  truth_vec <- as.logical(data[[truth_col]])
  pred_vec  <- as.logical(data[[pred_col]])
  
  lvls <- c(FALSE, TRUE)
  lbls <- c("Normal", "Outlier")
  
  truth_fct <- factor(truth_vec, levels = lvls, labels = lbls)
  pred_fct  <- factor(pred_vec, levels = lvls, labels = lbls)
  
  cm_data <- data.frame(Truth = truth_fct, Prediction = pred_fct) %>%
    group_by(Truth, Prediction) %>%
    summarise(Count = n(), .groups = "drop")
  
  grid <- expand.grid(Truth = lbls, Prediction = lbls)
  cm_data <- left_join(grid, cm_data, by = c("Truth", "Prediction"))
  cm_data$Count[is.na(cm_data$Count)] <- 0
  
  cm_data <- cm_data %>%
    mutate(
      Label = case_when(
        Truth == "Outlier" & Prediction == "Outlier" ~ "TP",
        Truth == "Normal"  & Prediction == "Outlier" ~ "FP",
        Truth == "Normal"  & Prediction == "Normal"  ~ "TN",
        Truth == "Outlier" & Prediction == "Normal"  ~ "FN"
      ),
      DispText = paste0(Label, "\n", Count),
      TextCol = ifelse(Count < max(Count) * 0.4, "black", "white")
    )
  
  p <- ggplot(cm_data, aes(x = Truth, y = Prediction, fill = Count)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = DispText, color = TextCol), size = 2.8, fontface = "bold", family = "Arial") +
    scale_fill_gradient(low = "grey95", high = config$color) +
    scale_color_identity() + 
    labs(x = "Ground Truth", y = "Predicted Class") +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_discrete(expand = c(0, 0)) +
    coord_fixed(ratio = 1, clip = "off") + 
    theme_report()
  
  return(p)
}

# ==============================================================================
# 4. Loop & Export
# ==============================================================================

message("\nGenerating confusion matrices...")

for (cfg in plot_configs) {
  p <- generate_conf_matrix(
    data = df_calc,
    truth_col = "is_outlier",
    pred_col = cfg$col,
    config = cfg
  )
  save_path <- file.path(output_dir, cfg$file)
  ggsave(save_path, plot = p, device = cairo_pdf, width = 2, height = 2)
}

message("All confusion matrices exported successfully.")