# ==============================================================================
# Polished MCC Analysis: Method Comparison (Mean/SD vs Median/MAD)
# Description: Calculates Z-scores and generates a single combined MCC curve plot.
#              UPDATED: Ported from base R to ggplot2 for style consistency.
#              UPDATED: Evaluates absolute Z-scores (|Z|) across thresholds.
#              UPDATED: Identifies and plots the optimal threshold lines.
#              UPDATED: Filtered to only show Mean/SD and Median/MAD.
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

message("Calculating Z-scores...")

df_calc <- df %>%
  group_by(conc) %>%
  mutate(
    z_mean   = (signal_out - mean(signal_out)) / sd(signal_out),
    z_median = (signal_out - median(signal_out)) / mad(signal_out)
  ) %>%
  ungroup()

# ==============================================================================
# 3. Visualization Setup & MCC Logic
# ==============================================================================

# Filtered strictly to Mean/SD and Median/MAD
plot_configs <- list(
  list(col = "z_mean",   label = "Mean/SD",    color = "firebrick"),
  list(col = "z_median", label = "Median/MAD", color = "dodgerblue4")
)

# Extract colors for ggplot scale
method_colors <- setNames(sapply(plot_configs, `[[`, "color"), 
                          sapply(plot_configs, `[[`, "label"))

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
# 4. Generate Combined Plot & Export
# ==============================================================================

message("Calculating MCC curves...")

# Calculate data for all configurations and bind them together
all_mcc_data <- lapply(plot_configs, function(cfg) {
  calculate_mcc_data(truth = df_calc$is_outlier, 
                     score_raw = df_calc[[cfg$col]], 
                     label = cfg$label)
}) %>% bind_rows()

# Identify optimal threshold for each method (to draw vertical lines)
opt_thresholds <- all_mcc_data %>%
  group_by(Method) %>%
  slice_max(mcc, n = 1, with_ties = FALSE) %>%
  ungroup()

message("Generating combined MCC plot...")

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

# Export fitting a 3.4 inch column. Height is set to 3.2 to accommodate the bottom legend.
save_path <- file.path(output_dir, "06_mcc_mean_median.pdf")
ggsave(save_path, plot = p, device = cairo_pdf, width = 3.4, height = 2.8)

message(paste("MCC curve exported successfully to:", save_path))