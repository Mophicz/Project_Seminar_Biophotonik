# ==============================================================================
# Polished Confusion Matrix Analysis: Z-Score Threshold Evaluation
# Description: Evaluates classification performance (Threshold = +/- 3) 
#              by plotting confusion matrices for Classic and Robust Z-Scores.
#              UPDATED: Scaled for composite assembly (width = 2, height = 3)
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

df_calc <- df %>%
  group_by(conc) %>%
  mutate(
    # Z-Score Calculations
    z_classic = (signal_out - mean(signal_out, na.rm = TRUE)) / sd(signal_out, na.rm = TRUE),
    z_robust  = (signal_out - median(signal_out, na.rm = TRUE)) / mad(signal_out, na.rm = TRUE),
    
    # Threshold Predictions (TRUE = Predicted Outlier, FALSE = Predicted Normal)
    pred_classic = abs(z_classic) > threshold,
    pred_robust  = abs(z_robust) > threshold
  ) %>%
  ungroup()

# ==============================================================================
# 3. Visualization Setup
# ==============================================================================

plot_configs <- list(
  list(col = "pred_classic", label = "Classic Z-Score", color = "firebrick",   file = "03_conf_classic.pdf"),
  list(col = "pred_robust",  label = "Robust Z-Score",  color = "dodgerblue4", file = "03_conf_robust.pdf")
)

theme_report <- function() {
  # CHANGED: Adjusted base_size, axis.title, and axis.text to match 2_zscore.R sizing
  theme_minimal(base_family = "Arial", base_size = 10) +
    theme(
      axis.title = element_text(size = 10),
      axis.text = element_text(size = 8), 
      
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      
      panel.border = element_blank(),
      
      panel.grid = element_blank(), # Removed grid for clean matrix tiles
      legend.position = "none"
    )
}

generate_conf_matrix <- function(data, truth_col, pred_col, config) {
  
  # Extract vectors
  truth_vec <- as.logical(data[[truth_col]])
  pred_vec  <- as.logical(data[[pred_col]])
  
  # Explicitly define factor levels to ensure a fixed 2x2 grid is always drawn
  lvls <- c(FALSE, TRUE)
  lbls <- c("Normal", "Outlier")
  
  truth_fct <- factor(truth_vec, levels = lvls, labels = lbls)
  pred_fct  <- factor(pred_vec, levels = lvls, labels = lbls)
  
  # Compute matrix counts
  cm_data <- data.frame(Truth = truth_fct, Prediction = pred_fct) %>%
    group_by(Truth, Prediction) %>%
    summarise(Count = n(), .groups = "drop")
  
  # Ensure all 4 combinations (TP, FP, TN, FN) exist even if count is 0
  grid <- expand.grid(Truth = lbls, Prediction = lbls)
  cm_data <- left_join(grid, cm_data, by = c("Truth", "Prediction"))
  cm_data$Count[is.na(cm_data$Count)] <- 0
  
  # Assign category labels and aesthetic text formatting
  cm_data <- cm_data %>%
    mutate(
      Label = case_when(
        Truth == "Outlier" & Prediction == "Outlier" ~ "TP",
        Truth == "Normal"  & Prediction == "Outlier" ~ "FP",
        Truth == "Normal"  & Prediction == "Normal"  ~ "TN",
        Truth == "Outlier" & Prediction == "Normal"  ~ "FN"
      ),
      DispText = paste0(Label, "\n", Count),
      # Dynamic text color: high counts get white text, low counts get black text
      TextCol = ifelse(Count < max(Count) * 0.4, "black", "white")
    )
  
  # Generate Tile Plot
  p <- ggplot(cm_data, aes(x = Truth, y = Prediction, fill = Count)) +
    geom_tile(color = "white", linewidth = 1) +
    # CHANGED: Adjusted geom_text size to 2.8 to match 2_zscore.R annotations
    geom_text(aes(label = DispText, color = TextCol), size = 2.8, fontface = "bold", family = "Arial") +
    scale_fill_gradient(low = "grey95", high = config$color) +
    scale_color_identity() + # Use literal color values defined in TextCol
    labs(
      x = "Ground Truth", 
      y = "Predicted Class"
    ) +
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
  
  message(paste("Processing:", cfg$label))
  
  p <- generate_conf_matrix(
    data = df_calc,
    truth_col = "is_outlier",
    pred_col = cfg$col,
    config = cfg
  )
  
  save_path <- file.path(output_dir, cfg$file)
  # CHANGED: Adjusted ggsave width and height to 2 and 3 respectively
  ggsave(save_path, plot = p, device = cairo_pdf, width = 2, height = 2)
  
  message(paste("   -> Saved to:", save_path))
}

message("All confusion matrices exported successfully.")