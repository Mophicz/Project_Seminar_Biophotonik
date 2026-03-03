# ==============================================================================
# Polished Master Assembly: IEEE Double-Column Figure
# Description: Generates and perfectly aligns 6 plots using an explicit 
#              patchwork grid. Strips redundant axes for micro-plot clarity.
#              UPDATED: Times New Roman 8pt tags, positioned cleanly.
#              UPDATED: All plot titles fully removed.
# ==============================================================================

rm(list = ls())

library(ggplot2)
library(dplyr)
library(here)
library(readr)
library(pROC)
library(tidyr)
library(patchwork) 

# ==============================================================================
# 1. Setup & Data Loading
# ==============================================================================

data_path <- here("data", "df_sim_4.txt")
output_dir <- "figures"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
df <- read_table(data_path)

# ==============================================================================
# 2. Universal Statistical Calculations
# ==============================================================================

message("Calculating universal statistics...")
threshold <- 3

df_calc <- df %>%
  group_by(conc) %>%
  mutate(
    mean_val  = mean(signal_out, na.rm = TRUE),
    sd_val    = sd(signal_out, na.rm = TRUE),
    z_classic = (signal_out - mean_val) / sd_val,
    pred_classic = abs(z_classic) > threshold,
    
    median_val = median(signal_out, na.rm = TRUE),
    mad_val    = mad(signal_out, na.rm = TRUE),
    z_robust   = (signal_out - median_val) / mad_val,
    pred_robust = abs(z_robust) > threshold
  ) %>% ungroup()

# ==============================================================================
# 3. IEEE Base Theme Definitions
# ==============================================================================

ieee_title <- 10
ieee_text  <- 8

theme_ieee_base <- function() {
  theme_minimal(base_family = "Arial", base_size = ieee_title) +
    theme(
      axis.title = element_text(size = 9),
      axis.text = element_text(size = ieee_text), 
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(0.1, "cm"),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major = element_line(color = "grey85", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      legend.position = "none",
      # Tighten margins to allow more room for data
      plot.margin = margin(3, 3, 3, 3, "pt") 
    )
}

# ==============================================================================
# 4. Plot Generation Functions
# ==============================================================================

# --- A. Z-Score Strip Plot ---
generate_z_plot <- function(data, col_name, main_color, keep_y_title = TRUE) {
  plot_df <- data.frame(X_Dummy = 0, Score = data[[col_name]], IsOutlier = factor(data$is_outlier, levels = c(FALSE, TRUE)))
  
  p <- ggplot(plot_df, aes(x = X_Dummy, y = Score)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = -3, ymax = 3, fill = main_color, alpha = 0.1) +
    geom_hline(yintercept = 0, color = main_color, linewidth = 1) +
    geom_point(aes(color = IsOutlier), size = 0.8, alpha = 0.5, position = position_jitter(width = 0.1, height = 0)) +
    annotate("text", x = 0.35, y = 3, label = "+3", vjust = -0.5, color = main_color, family = "Arial", size = 2.5, fontface = "bold") +
    annotate("text", x = 0.35, y = -3, label = "-3", vjust = 1.5, color = main_color, family = "Arial", size = 2.5, fontface = "bold") +
    scale_color_manual(values = c("FALSE" = "grey40", "TRUE" = "red")) +
    scale_y_continuous(expand = expansion(mult = 0.1)) +
    scale_x_continuous(limits = c(-0.5, 0.5)) +
    labs(y = "Z-Score") + coord_cartesian(clip = "off") + theme_ieee_base() +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(), panel.grid.major.x = element_blank())
  
  if(!keep_y_title) p <- p + theme(axis.title.y = element_blank())
  return(p)
}

# --- B. Confusion Matrix Plot ---
generate_conf_matrix <- function(data, pred_col, main_color, keep_y_title = TRUE) {
  lvls <- c(FALSE, TRUE); lbls <- c("Norm", "Out")
  truth_fct <- factor(as.logical(data[["is_outlier"]]), levels = lvls, labels = lbls)
  pred_fct  <- factor(as.logical(data[[pred_col]]), levels = lvls, labels = lbls)
  
  cm_data <- data.frame(Truth = truth_fct, Prediction = pred_fct) %>%
    group_by(Truth, Prediction) %>% summarise(Count = n(), .groups = "drop") %>%
    right_join(expand.grid(Truth = lbls, Prediction = lbls), by = c("Truth", "Prediction")) %>%
    mutate(Count = replace_na(Count, 0)) %>%
    mutate(
      Label = case_when(Truth == "Out" & Prediction == "Out" ~ "TP", Truth == "Norm" & Prediction == "Out" ~ "FP",
                        Truth == "Norm" & Prediction == "Norm" ~ "TN", Truth == "Out" & Prediction == "Norm" ~ "FN"),
      DispText = paste0(Label, "\n", Count),
      TextCol = ifelse(Count < max(Count) * 0.4, "black", "white")
    )
  
  p <- ggplot(cm_data, aes(x = Truth, y = Prediction, fill = Count)) +
    geom_tile(color = "white", linewidth = 1) +
    geom_text(aes(label = DispText, color = TextCol), size = 2.5, fontface = "bold", family = "Arial") +
    scale_fill_gradient(low = "grey95", high = main_color) + scale_color_identity() + 
    labs(x = "Truth", y = "Predicted") +
    scale_x_discrete(expand = c(0, 0)) + scale_y_discrete(expand = c(0, 0)) +
    coord_fixed(ratio = 1, clip = "off") + theme_ieee_base() +
    theme(panel.border = element_blank(), panel.grid.major = element_blank())
  
  if(!keep_y_title) p <- p + theme(axis.title.y = element_blank())
  return(p)
}

# --- C. ROC Curve Plot ---
generate_roc <- function(data, predictor_col, main_color, keep_y_title = TRUE) {
  r <- roc(data$is_outlier, data[[predictor_col]], direction = "<", quiet = TRUE)
  plot_data <- data.frame(spec = r$specificities, sens = r$sensitivities)
  label_text <- paste0()
  
  p <- ggplot(plot_data, aes(x = spec, y = sens)) +
    geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1), linetype = "dashed", color = "grey60") +
    geom_path(color = main_color, linewidth = 1.0) +
    annotate("text", x = 0.25, y = 0.15, label = label_text, size = 2.5, fontface = "bold", color = main_color, family = "Arial") +
    scale_x_reverse(limits = c(1, 0), breaks = c(1, 0.5, 0), labels = c("1", ".5", "0"), expand = expansion(mult = 0.03), name = "Spec") +
    scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), labels = c("0", ".5", "1"), expand = expansion(mult = 0.03), name = "Sens") +
    coord_fixed(ratio = 1, clip = "off") + theme_ieee_base()
  
  if(!keep_y_title) p <- p + theme(axis.title.y = element_blank())
  return(p)
}

# ==============================================================================
# 5. Build Subplots 
# ==============================================================================

message("Generating individual plots...")

# Classic Methods (Left side of pairs)
p_z_class    <- generate_z_plot(df_calc, "z_classic", "firebrick")
p_conf_class <- generate_conf_matrix(df_calc, "pred_classic", "firebrick")
p_roc_class  <- generate_roc(df_calc, "z_classic", "firebrick")

# Robust Methods (Right side of pairs - removing Y axis titles to save space)
p_z_rob    <- generate_z_plot(df_calc, "z_robust", "dodgerblue4", keep_y_title = FALSE)
p_conf_rob <- generate_conf_matrix(df_calc, "pred_robust", "dodgerblue4", keep_y_title = FALSE)
p_roc_rob  <- generate_roc(df_calc, "z_robust", "dodgerblue4", keep_y_title = FALSE)

# ==============================================================================
# 6. Assembly via Patchwork & Export
# ==============================================================================

message("Assembling master layout...")

design <- "
  1234
  1234
  1256
  1256
"

master_figure <- p_z_class + p_z_rob + p_conf_class + p_conf_rob + p_roc_class + p_roc_rob +
  plot_layout(design = design) +
  plot_annotation(tag_levels = 'a', tag_prefix = '(', tag_suffix = ')') & 
  theme(
    plot.tag = element_text(size = 8, family = "Times New Roman"),
    plot.margin = margin(t = 10, r = 3, b = 3, l = 10, "pt")
  )

save_path <- file.path(output_dir, "05_supervised_multi.pdf")

ggsave(save_path, plot = master_figure, device = cairo_pdf, width = 7.16, height = 4.0)

message(paste("Success! Master figure saved to:", save_path))