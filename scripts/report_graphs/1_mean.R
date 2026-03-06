# ==============================================================================
# Polished Signal Analysis Report: Statistical Comparison (Vertical Strip Plot)
# Description: Generates five plots (Mean, Median, IQR, Trimmed, Winsorized).
#              UPDATED: Standardized to 10% trim/winsorize.
#              UPDATED: Font changed to Arial, axis titles scaled for paper.
#              UPDATED: Scaled for composite assembly (width = 1.8, height = 2).
#              UPDATED: Y-axis limits are strictly consistent across plots.
# ==============================================================================

rm(list = ls())

library(ggplot2)
library(dplyr)
library(here)
library(readr)

# ==============================================================================
# 1. Setup & Data Loading
# ==============================================================================

data_path <- here("data", "df_sim_4.txt")
output_dir <- "figures"

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

df <- read_table(data_path)

conc_levels <- unique(df$conc)
current_conc <- conc_levels[1]

df_subset <- df %>% 
  filter(conc == current_conc)

plot_data <- data.frame(
  X_Dummy = 0, 
  Signal = df_subset$signal_out,
  IsOutlier = factor(df_subset$is_outlier, levels = c(FALSE, TRUE)) 
)

# ==============================================================================
# 2. Statistical Calculations & Global Axis Limits
# ==============================================================================

signal <- plot_data$Signal

# 1. Mean / SD
stat_mean <- list(
  center = mean(signal, na.rm = TRUE),
  spread = sd(signal, na.rm = TRUE),
  color  = "firebrick",
  label_sym = "SD",           
  file   = "01_mean_sd.pdf"
)

# 2. Median / MAD
stat_median <- list(
  center = median(signal, na.rm = TRUE),
  spread = mad(signal, na.rm = TRUE),
  color  = "dodgerblue4",
  label_sym = "MAD",             
  file   = "01_median_mad.pdf"
)

# 3. Median / IQR
stat_iqr <- list(
  center = median(signal, na.rm = TRUE),
  spread = IQR(signal, na.rm = TRUE) / 1.349,
  color  = "black",
  label_sym = "IQR",             
  file   = "01_median_iqr.pdf"
)

# Helper for Trimmed/Winsorized limits (10% trim)
trim_prop <- 0.1
q_limits <- quantile(signal, probs = c(trim_prop, 1 - trim_prop), na.rm = TRUE)

# 4. Trimmed Mean / SD (10%)
trim_dat <- signal[signal >= q_limits[1] & signal <= q_limits[2]]
stat_trimmed <- list(
  center = mean(signal, trim = trim_prop, na.rm = TRUE),
  spread = sd(trim_dat, na.rm = TRUE),
  color  = "darkorchid4",
  label_sym = "SD",     
  file   = "01_trimmed_mean.pdf"
)

# 5. Winsorized Mean / SD (10%)
wins_dat <- pmax(pmin(signal, q_limits[2]), q_limits[1])
stat_wins <- list(
  center = mean(wins_dat, na.rm = TRUE),
  spread = sd(wins_dat, na.rm = TRUE),
  color  = "orange",
  label_sym = "SD",     
  file   = "01_winsorized_mean.pdf"
)

all_stats <- list(stat_mean, stat_median, stat_iqr, stat_trimmed, stat_wins)

all_y_mins <- sapply(all_stats, function(s) s$center - s$spread)
all_y_maxs <- sapply(all_stats, function(s) s$center + s$spread)

global_y_min <- min(c(signal, all_y_mins), na.rm = TRUE)
global_y_max <- max(c(signal, all_y_maxs), na.rm = TRUE)
global_y_limits <- c(global_y_min, global_y_max)

# ==============================================================================
# 3. Visualization Function 
# ==============================================================================

theme_report <- function() {
  theme_minimal(base_family = "Arial", base_size = 10) +
    theme(
      axis.title.y = element_text(size = 10),
      axis.text.y = element_text(size = 8), 
      axis.ticks.y = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length.y = unit(0.15, "cm"),
      
      axis.title.x = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.5),
      panel.grid.major.x = element_blank(), 
      panel.grid.minor = element_blank(),
      
      legend.position = "none"
    )
}

generate_plot <- function(data, stats_obj, y_limits) {
  
  center_val <- stats_obj$center
  spread_val <- stats_obj$spread
  y_min <- center_val - spread_val
  y_max <- center_val + spread_val
  main_color <- stats_obj$color
  
  lbl_upper <- paste("'+1' ~", stats_obj$label_sym)
  lbl_lower <- paste("'-1' ~", stats_obj$label_sym)
  
  p <- ggplot(data, aes(x = X_Dummy, y = Signal)) +
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = y_min, ymax = y_max, 
             fill = main_color, alpha = 0.1) +
    geom_hline(yintercept = center_val, color = main_color, linewidth = 1) +
    geom_point(aes(color = IsOutlier), alpha = 0.5, size = 1, 
               position = position_jitter(width = 0.1, height = 0, seed = 42)) +
    
    annotate("text", x = 0.35, y = y_max, 
             label = lbl_upper, parse = TRUE, family = "Arial",
             vjust = -0.6, color = main_color, size = 2.8, fontface = "bold") +
    annotate("text", x = 0.35, y = y_min, 
             label = lbl_lower, parse = TRUE, family = "Arial",
             vjust = 1.6, color = main_color, size = 2.8, fontface = "bold") +
    
    scale_color_manual(values = c("FALSE" = "grey40", "TRUE" = "red")) +
    scale_x_continuous(limits = c(-0.5, 0.5)) +
    scale_y_continuous(limits = y_limits, expand = expansion(mult = 0.15)) +
    labs(y = "Signal Intensity") + 
    coord_cartesian(clip = "off") +
    theme_report()
  
  return(p)
}

# ==============================================================================
# 4. Loop Execution
# ==============================================================================

message("Starting signal plot generation...")

for (curr_metric in all_stats) {
  p <- generate_plot(plot_data, curr_metric, global_y_limits)
  save_path <- file.path(output_dir, curr_metric$file)
  ggsave(save_path, plot = p, device = cairo_pdf, width = 1.8, height = 2)
}
message("All signal plots generated successfully.")