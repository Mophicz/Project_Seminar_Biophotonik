# ==============================================================================
# Polished Z-Score Report: Vertical Strip Plot (Single Group)
# Description: Plots Z-scores for the first concentration group only.
#              UPDATED: Includes all 5 methods with 10% trim/winsorize.
#              UPDATED: Scaled for composite assembly (1.8 x 2.0 inches).
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

if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
df <- read_table(data_path)

# ==============================================================================
# 2. Statistical Calculations & Filtering
# ==============================================================================

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
  ) %>% ungroup()

target_conc <- unique(df_calc$conc)[1]
df_plot_data <- df_calc %>% filter(conc == target_conc)

all_configs <- list(
  list(column = "z_mean",   color = "firebrick",   file = "02_zscore_mean_sd.pdf"),
  list(column = "z_median", color = "dodgerblue4", file = "02_zscore_median_mad.pdf"),
  list(column = "z_iqr",    color = "black",       file = "02_zscore_median_iqr.pdf"),
  list(column = "z_trim",   color = "darkorchid4", file = "02_zscore_trimmed.pdf"),
  list(column = "z_wins",   color = "orange",      file = "02_zscore_winsorized.pdf")
)

# ==============================================================================
# 3. Visualization Function
# ==============================================================================

theme_strip <- function() {
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

generate_vertical_plot <- function(data, config) {
  y_values <- data[[config$column]]
  upper_lim  <- 3
  lower_lim  <- -3
  main_color <- config$color
  
  plot_df <- data.frame(
    X_Dummy = 0, Score = y_values, IsOutlier = factor(data$is_outlier, levels = c(FALSE, TRUE))
  )
  
  p <- ggplot(plot_df, aes(x = X_Dummy, y = Score)) +
    annotate("rect", xmin = -Inf, xmax = Inf, ymin = lower_lim, ymax = upper_lim, fill = main_color, alpha = 0.1) +
    geom_hline(yintercept = 0, color = main_color, linewidth = 1) +
    geom_point(aes(color = IsOutlier), size = 1, alpha = 0.5, position = position_jitter(width = 0.1, height = 0, seed = 42)) +
    
    annotate("text", x = 0.35, y = upper_lim, label = "+3", vjust = -0.5, 
             color = main_color, family = "Arial", size = 2.8, fontface = "bold") +
    annotate("text", x = 0.35, y = lower_lim, label = "-3", vjust = 1.5, 
             color = main_color, family = "Arial", size = 2.8, fontface = "bold") +
    
    scale_color_manual(values = c("FALSE" = "grey40", "TRUE" = "red")) +
    scale_y_continuous(expand = expansion(mult = 0.1)) +
    scale_x_continuous(limits = c(-0.5, 0.5)) +
    labs(y = "Z-Score") + coord_cartesian(clip = "off") + theme_strip()
  
  return(p)
}

# ==============================================================================
# 4. Execution
# ==============================================================================

message("Generating individual Z-score plots...")

for (curr_config in all_configs) {
  p <- generate_vertical_plot(df_plot_data, curr_config)
  save_path <- file.path(output_dir, curr_config$file)
  ggsave(save_path, plot = p, device = cairo_pdf, width = 1.8, height = 2)
}

message("All Z-score plots generated successfully.")