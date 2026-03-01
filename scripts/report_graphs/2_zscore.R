# ==============================================================================
# Polished Z-Score Report: Vertical Strip Plot (Single Group)
# Description: Plots Z-scores for the first concentration group only.
#              - Matches logic from meeting_13-11.R (conc[1])
#              - Colors (Red/Grey) are determined STRICTLY by the 'is_outlier'
#                variable in the input dataframe, not the Z-score value.
#              - Calculates Classic, Robust, and Trimmed Z-scores.
#              UPDATED: Applied exact 1_mean.R styling.
#              UPDATED: Changed green color to colorblind-safe purple.
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

# ==============================================================================
# 2. Statistical Calculations & Filtering
# ==============================================================================

# Helper functions for Trimmed statistics (matching 1_mean.R logic)
get_trimmed_mean <- function(x, trim_prop = 0.2) {
  x <- x[!is.na(x)]
  n <- length(x)
  n_trimm <- floor(n * trim_prop)
  if (n_trimm > 0) {
    keeper_values <- sort(x)[(n_trimm + 1) : (n - n_trimm)]
  } else {
    keeper_values <- x
  }
  return(mean(keeper_values))
}

get_trimmed_sd <- function(x, trim_prop = 0.2) {
  x <- x[!is.na(x)]
  n <- length(x)
  n_trimm <- floor(n * trim_prop)
  if (n_trimm > 0) {
    keeper_values <- sort(x)[(n_trimm + 1) : (n - n_trimm)]
  } else {
    keeper_values <- x
  }
  return(sd(keeper_values))
}

# Calculate stats grouped by concentration (preserves is_outlier column)
df_calc <- df %>%
  group_by(conc) %>%
  mutate(
    # Classic Z-Score
    mean_val  = mean(signal_out, na.rm = TRUE),
    sd_val    = sd(signal_out, na.rm = TRUE),
    z_classic = (signal_out - mean_val) / sd_val,
    
    # Robust Z-Score
    median_val = median(signal_out, na.rm = TRUE),
    mad_val    = mad(signal_out, na.rm = TRUE),
    z_robust   = (signal_out - median_val) / mad_val,
    
    # Trimmed Z-Score
    trimmed_mean_val = get_trimmed_mean(signal_out, 0.2),
    trimmed_sd_val   = get_trimmed_sd(signal_out, 0.2),
    z_trimmed        = (signal_out - trimmed_mean_val) / trimmed_sd_val
  ) %>%
  ungroup()

# FILTER: Keep only the first concentration group
target_conc <- unique(df_calc$conc)[1]
# df_plot_data <- df_calc %>% filter(conc == target_conc)
df_plot_data <- df_calc

message(paste("Filtered data to concentration group:", target_conc))

# Configuration list
z_classic_config <- list(
  column    = "z_classic",
  color     = "firebrick",
  label_sym = "Limit",
  file      = "02_zscore_classic.pdf"
)

z_robust_config <- list(
  column    = "z_robust",
  color     = "dodgerblue4", 
  label_sym = "Limit",
  file      = "02_zscore_robust.pdf"
)

# CHANGED: Green color swapped to purple for colorblind safety
z_trimmed_config <- list(
  column    = "z_trimmed",
  color     = "darkorchid4", 
  label_sym = "Limit",
  file      = "02_zscore_trimmed.pdf"
)

all_configs <- list(z_classic_config, z_robust_config, z_trimmed_config)

# ==============================================================================
# 3. Visualization Function
# ==============================================================================

# CHANGED: Exact style coherence with 1_mean.R (1D plot logic)
theme_strip <- function() {
  theme_minimal(base_family = "Arial", base_size = 16) +
    theme(
      axis.title.y = element_text(size = 24),
      axis.text.y = element_text(size = 14), 
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
  
  # Prepare plotting data
  # Uses the EXISTING 'is_outlier' column from the dataframe for coloring
  plot_df <- data.frame(
    X_Dummy = 0,
    Score = y_values,
    IsOutlier = factor(data$is_outlier, levels = c(FALSE, TRUE))
  )
  
  lbl_upper <- paste("+3", config$label_sym)
  lbl_lower <- paste("-3", config$label_sym)
  
  p <- ggplot(plot_df, aes(x = X_Dummy, y = Score)) +
    
    # 1. Shaded Region (Safe Zone)
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = lower_lim, ymax = upper_lim, 
             fill = main_color, alpha = 0.1) +
    
    # 2. Center Line (0) - Solid
    geom_hline(yintercept = 0, color = main_color, linewidth = 1) +
    
    # 3. Points (No Jitter)
    #    - position = "identity" ensures they are on a straight vertical line
    geom_point(aes(color = IsOutlier), size = 2, alpha = 0.5, position = position_jitter(width = 0.1, height = 0)) +
    
    # 4. Text Labels (CHANGED: Set to Arial, size 5, bold)
    annotate("text", x = 0.35, y = upper_lim, 
             label = lbl_upper, vjust = -0.5, 
             color = main_color, family = "Arial", size = 5, fontface = "bold") +
    
    annotate("text", x = 0.35, y = lower_lim, 
             label = lbl_lower, vjust = 1.5, 
             color = main_color, family = "Arial", size = 5, fontface = "bold") +
    
    # 5. Coloring based on IsOutlier factor
    scale_color_manual(values = c("FALSE" = "grey40", "TRUE" = "red")) +
    
    scale_y_continuous(expand = expansion(mult = 0.1)) +
    scale_x_continuous(limits = c(-0.5, 0.5)) +
    
    labs(y = "Z-Score") +
    # CHANGED: Added clip = "off" to prevent annotation cutoffs
    coord_cartesian(clip = "off") +
    theme_strip()
  
  return(p)
}

# ==============================================================================
# 4. Execution
# ==============================================================================

message("Starting vertical Z-score plot generation...")

for (i in 1:length(all_configs)) {
  curr_config <- all_configs[[i]]
  message(paste0("Processing: ", curr_config$file))
  
  # Note: is_outlier is NOT recalculated here. 
  # It uses whatever was in the original loaded file.
  
  p <- generate_vertical_plot(df_plot_data, curr_config)
  
  save_path <- file.path(output_dir, curr_config$file)
  # CHANGED: Updated device to cairo_pdf for proper Arial rendering
  ggsave(save_path, plot = p, device = cairo_pdf, width = 4, height = 6)
  
  message(paste("   -> Saved to:", save_path))
}

message("Done.")