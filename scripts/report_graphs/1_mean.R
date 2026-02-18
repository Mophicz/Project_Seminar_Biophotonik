# ==============================================================================
# Polished Signal Analysis Report: Statistical Comparison (Fixed Loop)
# Description: Generates three plots (Mean, Median, Trimmed).
#              Includes fixes for loop stability and PDF generation.
#              UPDATED: Now colors points based on 'is_outlier' column.
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

# Load data
df <- read_table(data_path)

# Filter for the first concentration level
conc_levels <- unique(df$conc)
current_conc <- conc_levels[1]

# Extract subset keeping signal AND outlier status
df_subset <- df %>% 
  filter(conc == current_conc)

# Create plotting data frame
plot_data <- data.frame(
  Index = seq_along(df_subset$signal_out),
  Signal = df_subset$signal_out,
  IsOutlier = factor(df_subset$is_outlier, levels = c(FALSE, TRUE)) # Ensure factors for coloring
)

# ==============================================================================
# 2. Statistical Calculations
# ==============================================================================

signal <- plot_data$Signal

# Ensure na.rm=TRUE to prevent calculation errors from stopping the script
stat_mean <- list(
  center = mean(signal, na.rm = TRUE),
  spread = sd(signal, na.rm = TRUE),
  color  = "firebrick",
  label_sym = "SD",
  file   = "01_mean_sd.pdf"
)

stat_median <- list(
  center = median(signal, na.rm = TRUE),
  spread = mad(signal, na.rm = TRUE),
  color  = "dodgerblue4",
  label_sym = "MAD",
  file   = "02_median_mad.pdf"
)

# Trimmed calculations
n <- length(signal)
n_trimm <- floor(n * 0.2)
sorted_signal <- sort(signal)
keeper_values <- sorted_signal[(n_trimm + 1) : (n - n_trimm)]

stat_trimmed <- list(
  center = mean(keeper_values, na.rm = TRUE),
  spread = sd(keeper_values, na.rm = TRUE),
  color  = "#2ecc71",
  label_sym = "SD (Trim)",
  file   = "03_trimmed_mean.pdf"
)

# Store in a list
all_stats <- list(stat_mean, stat_median, stat_trimmed)

# ==============================================================================
# 3. Visualization Function
# ==============================================================================

theme_report <- function() {
  theme_minimal(base_size = 14) +
    theme(
      axis.title = element_text(face = "bold", size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "none" # Legend hidden as per original style
    )
}

generate_plot <- function(data, stats_obj) {
  
  # Extract values
  center_val <- stats_obj$center
  spread_val <- stats_obj$spread
  y_min <- center_val - spread_val
  y_max <- center_val + spread_val
  main_color <- stats_obj$color
  
  lbl_upper <- paste("+1", stats_obj$label_sym)
  lbl_lower <- paste("-1", stats_obj$label_sym)
  
  p <- ggplot(data, aes(x = Index, y = Signal)) +
    
    # Shaded Region
    annotate("rect", xmin = -Inf, xmax = Inf, 
             ymin = y_min, ymax = y_max, 
             fill = main_color, alpha = 0.1) +
    
    # Center Line
    geom_hline(yintercept = center_val, 
               color = main_color, linewidth = 1) +
    
    # Points - Colored by IsOutlier
    geom_point(aes(color = IsOutlier), alpha = 0.5, size = 2) +
    
    # Text Labels (using coord_cartesian(clip="off") to handle edges)
    annotate("text", x = nrow(data) * 0.95, y = y_max, 
             label = lbl_upper, vjust = -0.6, 
             color = main_color, size = 3.5, fontface = "bold") +
    
    annotate("text", x = nrow(data) * 0.95, y = y_min, 
             label = lbl_lower, vjust = 1.6, 
             color = main_color, size = 3.5, fontface = "bold") +
    
    # Manual Color Scale (Matches the Z-score script)
    scale_color_manual(values = c("FALSE" = "grey40", "TRUE" = "red")) +
    
    labs(x = "Measurement Index", y = "Signal Intensity") +
    
    # Add padding and turn off clipping
    scale_y_continuous(expand = expansion(mult = 0.15)) +
    coord_cartesian(clip = "off") +
    theme_report()
  
  return(p)
}

# ==============================================================================
# 4. Robust Loop Execution
# ==============================================================================

# We loop by index to ensure each object is distinct and fully processed
message("Starting plot generation...")

for (i in 1:length(all_stats)) {
  
  # Get current metric object
  curr_metric <- all_stats[[i]]
  
  message(paste0("[", i, "/", length(all_stats), "] Processing: ", curr_metric$file))
  
  # Generate
  p <- generate_plot(plot_data, curr_metric)
  
  # Save
  save_path <- file.path(output_dir, curr_metric$file)
  ggsave(save_path, plot = p, device = "pdf", width = 6, height = 4)
  
  message(paste("   -> Saved to:", save_path))
}

message("All plots generated successfully.")
