# ==============================================================================
# Polished Analysis: Plot 2.3 (Converted + Sorted Legend)
# Description: Reproduces Plot 2.3 with specific color map, 
#              custom sort order for the legend, and updated margins.
#              UPDATED: Exact graph style coherence with 1_mean.R
#              UPDATED: X-axis ticks mapped exactly to specific conc groups
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

# --- CRITICAL STEP: Set Factor Levels for Legend Order ---
# This forces ggplot to sort the legend exactly as specified, 
# rather than alphabetically.
df$type <- factor(df$type, levels = c("normal", "3xSD", "10xSD", "random"))

# ==============================================================================
# 2. Visualization Function
# ==============================================================================

theme_report <- function() {
  theme_minimal(base_family = "Arial", base_size = 16) +
    theme(
      axis.title = element_text(size = 24),
      axis.text = element_text(size = 14), 
      
      axis.ticks = element_line(color = "black", linewidth = 0.5),
      axis.ticks.length = unit(0.15, "cm"),
      
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      
      panel.grid.major.y = element_line(color = "grey85", linewidth = 0.5),
      panel.grid.major.x = element_line(color = "grey85", linewidth = 0.5), 
      panel.grid.minor = element_blank(),
      
      legend.position = "right", 
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14)
    )
}

# ==============================================================================
# 3. Generate Plot 2.3
# ==============================================================================

message("Generating Plot 2.3 with sorted legend...")

# Colors mapped to the specific levels
cols_map <- c(
  "normal" = "grey",
  "3xSD"   = "yellow",
  "10xSD"  = "orange",
  "random" = "red"
)

p <- ggplot(df, aes(x = conc_jitter, y = signal_out, color = type)) +
  geom_point(size = 2, alpha = 0.5) +
  scale_color_manual(values = cols_map) +
  
  # CHANGED: Explicitly set the x-axis breaks to the requested groups
  scale_x_continuous(breaks = c(10, 250, 500, 750, 1000)) + 
  scale_y_continuous(expand = expansion(mult = 0.15)) +
  
  labs(
    x = "Concentration", 
    y = "Signal Intensity",
    color = "Data Type"
  ) +
  coord_cartesian(clip = "off") +
  theme_report()

# ==============================================================================
# 4. Export
# ==============================================================================

save_path <- file.path(output_dir, "00_dataset.pdf")

ggsave(save_path, plot = p, device = cairo_pdf, width = 7, height = 4)

message(paste("Saved plot to:", save_path))