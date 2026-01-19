# ==============================================================================
# Polished Analysis: Plot 2.3 (Converted + Sorted Legend)
# Description: Reproduces Plot 2.3 with specific color map and 
#              custom sort order for the legend.
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
  theme_minimal(base_size = 14) +
    theme(
      axis.title = element_text(face = "bold", size = 12),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      legend.position = "right", 
      legend.title = element_text(face = "bold", size = 10),
      legend.text = element_text(size = 10)
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
  labs(
    x = "Jittered Concentration", 
    y = "Signal Intensity",
    color = "Data Type"
  ) +
  theme_report()

# ==============================================================================
# 4. Export
# ==============================================================================

save_path <- file.path(output_dir, "plot_2_3_sorted_legend.pdf")
ggsave(save_path, plot = p, device = "pdf", width = 7, height = 4)

message(paste("Saved plot to:", save_path))