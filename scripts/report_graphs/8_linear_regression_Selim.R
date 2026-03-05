# ==============================================================================
# Polished Multi-Model Comparison Plot (Linear vs Theil-Sen vs Original Data)
# Description: Generates a combined regression comparison plot.
#              UPDATED: Ported to match the config-list architecture of 6_MCC.R
# ==============================================================================

rm(list = ls())

library(ggplot2)
library(dplyr)
library(here)
library(readr)
library(mblm)

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
# 2. Model Fitting
# ==============================================================================

message("Fitting regression models...")

# Model 1: Linear Regression (signal_out)
lin_reg <- lm(signal_out ~ conc, data = df)

# Model 2: Linear Regression (signal_orig)
lin_reg_2 <- lm(signal_orig ~ conc, data = df)

# Model 3: Theil-Sen Estimator (signal_out)
theil_sen <- mblm(signal_out ~ conc, data = df)

# ==============================================================================
# 3. Visualization Setup & Configuration Logic
# ==============================================================================

# Define aesthetics (now including 'lwd' for linewidth) in the centralized list
plot_configs <- list(
  list(label = "OLS",         color = "firebrick",   linetype = "dashed", lwd = 1.2, int = coef(lin_reg)[1],   slp = coef(lin_reg)[2]),
  list(label = "Theil-Sen",   color = "dodgerblue4", linetype = "dashed", lwd = 1.2, int = coef(theil_sen)[1], slp = coef(theil_sen)[2]),
  list(label = "signal_orig", color = "black",      linetype = "solid",  lwd = 0.5, int = coef(lin_reg_2)[1], slp = coef(lin_reg_2)[2])
)

# Extract mappings for ggplot scales
method_colors <- setNames(sapply(plot_configs, `[[`, "color"), 
                          sapply(plot_configs, `[[`, "label"))

method_linetypes <- setNames(sapply(plot_configs, `[[`, "linetype"), 
                             sapply(plot_configs, `[[`, "label"))

method_linewidths <- setNames(sapply(plot_configs, `[[`, "lwd"), 
                              sapply(plot_configs, `[[`, "label"))

# EXACT NEW NAMES: Define the order you want the legend to display
legend_order <- c("signal_orig", "OLS", "Theil-Sen")

# Exact theme matched to 6_MCC.R
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
      
      # COMPACT LEGEND STYLING
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.text = element_text(size = 8),
      legend.key.height = unit(0.3, "cm"),  # Tighter vertical space
      legend.key.width = unit(0.4, "cm"),   # Shorter line segments
      legend.spacing.x = unit(0.1, "cm"),   # Squeezes items closer together horizontally
      legend.box.spacing = unit(0.1, "cm"), # Pulls legend closer to the x-axis
      legend.margin = margin(t = 0, r = 0, b = 0, l = 0)
    )
}

# ==============================================================================
# 4. Generate Plot & Export
# ==============================================================================

message("Generating multi-model comparison plot...")

# Convert configuration list into a tidy dataframe for ggplot
line_data <- bind_rows(lapply(plot_configs, function(cfg) {
  data.frame(Method = cfg$label, intercept = cfg$int, slope = cfg$slp)
}))

p_comp <- ggplot(df, aes(x = conc, y = signal_out)) +
  geom_point(color = "grey40", size = 1.5, alpha = 0.7) +
  
  # Map color, linetype, AND linewidth to 'Method'
  geom_abline(data = line_data, 
              aes(intercept = intercept, slope = slope, 
                  color = Method, linetype = Method, linewidth = Method), 
              key_glyph = "path") + 
  
  # Apply extracted colors, linetypes, linewidths, and enforce the legend order
  scale_color_manual(values = method_colors, breaks = legend_order) +
  scale_linetype_manual(values = method_linetypes, breaks = legend_order) +
  scale_linewidth_manual(values = method_linewidths, breaks = legend_order) +
  
  labs(x = "Concentration", y = "Signal Intensity") +
  coord_cartesian(ylim = c(0, 4000), clip = "off") +
  theme_report()

# Export fitting a 3.4 inch column
save_path <- file.path(output_dir, "08_linreg_comparison.pdf")
ggsave(save_path, plot = p_comp, device = cairo_pdf, width = 3.4, height = 2.8)

message(paste("Comparison plot exported successfully to:", save_path))