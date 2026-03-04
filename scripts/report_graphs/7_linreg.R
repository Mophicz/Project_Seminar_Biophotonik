# ==============================================================================
# Polished Linear Regression Diagnostics
# Description: Generates regression, residual, leverage, and distance plots.
#              UPDATED: Ported from base R to ggplot2 for style consistency.
#              UPDATED: Sized for composite assembly (3.4 in / 1.7 in widths).
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
# 2. Statistical Calculations & Modeling
# ==============================================================================

message("Computing linear regression and diagnostics...")

# Fit linear model
lin_reg <- lm(signal_out ~ conc, data = df)

# Extract model parameters for thresholds
n_obs <- nobs(lin_reg)
p_vars <- length(coefficients(lin_reg))

# Threshold definitions based on original script
mean_lev      <- p_vars / n_obs
thresh_lev    <- 2 * mean_lev
thresh_cooks  <- 4 / n_obs

# Augment dataframe with all necessary diagnostic statistics
df_plot <- df %>%
  mutate(
    fitted_vals = fitted(lin_reg),
    resids      = residuals(lin_reg),
    std_resids  = rstandard(lin_reg),
    stu_resids  = rstudent(lin_reg),
    leverage    = hatvalues(lin_reg),
    cooksd      = cooks.distance(lin_reg)
  )

# ==============================================================================
# 3. Visualization Setup (Matched to 6_MCC & 2_zscore)
# ==============================================================================

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
      plot.margin = margin(t = 15, r = 15, b = 15, l = 15),
      legend.position = "none"
    )
}

# Standard graphical parameters
pt_color   <- "grey40"
pt_size    <- 1.5
pt_alpha   <- 0.7
line_color <- "firebrick"
line_width <- 1

# Dimensions
full_width <- 3.4
half_width <- 2.3
plot_height <- 2.8
half_height <- 1.9

# ==============================================================================
# 4. Generate Plots
# ==============================================================================

message("Generating and exporting plots...")

# --- 1. Linear Regression ---
p_reg <- ggplot(df_plot, aes(x = conc, y = signal_out)) +
  geom_point(color = pt_color, size = pt_size, alpha = pt_alpha) +
  geom_abline(intercept = coef(lin_reg)[1], 
              slope = coef(lin_reg)[2], 
              color = "black", 
              linewidth = 0.5) +
  labs(x = "Concentration", y = "Signal Intensity") +
  coord_cartesian(clip = "off") +
  theme_report()

ggsave(file.path(output_dir, "07_linreg_fit.pdf"), plot = p_reg, 
       device = cairo_pdf, width = full_width, height = plot_height)

# --- 2. Residuals ---
p_resid <- ggplot(df_plot, aes(x = conc, y = resids)) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_point(color = pt_color, size = pt_size, alpha = pt_alpha) +
  labs(x = "Concentration", y = "Residuals") +
  coord_cartesian(clip = "off") +
  theme_report()

ggsave(file.path(output_dir, "07_linreg_residuals.pdf"), plot = p_resid, 
       device = cairo_pdf, width = half_width, height = half_height)

# --- 3. Standardized Residuals (Half Width) ---
p_std <- ggplot(df_plot, aes(x = conc, y = std_resids)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -3, ymax = 3, fill = line_color, alpha = 0.1) +
  geom_hline(yintercept = c(-3, 3), color = line_color, 
             linetype = "solid", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_point(color = pt_color, size = pt_size, alpha = pt_alpha) +
  annotate("text", x = Inf, y = 3, label = "+3", hjust = 1.2, vjust = -0.5, 
           color = line_color, family = "Arial", size = 3, fontface = "bold") +
  annotate("text", x = Inf, y = -3, label = "-3", hjust = 1.2, vjust = 1.5, 
           color = line_color, family = "Arial", size = 3, fontface = "bold") +
  expand_limits(y = c(-4.5, 4)) +
  labs(x = "Concentration", y = "Standardized Res.") +
  coord_cartesian(clip = "off") +
  theme_report()

ggsave(file.path(output_dir, "07_linreg_std_resids.pdf"), plot = p_std, 
       device = cairo_pdf, width = half_width, height = half_height)

# --- 4. Studentized Residuals (Half Width) ---
p_stu <- ggplot(df_plot, aes(x = conc, y = stu_resids)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = -3, ymax = 3, fill = line_color, alpha = 0.1) +
  geom_hline(yintercept = c(-3, 3), color = line_color, 
             linetype = "solid", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_point(color = pt_color, size = pt_size, alpha = pt_alpha) +
  annotate("text", x = Inf, y = 3, label = "+3", hjust = 1.2, vjust = -0.5, 
           color = line_color, family = "Arial", size = 3, fontface = "bold") +
  annotate("text", x = Inf, y = -3, label = "-3", hjust = 1.2, vjust = 1.5, 
           color = line_color, family = "Arial", size = 3, fontface = "bold") +
  expand_limits(y = c(-4.5, 4)) +
  labs(x = "Concentration", y = "Studentized Res.") +
  coord_cartesian(clip = "off") +
  theme_report()

ggsave(file.path(output_dir, "07_linreg_stu_resids.pdf"), plot = p_stu, 
       device = cairo_pdf, width = half_width, height = half_height)

# --- 5. Leverage ---
p_lev <- ggplot(df_plot, aes(x = conc, y = leverage)) +
  geom_hline(yintercept = mean_lev, color = "black", 
             linetype = "solid", linewidth = 0.5) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = thresh_lev, fill = line_color, alpha = 0.1) +
  geom_hline(yintercept = thresh_lev, color = line_color, 
             linetype = "solid", linewidth = 0.5) +
  annotate("text", x = Inf, y = mean_lev, label = "bar(h)", parse = TRUE, 
           hjust = 1.5, vjust = -0.5, color = "black", family = "Arial", size = 3) +
  annotate("text", x = Inf, y = thresh_lev, label = "2*bar(h)", parse = TRUE, 
           hjust = 1.3, vjust = -0.5, color = line_color, family = "Arial", size = 3) +
  expand_limits(y = c(0, 0.1), x = c(0, 1050)) +
  geom_point(color = pt_color, size = pt_size, alpha = pt_alpha) +
  labs(x = "Concentration", y = "Leverage") +
  coord_cartesian(clip = "off") +
  theme_report()

ggsave(file.path(output_dir, "07_linreg_leverage.pdf"), plot = p_lev, 
       device = cairo_pdf, width = full_width, height = plot_height)

# --- 6. Cook's Distance ---
# Using geom_segment to replicate the type="h" base R plot
p_cooks <- ggplot(df_plot, aes(x = seq_along(cooksd), y = cooksd)) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 0, ymax = thresh_cooks, fill = line_color, alpha = 0.1) +
  geom_hline(yintercept = thresh_cooks, color = line_color, 
             linetype = "solid", linewidth = 0.5) +
  geom_segment(aes(xend = seq_along(cooksd), yend = 0), linewidth = 1, color = pt_color) +
  #geom_point(color = pt_color, size = pt_size, alpha = pt_alpha) +
  annotate("text", x = Inf, y = thresh_cooks, label = "4/n", parse = TRUE, 
           hjust = 1.2, vjust = -0.5, color = line_color, family = "Arial", size = 3) +
  labs(x = "Observation Index", y = "Cook's Distance") +
  coord_cartesian(clip = "off") +
  theme_report()

ggsave(file.path(output_dir, "07_linreg_cooks.pdf"), plot = p_cooks, 
       device = cairo_pdf, width = full_width, height = plot_height)

# --- 7. Williams Plot ---
p_will <- ggplot(df_plot, aes(x = leverage, y = stu_resids)) +
  annotate("rect", xmin = -Inf, xmax = thresh_cooks, ymin = -3, ymax = 3, fill = line_color, alpha = 0.1) +
  geom_hline(yintercept = c(-3, 3), color = line_color, 
             linetype = "solid", linewidth = 0.5) +
  geom_vline(xintercept = thresh_cooks, color = line_color, 
             linetype = "solid", linewidth = 0.5) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_point(color = pt_color, size = pt_size, alpha = pt_alpha) +
  annotate("text", x = Inf, y = 3, label = "+3", hjust = 4, vjust = -0.5, 
           color = line_color, family = "Arial", size = 3, fontface = "bold") +
  annotate("text", x = Inf, y = -3, label = "-3", hjust = 5, vjust = 1.5, 
           color = line_color, family = "Arial", size = 3, fontface = "bold") +
  annotate("text", x = thresh_cooks, y = Inf, label = "2*bar(h)", parse = TRUE, 
           hjust = 1.5, vjust = 1.5, color = line_color, family = "Arial", size = 3) +
  expand_limits(y = c(-4, 4)) +
  labs(x = "Leverage", y = "Studentized Residuals") +
  coord_cartesian(clip = "off") +
  theme_report()

ggsave(file.path(output_dir, "07_linreg_williams.pdf"), plot = p_will, 
       device = cairo_pdf, width = full_width, height = plot_height)

message("All linear regression plots exported successfully.")