# ==============================================================================
# Polished Iterative Outlier Cleaning: OLS vs Theil-Sen
# Description: Compares iterative removal using Standardized Residuals.
#              UPDATED: Plots are now printed individually in RStudio.
# ==============================================================================

rm(list = ls())

library(readr)
library(here)
library(ggplot2)
library(dplyr)
library(mblm)
library(tidyr)

# ==============================================================================
# 1. Setup & Configuration
# ==============================================================================

# Define settings
max_iters <- 12
threshold <- 2 # Standardized residual threshold
data_filename <- "df_sim_4.txt" 
base_name_clean <- sub("_iter_[0-9]+$", "", tools::file_path_sans_ext(data_filename))

data_path <- here("data", data_filename)
df_original <- read_table(data_path)

# Unified IEEE-style Theme
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
      plot.title = element_text(size = 10, face = "bold", hjust = 0.5),
      legend.position = "bottom",
      plot.margin = margin(t = 15, r = 15, b = 15, l = 15)
    )
}

# ==============================================================================
# 2. Core Iterative Cleaning Function
# ==============================================================================

run_cleaning_loop <- function(df, method, max_iters, threshold, base_name) {
  
  metrics <- data.frame(
    iteration = integer(),
    removed_true_outliers = integer(),
    removed_valid_data = integer(),
    remaining_outliers = integer()
  )
  
  # Method specific aesthetics
  line_color <- ifelse(method == "OLS", "firebrick", "dodgerblue4")
  
  cum_tp <- 0
  cum_fp <- 0
  total_real_outliers <- sum(df$is_outlier == TRUE)
  
  for (i in 1:max_iters) {
    message(sprintf("--- %s: Starting Iteration %d ---", method, i))
    
    # Fit Model & Calculate Standardized Residuals
    if (method == "OLS") {
      mod <- lm(signal_out ~ conc, data = df)
      std_resids <- rstandard(mod)
    } else if (method == "Theil-Sen") {
      mod <- mblm(signal_out ~ conc, data = df)
      # Robust standardization for non-parametric model
      res <- residuals(mod)
      std_resids <- res / mad(res) 
    }
    
    # --- PLOTTING (Outputs directly to RStudio as individual plots) ---
    
    # 1. Fit Plot
    p_fit <- ggplot(df, aes(x = conc, y = signal_out)) +
      geom_point(color = "grey40", size = 1.5, alpha = 0.7) +
      geom_abline(intercept = coef(mod)[1], slope = coef(mod)[2], 
                  color = line_color, linewidth = 1) +
      labs(title = sprintf("%s Outlier Removal (Iter %d) - Fit", method, i), 
           x = "Concentration", y = "Signal Intensity") +
      theme_report()
    
    # 2. Residuals Plot
    p_res <- ggplot(df, aes(x = conc, y = std_resids)) +
      annotate("rect", xmin = -Inf, xmax = Inf, ymin = -threshold, ymax = threshold, 
               fill = line_color, alpha = 0.1) +
      geom_hline(yintercept = c(-threshold, threshold), color = line_color, 
                 linetype = "solid", linewidth = 0.5) +
      geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
      geom_point(color = "grey40", size = 1.5, alpha = 0.7) +
      annotate("text", x = Inf, y = threshold, label = paste0("+", threshold), 
               hjust = 1.2, vjust = -0.5, color = line_color, family = "Arial", size = 3, fontface = "bold") +
      annotate("text", x = Inf, y = -threshold, label = paste0("-", threshold), 
               hjust = 1.2, vjust = 1.5, color = line_color, family = "Arial", size = 3, fontface = "bold") +
      expand_limits(y = c(-threshold - 1.5, threshold + 1.5)) +
      labs(title = sprintf("%s Outlier Removal (Iter %d) - Std. Residuals", method, i), 
           x = "Concentration", y = "Std. Residuals") +
      theme_report()
    
    # Print plots individually
    print(p_fit)
    print(p_res)
    
    # --- OUTLIER DETECTION & METRICS ---
    
    is_calculated_outlier <- abs(std_resids) > threshold
    
    if(sum(is_calculated_outlier) == 0) {
      message("No outliers found. Convergence reached.\n")
      break
    }
    
    removed_data <- df[is_calculated_outlier, ]
    tp <- sum(removed_data$is_outlier == TRUE)
    fp <- sum(removed_data$is_outlier == FALSE)
    fn <- sum(df$is_outlier == TRUE) - tp
    
    # Iteration metrics
    prec_iter <- ifelse((tp + fp) == 0, NA, tp / (tp + fp))
    rec_iter <- ifelse((tp + fn) == 0, NA, tp / (tp + fn))
    
    # Cumulative metrics
    cum_tp <- cum_tp + tp
    cum_fp <- cum_fp + fp
    prec_cum <- ifelse((cum_tp + cum_fp) == 0, NA, cum_tp / (cum_tp + cum_fp))
    rec_cum <- ifelse(total_real_outliers == 0, NA, cum_tp / total_real_outliers)
    
    message(sprintf("Summary: Removed %d true, %d false. | Cum. Precision: %.2f, Cum. Recall: %.2f", 
                    tp, fp, prec_cum, rec_cum))
    
    metrics <- rbind(metrics, data.frame(
      iteration = i,
      removed_true_outliers = tp,
      removed_valid_data = fp,
      remaining_outliers = fn
    ))
    
    message(sprintf("Summary: Removed %d real outliers and %d valid points.", tp, fp))
    
    # Remove and save iteration data
    df <- df[!is_calculated_outlier, ]
    new_name <- paste0(base_name, "_", method, "_iter_", i, ".txt")
    write_delim(df, here("data", new_name))
  }
  
  metrics$Method <- method
  return(metrics)
}

# ==============================================================================
# 3. Execution
# ==============================================================================

# Run the analysis loop for both methods
metrics_ols <- run_cleaning_loop(df_original, "OLS", max_iters, threshold, base_name_clean)
metrics_ts  <- run_cleaning_loop(df_original, "Theil-Sen", max_iters, threshold, base_name_clean)

# Combine metrics
all_metrics <- bind_rows(metrics_ols, metrics_ts)

# ==============================================================================
# 4. Final Performance Comparison Plot
# ==============================================================================

message("Generating final performance comparison plot...")

# Pivot data for stacked bar chart compatibility
metrics_long <- all_metrics %>%
  pivot_longer(cols = c(removed_true_outliers, removed_valid_data),
               names_to = "Category",
               values_to = "Count") %>%
  mutate(Category = recode(Category, 
                           "removed_true_outliers" = "True Outlier (Good)",
                           "removed_valid_data" = "Valid Data (Bad)"),
         # Keep standard method names for the facet headers
         Method = factor(Method, levels = c("OLS", "Theil-Sen")))

p_summary <- ggplot(metrics_long, aes(x = factor(iteration), y = Count, fill = Category)) +
  geom_col(color = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("True Outlier (Good)" = "forestgreen", 
                               "Valid Data (Bad)" = "firebrick")) +
  # Standard facet wrap (places "OLS" and "Theil-Sen" at the top)
  facet_wrap(~ Method, ncol = 2) + 
  # Standard global x-axis label
  labs(x = "Iteration", 
       y = "Points Removed") +
  coord_cartesian(clip = "off") +
  theme_report() +
  theme(legend.title = element_blank(),
        # Add a little extra bottom margin so you have room for (a) and (b) in Inkscape
        plot.margin = margin(t = 15, r = 15, b = 30, l = 15))

# Print final plot to RStudio
print(p_summary)

# Export fitting a standard IEEE double-column width (7.16 inches)
output_dir <- "figures"
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

save_path <- file.path(output_dir, "09_iterative_cleaning_summary_2.pdf")
#ggsave(save_path, plot = p_summary, device = cairo_pdf, width = 3.4, height = 2.8)

message(paste("Summary plot exported successfully to:", save_path))