rm(list = ls())

library(here)
library(readr)
library(ggplot2)

source(here("scripts", "ols.R"))

# Define file paths
data_path <- here("data", "df_sim_4.txt")
save_path <- here("outputs", "signal_vs_concentration.png")

# Load data
df <- read_table(data_path)

regression_df <- ols(df$conc_jitter, df$signal_out)

df_filtered <- subset(df, is_outlier == FALSE)
regression_df_no_outliers <- ols(df_filtered$conc_jitter, df_filtered$signal_out)

# Plot
plot <- ggplot(df, aes(x = conc_jitter, y = signal_out, color = is_outlier)) +
  
  geom_point(alpha = 0.7) +
  
  #geom_line(data = regression_df, 
  #          aes(x = x, y = y_predicted, 
  #              color = "With Outliers"),
  #          linetype = "solid", 
  #          linewidth = 1,
  #          inherit.aes = FALSE,
  #          alpha = 0.7) +
  
  # geom_line(data = regression_df_no_outliers, 
  #           aes(x = x, y = y_predicted, 
  #               color = "Without Outliers"),
  #           linetype = "solid", 
  #           linewidth = 1,
  #           inherit.aes = FALSE,
  #           alpha = 0.7) +
  
  scale_color_manual(
    name = "Outlier", 
    values = c(
      "FALSE" = "gray30", 
      "TRUE" = "red",
      "With Outliers" = "blue",
      "Without Outliers" = "green"
    )
  ) +
  
  labs(
    title = "Signal vs. Concentration",
    x = "Jittered Concentration",
    y = "Signal Output"
  ) +
  
  theme_minimal()

# Display
print(plot)

# Save
ggsave(save_path, plot = plot, width = 8, height = 6, dpi = 300)

print(paste("Plot successfully saved to:", save_path))
