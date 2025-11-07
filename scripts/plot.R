library(here)
library(readr)
library(ggplot2)

# Define file paths
data_path <- here("data", "df_sim_4.txt")
save_path <- here("outputs", "signal_vs_concentration.png")

# Load data
df <- read_table(data_path)

# Plot
plot <- ggplot(df, aes(x = conc_jitter, y = signal_out, color = is_outlier)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("FALSE" = "gray30", "TRUE" = "red")) +
  labs(
    title = "Signal vs. Concentration",
    x = "Jittered Concentration",
    y = "Signal Output",
    color = "Outlier"
  ) +
  theme_minimal()

# Display
print(plot)

# Save
ggsave(save_path, plot = plot, width = 8, height = 6, dpi = 300)

print(paste("Plot successfully saved to:", save_path))
