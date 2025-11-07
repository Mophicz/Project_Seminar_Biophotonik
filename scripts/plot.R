# Load required libraries
# If you don't have these, run this line in your console first:
# install.packages(c("here", "readr", "ggplot2"))
library(here)
library(readr)
library(ggplot2)

# Define file paths (here() automatically finds your project root)
data_path <- here("data", "df_sim_4.txt")
save_path <- here("outputs", "signal_vs_concentration.png")

# Load the data
# read_table is for whitespace-separated files like your sample
df <- read_table(data_path)

# Create the plot
# Plot signal vs. concentration, and color the points by outlier status
plot <- ggplot(df, aes(x = conc_jitter, y = signal_out, color = is_outlier)) +
  geom_point(alpha = 0.7) +  # Use semi-transparent points
  scale_color_manual(values = c("FALSE" = "gray30", "TRUE" = "red")) + # Make outliers red
  labs(
    title = "Signal vs. Concentration",
    x = "Jittered Concentration",
    y = "Signal Output",
    color = "Outlier"
  ) +
  theme_minimal() # Use a clean plot theme

# Save the plot to the outputs/ folder
ggsave(save_path, plot = plot, width = 8, height = 6, dpi = 300)

# Optional: Print a confirmation message to the console
print(paste("Plot successfully saved to:", save_path))