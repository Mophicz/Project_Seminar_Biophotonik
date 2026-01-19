rm(list = ls())

library(here)
library(readr)

# Define file paths
data_path <- here("data", "df_sim_4.txt")
save_path <- here("outputs", "signal_vs_concentration.png")

# Load data
df <- read_table(data_path)

# get unique concentration values
conc <- unique(df$conc)

# get indexes of a single concentration value
idx <- df$conc == conc[1]
signal <- df$signal_out[idx]

median <- median(signal)
mad <- mad(signal)
mean <- mean(signal)
sd <- sd(signal)


plot(signal)
# abline(h=mean)
# abline(h=mean + sd, col="red")

# abline(h=median)
# abline(h=median + mad, col="red")

# trimm data
sorted_idx <- order(signal)
n <- length(signal)
n_trimm <- floor(n * 0.2)
keeper_idx <- sorted_idx[(n_trimm + 1) : (n - n_trimm)]

signal_trimmed <- signal[sort(keeper_idx)]

mean_trimmed <- mean(signal_trimmed)
sd_trimmed <- sd(signal_trimmed)

abline(h=mean_trimmed)
abline(h=mean_trimmed + sd_trimmed, col="red")
