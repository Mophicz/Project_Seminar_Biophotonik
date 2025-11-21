rm(list = ls())

library(here)
library(readr)

source(here("scripts", "ols.R"))

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
abline(h=mean)
abline(h=mean + sd, col="red")


plot(signal)
abline(h=median)
abline(h=median + mad, col="red")