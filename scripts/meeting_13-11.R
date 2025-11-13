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

conc <- unique(df$conc)
median_all <- numeric(length(conc))
mad_all <- numeric(length(conc))
mean_all <- numeric(length(conc))
sd_all <- numeric(length(conc))

for(i in seq_along(conc)) {
  idx <- df$conc == conc[i]
  median_all[i] <- median(df$signal_out[idx])
  mad_all[i] <- mad(df$signal_out[idx])
  mean_all[i] <- mean(df$signal_out[idx])
  sd_all[i] <- sd(df$signal_out[idx])
}

# z-score
z_score1 <- (df$signal_out[df$conc == conc[1]] - mean_all[1]) / sd_all[1]

(df$signal_out[df$conc == conc[5]] - mean_all[5]) / sd_all[5]

plot(rep(10, 20), z_score1)
abline(h=3)

z_score2 <- (df$signal_out[df$conc == conc[1]] - median_all[1]) / mad_all[1]

plot(rep(10, 20), z_score2)
abline(h=3)

length(which(z_score2 >= 3))

