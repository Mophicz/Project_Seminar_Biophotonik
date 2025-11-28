rm(list = ls())

library(here)
library(readr)

source(here("scripts", "ols.R"))

# Define file paths
data_path <- here("data", "df_sim_4.txt")
save_path <- here("outputs", "signal_vs_concentration.png")

# Load data
df <- read_table(data_path)

# make containers
conc <- unique(df$conc)
iqr_all <- numeric(length(conc))

# fill containers
for(i in seq_along(conc)) {
  idx <- df$conc == conc[i]
  iqr_all[idx] <- IQR(df$signal_out[idx])
}

idx <- df$conc == conc[1]
signal <- df$signal_out[idx]

Q1 <- quantile(signal, 0.25)
Q3 <- quantile(signal, 0.75)

upper <- Q3 + (1.5 * iqr_all[idx])
lower <- Q1 - (1.5 * iqr_all[idx])

plot(rep(10, 20), signal)
abline(h=upper, col="red")
abline(h=lower, col="red")