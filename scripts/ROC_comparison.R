rm(list = ls())

library(here)
library(readr)
library(pROC)

source(here("scripts", "ols.R"))

# import data

data_path <- here("data", "df_sim_4.txt")
df <- read_table(data_path)

# define metrics

conc <- unique(df$conc)                # concentration groups

mean_all <- numeric(length(conc))      # mean per group
sd_all <- numeric(length(conc))        # standard deviation per group

iqr_all <- numeric(length(conc))       # interquartile distance per group

median_all <- numeric(length(conc))    # median per group
mad_all <- numeric(length(conc))       # median absolute deviation per group

trim_mean_all <- numeric(length(conc)) # trimmed mean (10%) per group 
trim_sd_all <- numeric(length(conc))   # trimmed standard deviation (10%) per group

wins_mean_all <- numeric(length(conc)) # winsorized mean (10%) per group
wins_sd_all <- numeric(length(conc))   # winsorized standard deviation (10%) per group

# calculate metrics

for(i in seq_along(conc)) {
  idx <- df$conc == conc[i]

  mean_all[i] <- mean(df$signal_out[idx])
  sd_all[i] <- sd(df$signal_out[idx])

  median_all[i] <- median(df$signal_out[idx])
  mad_all[i] <- mad(df$signal_out[idx])
  
  iqr_all[i] <- IQR(df$signal_out[idx])
  
  trim_mean_all[i] <- mean(df$signal_out[idx], trim = 0.1)
  q_limits <- quantile(df$signal_out[idx], probs = c(0.1, 0.9))
  trim_dat <- df$signal_out[idx][df$signal_out[idx] >= q_limits[1] & df$signal_out[idx] <= q_limits[2]]
  trim_sd_all[i] <- sd(trim_dat)
  
  wins_dat <- df$signal_out[idx]
  wins_dat[wins_dat < q_limits[1]] <- q_limits[1]
  wins_dat[wins_dat > q_limits[2]] <- q_limits[2]
  
  wins_mean_all[i] <- mean(wins_dat)
  wins_sd_all[i] <- sd(wins_dat)
}

# define z-scores

z_score_mean <- numeric(nrow(df))
z_score_median <- numeric(nrow(df))
z_score_iqr <- numeric(nrow(df))
z_score_trim <- numeric(nrow(df))
z_score_wins <- numeric(nrow(df))

# calculate z-scores

for(i in seq_along(conc)) {
  idx <- df$conc == conc[i]
  
  z_score_mean[idx] <- (df$signal_out[idx] - mean_all[i]) / sd_all[i]
  z_score_median[idx] <- (df$signal_out[idx] - median_all[i]) / mad_all[i]
  z_score_iqr[idx] <- (df$signal_out[idx] - median_all[i]) / (iqr_all[i] / 1.349)
  z_score_trim[idx] <- (df$signal_out[idx] - trim_mean_all[i]) / trim_sd_all[i]
  z_score_wins[idx] <- (df$signal_out[idx] - wins_mean_all[i]) / wins_sd_all[i]
}

# ROCs

roc_mean <- roc(
  response = as.vector(df$is_outlier),
  predictor = z_score_mean,
  direction = ">",
  levels = c(TRUE, FALSE)
)

roc_median <- roc(
  response = as.vector(df$is_outlier),
  predictor = z_score_median,
  direction = ">",
  levels = c(TRUE, FALSE)
)

roc_iqr <- roc(
  response = as.vector(df$is_outlier),
  predictor = z_score_iqr,
  direction = ">",
  levels = c(TRUE, FALSE)
)

roc_trim <- roc(
  response = as.vector(df$is_outlier),
  predictor = z_score_trim,
  direction = ">",
  levels = c(TRUE, FALSE)
)

roc_wins <- roc(
  response = as.vector(df$is_outlier),
  predictor = z_score_wins,
  direction = ">",
  levels = c(TRUE, FALSE)
)

# plot
par(mfrow = c(2, 3))

plot(roc_mean, main = "Mean + SD", col = "black")
legend("bottomright", legend = paste("AUC =", round(roc_mean$auc, 3)), bty = "n")

plot(roc_median, main = "Median + MAD", col = "blue")
legend("bottomright", legend = paste("AUC =", round(roc_median$auc, 3)), bty = "n")

plot(roc_iqr, main = "Median + IQR", col = "green")
legend("bottomright", legend = paste("AUC =", round(roc_iqr$auc, 3)), bty = "n")

plot(roc_trim, main = "Trimmed", col = "red")
legend("bottomright", legend = paste("AUC =", round(roc_trim$auc, 3)), bty = "n")

plot(roc_wins, main = "Winsorized", col = "orange")
legend("bottomright", legend = paste("AUC =", round(roc_wins$auc, 3)), bty = "n")

