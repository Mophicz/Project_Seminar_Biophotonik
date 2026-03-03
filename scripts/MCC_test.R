rm(list = ls())

library(readr)
library(here)

data_path <- here("data", "df_sim_4.txt")
# Load data
df <- read_table(data_path)

# make containers
conc <- unique(df$conc)
median_all <- numeric(length(conc))
mad_all <- numeric(length(conc))
mean_all <- numeric(length(conc))
sd_all <- numeric(length(conc))

# fill containers
for(i in seq_along(conc)) {
  idx <- df$conc == conc[i]
  median_all[i] <- median(df$signal_out[idx])
  mad_all[i] <- mad(df$signal_out[idx])
  mean_all[i] <- mean(df$signal_out[idx])
  sd_all[i] <- sd(df$signal_out[idx])
}

# z-scores
z_score_classic <- numeric(nrow(df))
z_score_robust <- numeric(nrow(df))

for(i in seq_along(conc)) {
  idx <- df$conc == conc[i]
  
  z_score_classic[idx] <- (df$signal_out[idx] - mean_all[i]) / sd_all[i]
  z_score_robust[idx] <- (df$signal_out[idx] - median_all[i]) / mad_all[i]
}

plot(z_score_classic)

thresh <- seq(0, 5, by=0.1)

# determine outliers
outlier_classic <- matrix(nrow=length(thresh), ncol=100)
outlier_robust <- matrix(nrow=length(thresh), ncol=100)

for (t in seq_along(thresh)){
  # determine outliers
  outlier_classic[t, ] <- abs(z_score_classic) >= thresh[t]
  outlier_robust[t, ] <- abs(z_score_robust) >= thresh[t]
}

# get prediction stats
TP_classic <- outlier_classic %*% df$is_outlier
FP_classic <- outlier_classic %*% (!df$is_outlier)
FN_classic <- (!outlier_classic) %*% df$is_outlier
TN_classic <- (!outlier_classic) %*% (!df$is_outlier)

TP_robust <- outlier_robust %*% df$is_outlier
FP_robust <- outlier_robust %*% (!df$is_outlier)
FN_robust <- (!outlier_robust) %*% df$is_outlier
TN_robust <- (!outlier_robust) %*% (!df$is_outlier)

# Denominators
denom_classic <- (TP_classic + FP_classic) * (TP_classic + FN_classic) * (TN_classic + FP_classic) * (TN_classic + FN_classic)
denom_robust <- (TP_robust + FP_robust) * (TP_robust + FN_robust) * (TN_robust + FP_robust) * (TN_robust + FN_robust)

MCC_classic <- ifelse(denom_classic > 0, 
                      (TP_classic * TN_classic - FP_classic * FN_classic) / sqrt(denom_classic), 
                      NA)

MCC_robust <- ifelse(denom_robust > 0, 
                     (TP_robust * TN_robust - FP_robust * FN_robust) / sqrt(denom_robust), 
                     NA)

plot(thresh, MCC_classic, type="l", col="red", ylim=range(-1, 1))

lines(thresh, MCC_robust, col = "blue")

