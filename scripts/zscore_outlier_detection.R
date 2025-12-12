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

point_colors <- ifelse(df$is_outlier, "red", "black")

par(mfrow = c(1, 2))

plot(z_score_classic, col = point_colors)
abline(h = 3, col = "red")
abline(h = -3, col = "red")

plot(z_score_robust, col = point_colors)
abline(h = 3, col = "red")
abline(h = -3, col = "red")

# # determine outliers
# outlier_classic <- z_score_classic >= 3
# outlier_robust <- z_score_robust >= 3
# 
# # get prediction stats
# TP_classic <- sum(outlier_classic & df$is_outlier)
# FP_classic <- sum(outlier_classic & !df$is_outlier)
# FN_classic <- sum(!outlier_classic & df$is_outlier)
# TN_classic <- sum(!outlier_classic & !df$is_outlier)
# 
# TP_robust <- sum(outlier_robust & df$is_outlier)
# FP_robust <- sum(outlier_robust & !df$is_outlier)
# FN_robust <- sum(!outlier_robust & df$is_outlier)
# TN_robust <- sum(!outlier_robust & !df$is_outlier)
# 
# # alternatively use package
# library(caret)
# 
# predicted <- factor(c(outlier_robust))
# actual <- factor(c(df$is_outlier))
# 
# conf_matrix <- confusionMatrix(predicted, actual)
# 
# print(conf_matrix)
# 
# # meeting 28.11
# library("pROC")
# roc_obj <- roc(
#   response = as.vector(df$is_outlier),
#   predictor = z_score_classic,
#   direction = ">",
#   levels = c(TRUE, FALSE)
# )
# 
# plot(
#   1 - roc_obj$specificities,
#   roc_obj$sensitivities,
#   typ = "l"
# )
# 
# roc_obj$auc
