library(readr)
library(here)

rm(list = ls())
dev.off()

# Define file paths
data_path <- here("data", "df_sim_4.txt")
save_path <- here("outputs", "signal_vs_concentration.png")

# Load data
df <- read_table(data_path)

# linear regression
lin_reg <- lm(y~x, data = list(x=df$conc, y=df$signal_out))

# plot of linear regression
plot(lin_reg$model$x, lin_reg$model$y)  # input data
abline(lin_reg)                         # linear regression output
  
# plot of residuals --> removes offset from data
plot(lin_reg$model$x, residuals(lin_reg))

# plot of normalized residuals --> equivalent to z-score
plot(lin_reg$model$x, rstandard(lin_reg))

# here residuals normalized with student --> leave current point out of data when calculating standard deviation
plot(lin_reg$model$x, rstudent(lin_reg))

# plot of leverage with average and 3*average outlier threshold
lev = hatvalues(lin_reg)
mean_lev = mean(lev)
thresh_lev = 3*mean_lev

plot(lin_reg$model$x, lev, ylim = range(lev, thresh_lev))
abline(h = mean_lev)
abline(h = thresh_lev, col = 'red')

# Cooks distance
plot(cooks.distance(lin_reg),type = "h")
abline(h = 4/nobs(lin_reg))

# Williams plot
plot(hatvalues(lin_reg), rstudent(lin_reg), xlim = range(hatvalues(lin_reg), 3*(length(coefficients(lin_reg))/nobs(lin_reg))))
abline(h = 3)
abline(v = 3*(length(coefficients(lin_reg))/nobs(lin_reg)))