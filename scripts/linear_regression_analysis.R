library(readr)
library(here)

rm(list = ls())
#dev.off()

# Define file paths
#data_path <- here("data", "df_sim_4.txt")
data_path <- here("data", "df_sim_4_iter_3.txt")
#data_path <- here("data", "df_sim_clean_4.txt")

# Load data
df <- read_table(data_path)

# linear regression
lin_reg <- lm(y~x, data = list(x=df$conc, y=df$signal_out))

# plot of linear regression
plot(lin_reg$model$x, lin_reg$model$y)  # input data
abline(lin_reg)                         # linear regression output
  
# plot of residuals --> removes offset from data
plot(lin_reg$model$x, residuals(lin_reg))

# plot of standardized residuals --> equivalent to z-score
plot(lin_reg$model$x, rstandard(lin_reg), ylim = range(-3, rstandard(lin_reg), 3))
abline(h = 3, col = 'red')
abline(h = -3, col = 'red')

# here residuals normalized with student --> leave current point out of data when calculating standard deviation
plot(lin_reg$model$x, rstudent(lin_reg), ylim = range(-3, rstudent(lin_reg), 3))
abline(h = 3, col = 'red')
abline(h = -3, col = 'red')

# plot of leverage with average and 3*average outlier threshold
lev = hatvalues(lin_reg)
mean_lev = length(coefficients(lin_reg))/nobs(lin_reg)
thresh_lev = 2*mean_lev

plot(lin_reg$model$x, lev, ylim = range(lev, thresh_lev))
abline(h = mean_lev)
abline(h = thresh_lev, col = 'red')

# Cooks distance
plot(cooks.distance(lin_reg),type = "h")
abline(h = 4/nobs(lin_reg), col = 'red')

# Williams plot
plot(hatvalues(lin_reg), 
     rstudent(lin_reg), 
     xlim = range(
        hatvalues(lin_reg), 
        3*(length(coefficients(lin_reg))/nobs(lin_reg))
        ),
     ylim = range(-3, rstudent(lin_reg), 3)
)
abline(h = 3, col ='red')
abline(h = -3, col ='red')
abline(v = 3*(length(coefficients(lin_reg))/nobs(lin_reg)), col = 'red')

# ## remove outliers from data
# 
# # define threshold
# thresh_cooks <- 4/nobs(lin_reg)
# 
# # identify outliers
# is_outlier <- cooks.distance(lin_reg) > thresh_cooks
# 
# # remove outliers from data
# df_new <- df[!is_outlier, ]
# 
# # construct filename
# base_name <- tools::file_path_sans_ext(basename(data_path))
# ext <- tools::file_ext(data_path)
# new_name <- paste0(base_name, "_iter_1.", ext)
# new_path <- here("data", new_name)
# 
# # save new dataset
# write_delim(df_new, new_path)
# message("New dataset saved to: ", new_path)
