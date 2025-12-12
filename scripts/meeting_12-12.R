rm(list = ls())
dev.off()

# Define file paths
data_path <- here("data", "df_sim_4.txt")
save_path <- here("outputs", "signal_vs_concentration.png")

# Load data
df <- read_table(data_path)

lin_reg <- lm(y~x, data = list(x=df$conc, y=df$signal_out))

plot(lin_reg$model$x, lin_reg$model$y)
abline(lin_reg)

plot(lin_reg$model$x, residuals(lin_reg))

plot(lin_reg$model$x, rstandard(lin_reg))

plot(lin_reg$model$x, rstudent(lin_reg))

plot(lin_reg$model$x, hatvalues(lin_reg))

plot(cooks.distance(lin_reg),type = "h")
abline(h = 4/nobs(lin_reg))

plot(hatvalues(lin_reg), rstudent(lin_reg), xlim = range(hatvalues(lin_reg), 3*(length(coefficients(lin_reg))/nobs(lin_reg))))
abline(h = 3)
abline(v = 3*(length(coefficients(lin_reg))/nobs(lin_reg)))