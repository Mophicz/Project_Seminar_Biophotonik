# -------------------------------------------------------------------
# OLS UTILITY FUNCTION
#
# This script defines a function `ols` that calculates
# OLS coefficients and returns the predicted data points.
# -------------------------------------------------------------------

#' Calculate OLS Regression Points
#'
#' Fits a simple linear regression model and returns
#' the predicted y-values for each input x-value.
#'
#' @param x_values Numeric vector of independent variable data.
#' @param y_values Numeric vector of dependent variable data.
#'
#' @return A data.frame with two columns: `x` (the original x_values)
#'   and `y_predicted` (the corresponding predicted y-values on the
#'   regression line). Returns `NULL` if input is invalid.
#'
ols <- function(x_values, y_values) {
  
  # Basic validation
  if (length(x_values) != length(y_values)) {
    stop("Error: x and y vectors must be the same length.")
  }
  if (!is.numeric(x_values) || !is.numeric(y_values)) {
    stop("Error: x and y vectors must be numeric.")
  }
  if (length(x_values) < 2) {
    stop("Error: At least two data points are required for regression.")
  }
  
  # --- 1. Calculate OLS Coefficients ---
  
  # Calculate means
  x_mean <- mean(x_values)
  y_mean <- mean(y_values)
  
  # Calculate slope (beta_1)
  beta_1_numerator <- sum((x_values - x_mean) * (y_values - y_mean))
  beta_1_denominator <- sum((x_values - x_mean)^2)
  
  # Check for zero variance in x
  if (beta_1_denominator == 0) {
    warning("Cannot calculate slope: variance of x is zero.")
    return(NULL)
  }
  
  beta_1 <- beta_1_numerator / beta_1_denominator
  
  # Calculate intercept (beta_0)
  beta_0 <- y_mean - (beta_1 * x_mean)
  
  # --- 2. Calculate Predicted Y-Values ---
  y_predicted <- beta_0 + beta_1 * x_values
  
  # --- 3. Return Data Points ---
  regression_points_df <- data.frame(
    x = x_values,
    y_predicted = y_predicted
  )
  
  return(regression_points_df)
}