library(readr)
library(here)

rm(list = ls())

# Define settings
max_iters <- 12
data_filename <- "df_sim_4.txt" 

# Construct initial paths
data_path <- here("data", data_filename)
pdf_path <- here("outputs", "cleaning_report.pdf")

# Extract base name
base_name_clean <- sub("_iter_[0-9]+$", "", tools::file_path_sans_ext(data_filename))

# Load data
df <- read_table(data_path)

# Initialize storage for performance metrics
metrics <- data.frame(
  iteration = integer(),
  removed_true_outliers = integer(), # Good (TP)
  removed_valid_data = integer(),    # Bad (FP)
  remaining_outliers = integer()     # How many are left?
)

pdf(file = pdf_path)

for (i in 1:max_iters) {
  
  message(paste0("--- Starting Iteration ", i, " ---"))
  
  # linear regression
  lin_reg <- lm(y~x, data = list(x=df$conc, y=df$signal_out))
  
  plot_title <- paste0("Iteration ", i)
  
  # Plots
  plot(lin_reg$model$x, lin_reg$model$y, main = plot_title)
  abline(lin_reg)
  
  plot(lin_reg$model$x, rstudent(lin_reg), ylim = range(-3, rstudent(lin_reg), 3), main = paste(plot_title, "- Studentized"))
  abline(h = 3, col = 'red')
  abline(h = -3, col = 'red')
  
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
  
  # --- OUTLIER DETECTION ---
  
  thresh_cooks <- 4/nobs(lin_reg)
  is_calculated_outlier <- cooks.distance(lin_reg) > thresh_cooks
  
  # Convergence check
  if(sum(is_calculated_outlier) == 0) {
    message("No outliers found. Convergence reached.")
    
  }
  
  # --- METRIC CALCULATION (The new part) ---
  
  # Subset the data that is ABOUT to be removed
  removed_data <- df[is_calculated_outlier, ]
  
  # Count True Positives (We removed an actual outlier)
  tp <- sum(removed_data$is_outlier == TRUE)
  
  # Count False Positives (We removed good data!)
  fp <- sum(removed_data$is_outlier == FALSE)
  
  # Count how many real outliers are still left in the remaining data
  fn <- sum(df$is_outlier == TRUE) - tp
  
  # Save to metrics table
  metrics <- rbind(metrics, data.frame(
    iteration = i,
    removed_true_outliers = tp,
    removed_valid_data = fp,
    remaining_outliers = fn
  ))
  
  message(paste("Iter", i, "summary: Removed", tp, "real outliers and", fp, "valid points."))
  
  # --- REMOVAL AND SAVING ---
  
  df_new <- df[!is_calculated_outlier, ]
  
  ext <- tools::file_ext(data_filename)
  new_name <- paste0(base_name_clean, "_iter_", i, ".", ext)
  new_path <- here("data", new_name)
  
  write_delim(df_new, new_path)
  
df <- df_new
}

# --- CLOSE PDF FIRST ---
# This saves the diagnostic plots to the file
dev.off()
message("Diagnostic plots saved to: ", pdf_path)

# --- DISPLAY PERFORMANCE PLOT IN RSTUDIO ---
# Now that the PDF is closed, this plot will appear in your RStudio plot pane
# Calculate the total points removed per iteration to set the y-axis limit
total_removed <- metrics$removed_true_outliers + metrics$removed_valid_data
max_y <- max(total_removed)

barplot(
  t(as.matrix(metrics[, c("removed_true_outliers", "removed_valid_data")])), 
  beside = FALSE, 
  names.arg = paste("Iter", metrics$iteration),
  col = c("forestgreen", "red"),
  main = "Outlier Removal Accuracy per Iteration",
  ylab = "Count of Points Removed",
  ylim = c(0, max_y+1),
  legend.text = c("True Outlier (Good)", "Valid Data (Bad)"),
  args.legend = list(x = "topright")
)
