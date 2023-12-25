library(ggplot2)
library(readr)
library(reshape2)

data <- read.csv("V0.csv")
high_score_data <- subset(data, score >= 4)
low_score_data <- subset(data, score < 4)

# compare measures distributions between high score group and low score group 
plot_density_histogram <- function(data_high, data_low, column_name, file_name) {
  mean_high <- mean(data_high[[column_name]], na.rm = TRUE)
  var_high <- var(data_high[[column_name]], na.rm = TRUE)
  mean_low <- mean(data_low[[column_name]], na.rm = TRUE)
  var_low <- var(data_low[[column_name]], na.rm = TRUE)

  num_samples_high <- nrow(data_high)
  num_samples_low <- nrow(data_low)

  p <- ggplot() +
    geom_density(data = data_high, aes_string(x = column_name), fill = "blue", alpha = 0.5) +
    geom_density(data = data_low, aes_string(x = column_name), fill = "red", alpha = 0.5) +
    labs(title = paste("Density Histogram of", column_name),
         x = column_name,
         y = "Density") +
    theme_light() +  
    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1,
             label = sprintf("High Score\nMean: %.2f\nVar: %.2f\nSamples: %d", mean_high, var_high, num_samples_high),
             color = "blue") +
    annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 3.3,
             label = sprintf("Low Score\nMean: %.2f\nVar: %.2f\nSamples: %d", mean_low, var_low, num_samples_low),
             color = "red")

  ggsave(file_name, plot = p, width = 10, height = 6, units = "in")
}

columns_to_plot <- colnames(data)[!colnames(data) %in% c("recording_id", "score")]
for (col in columns_to_plot) {
  file_name <- paste0("plot_", col, ".png")
  plot_density_histogram(high_score_data, low_score_data, col, file_name)
}

# Z test for each measure variable
perform_z_test <- function(data, column) {
  high_score_data <- subset(data, score >= 4)
  low_score_data <- subset(data, score < 4)

  x <- high_score_data[[column]]
  y <- low_score_data[[column]]

  mean_x <- mean(x, na.rm = TRUE)
  mean_y <- mean(y, na.rm = TRUE)
  sd_x <- sd(x, na.rm = TRUE)
  sd_y <- sd(y, na.rm = TRUE)
  n_x <- sum(!is.na(x))
  n_y <- sum(!is.na(y))

  z <- (mean_x - mean_y) / sqrt(sd_x^2/n_x + sd_y^2/n_y)
  p_value <- 2 * pnorm(-abs(z))

  return(c(mean_x = mean_x, mean_y = mean_y, sd_x = sd_x, sd_y = sd_y, n_x = n_x, n_y = n_y, z = z, p_value = p_value))
}

process_csv <- function(file_path) {
  data <- read_csv(file_path)

  columns_to_test <- colnames(data)[!colnames(data) %in% c("recording_id", "score")]
  results <- sapply(columns_to_test, function(column) perform_z_test(data, column))

  results_df <- as.data.frame(t(results))
  rownames(results_df) <- columns_to_test

  output_file <- gsub(".csv", "_analysis_results.csv", file_path)
  write.csv(results_df, output_file, row.names = TRUE)
}

process_csv("V0.csv")

process_csv("R10.csv")

process_csv("R20.csv")

process_csv("R30.csv")

process_csv("NR.csv")

process_data <- function(file_path, version) {
  data <- read.csv(file_path)
  names(data)[1] <- "Variable"
  p_value_col <- grep("p_value", names(data), value = TRUE)
  if (length(p_value_col) == 0) {
    stop("No column containing 'p_value' found in", file_path)
  }
  data <- data[, c("Variable", p_value_col)]
  names(data)[names(data) == p_value_col] <- paste("p_value", version, sep = "_")
  return(data)
}

file_paths <- list(
  V0 = "V0_analysis_results.csv",
  R10 = "R10_analysis_results.csv",
  R20 = "R20_analysis_results.csv",
  R30 = "R30_analysis_results.csv",
  NR = "NR_analysis_results.csv"
)

all_data <- Reduce(function(x, y) merge(x, y, by = "Variable", all = TRUE), 
                   lapply(names(file_paths), function(x) process_data(file_paths[[x]], x)))

# compares results of 5 versions of data and 19 variables
plot_subset <- function(data, start_idx, end_idx, file_name) {
  subset_data <- data[start_idx:end_idx, ]
  subset_data_long <- melt(subset_data, id.vars = "Variable")
  
  p <- ggplot(subset_data_long, aes(x = Variable, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = position_dodge()) +
    geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(x = "Variables", y = "P-values", title = paste("P-values for Variables"))
    
  ggsave(file_name, plot = p, width = 10, height = 6, units = "in")
}

plot_subset(all_data, 1, 19, "comparisons.png")
