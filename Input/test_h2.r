library(ggplot2)
library(dplyr)

library(car)
library(lmtest)

perform_analysis <- function(csv_path, output_csv_path) {
  temp <- read.csv(csv_path)
  data <- temp[temp$score >= 2, ]

  linear_model <- lm(score ~ proportion_of_deletions + proportion_of_pause_time + length_of_p_bursts + proportion_of_r_bursts + length_of_deletions + mean_pause_length + standard_deviation_per_interval, data = data)
  results <- data.frame(Metric = character(), Value = numeric())

  shapiro_results <- shapiro.test(residuals(linear_model))
  results <- rbind(results, data.frame(Metric = "shapiro p-value", Value = shapiro_results$p.value))

  bp_test <- bptest(linear_model)
  results <- rbind(results, data.frame(Metric = "bptest p-value", Value = bp_test$p.value))

  dw_test <- dwtest(linear_model)
  results <- rbind(results, data.frame(Metric = "dwtest p-value", Value = dw_test$p.value))

  vif_results <- vif(linear_model)
  vif_names <- names(vif_results)
  for (i in 1:length(vif_results)) {
    results <- rbind(results, data.frame(Metric = paste("vif", vif_names[i], sep = "-"), Value = vif_results[i]))
  }

  coefs <- summary(linear_model)$coefficients
  for (i in 1:nrow(coefs)) {
    results <- rbind(results, data.frame(Metric = paste("coefficient", rownames(coefs)[i], sep = "-"), Value = coefs[i, 1]))
    results <- rbind(results, data.frame(Metric = paste("p-value", rownames(coefs)[i], sep = "-"), Value = coefs[i, 4]))
  }

  results <- rbind(results, data.frame(Metric = "R squared value", Value = summary(linear_model)$r.squared))
  results <- rbind(results, data.frame(Metric = "adjusted R squared value", Value = summary(linear_model)$adj.r.squared))
  write.csv(results, output_csv_path, row.names = FALSE)
}

# linear regression on 5 versions of data, save results including tests for assumptions
perform_analysis("V0.csv", "V0_H2.csv")
perform_analysis("R10.csv", "R10_H2.csv")
perform_analysis("R20.csv", "R20_H2.csv")
perform_analysis("R30.csv", "R30_H2.csv")
perform_analysis("NR.csv", "NR_H2.csv")

files <- c("V0_H2.csv", "R10_H2.csv", "R20_H2.csv", "R30_H2.csv", "NR_H2.csv")
version_names <- c("V0", "R10", "R20", "R30", "NR")

all_data <- lapply(files, read.csv, stringsAsFactors = FALSE)
for (i in 1:length(all_data)) {
    all_data[[i]]$Version <- version_names[i]
}
combined_data <- do.call(rbind, all_data)

# visualize results for comparisons
save_custom_plot <- function(plot, filename) {
    g <- plot + 
         theme_light() +
         theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
         coord_cartesian(expand = 0.05) +
         theme(legend.position="bottom")
    ggsave(filename, plot = g, width = 10, height = 6, dpi = 300)
}

plot_tests <- ggplot(test_data, aes(x = Metric, y = Value, fill = Version)) +
              geom_bar(stat = "identity", position = position_dodge()) +
              geom_hline(yintercept = 0.05, linetype = "dotted", color = "red") +
              labs(title = "Comparison of Test P-values", x = "Test", y = "P-value") +
              scale_fill_brewer(palette = "Set1")
save_custom_plot(plot_tests, "tests_comparison.png")

plot_coefs <- ggplot(coef_data, aes(x = Metric, y = Value, fill = Version)) +
              geom_bar(stat = "identity", position = position_dodge()) +
              labs(title = "Comparison of Coefficients", x = "Coefficient", y = "Value") +
              scale_fill_brewer(palette = "Set1")
save_custom_plot(plot_coefs, "coefficients_comparison.png")

plot_coef_pvals <- ggplot(coef_p_data, aes(x = Metric, y = Value, fill = Version)) +
                   geom_bar(stat = "identity", position = position_dodge()) +
                   geom_hline(yintercept = 0.05, linetype = "dotted", color = "red") +
                   labs(title = "Comparison of Coefficient P-values", x = "Coefficient P-value", y = "P-value") +
                   scale_fill_brewer(palette = "Set1")
save_custom_plot(plot_coef_pvals, "coef_pvalues_comparison.png")

plot_vifs <- ggplot(vif_data, aes(x = Metric, y = Value, fill = Version)) +
             geom_bar(stat = "identity", position = position_dodge()) +
             geom_hline(yintercept = 5, linetype = "dotted", color = "red") +
             labs(title = "Comparison of VIF Values", x = "VIF", y = "Value") +
             scale_fill_brewer(palette = "Set1")
save_custom_plot(plot_vifs, "vif_comparison.png")

plot_r2 <- ggplot(r2_data, aes(x = Metric, y = Value, fill = Version)) +
           geom_bar(stat = "identity", position = position_dodge()) +
           labs(title = "Comparison of R-Squared Values", x = "R-Squared", y = "Value") +
           scale_fill_brewer(palette = "Set1")
save_custom_plot(plot_r2, "r2_comparison.png")