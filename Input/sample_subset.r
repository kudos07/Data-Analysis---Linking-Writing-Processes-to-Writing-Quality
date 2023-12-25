library(readr)
library(dplyr)
library(ggplot2)
library(jsonlite)
set.seed(42)
train_logs <- read_csv("train_logs.csv")
train_scores <- read_csv("train_scores.csv")

count_data <- train_logs %>% 
  group_by(id) %>% 
  summarise(count = n())

# sample 200 recordings and save
merged_data <- inner_join(count_data, train_scores, by = "id")
subset_data <- merged_data %>% sample_n(200)

write.csv(merged_data, "merged_data.csv", row.names = FALSE)
write.csv(subset_data, "subset_data.csv", row.names = FALSE)
merged_data <- read_csv("merged_data.csv")
subset_data <- read_csv("subset_data.csv")
 
plot_density_histogram <- function(path_1, path_2, column_name, file_name, label1, label2) {
  data_1 <- read_csv(path_1)
  data_2 <- read_csv(path_2)
    
  mean_1 <- mean(data_1[[column_name]], na.rm = TRUE)
  var_1 <- var(data_1[[column_name]], na.rm = TRUE)
  mean_2 <- mean(data_2[[column_name]], na.rm = TRUE)
  var_2 <- var(data_2[[column_name]], na.rm = TRUE)
  num_samples_1 <- nrow(data_1)
  num_samples_2 <- nrow(data_2)

  p <- ggplot() +
    geom_density(data = data_1, aes(x = !!sym(column_name)), fill = "blue", alpha = 0.5) +
    geom_density(data = data_2, aes(x = !!sym(column_name)), fill = "red", alpha = 0.5) +
    labs(title = paste("Density Histogram of", column_name),
         x = column_name,
         y = "Density",
         subtitle = sprintf("%s | %s", label1, label2)) + 
    theme_light() +  
    annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 1.1,
             label = sprintf("%s\nMean: %.2f\nVar: %.2f\nSamples: %d", label1, mean_1, var_1, num_samples_1),
             color = "blue") +
    annotate("text", x = Inf, y = Inf, hjust = 1.1, vjust = 3.3,
             label = sprintf("%s\nMean: %.2f\nVar: %.2f\nSamples: %d", label2, mean_2, var_2, num_samples_2),
             color = "red")

  ggsave(file_name, plot = p, width = 10, height = 6, units = "in")
}

# plot the score/count distribution of subset and fullset
plot_density_histogram("merged_data.csv", "subset_data.csv", "count", "count_density_comparison.png", "Merged Data", "Subset Data")
plot_density_histogram("merged_data.csv", "subset_data.csv", "score", "score_density_comparison.png", "Merged Data", "Subset Data")

train_logs <- read.csv("train_logs.csv")
subset_data <- read.csv("subset_data.csv")

filtered_train_logs <- train_logs %>%
  filter(id %in% subset_data$id)

merged_data <- filtered_train_logs %>%
  inner_join(subset_data, by = "id")

# prepare recording data for further variable computation
ordered_data <- merged_data %>%
  arrange(id, event_id) %>%
  group_by(id) %>%
  do(recording = .[, -c(which(names(.) %in% c("id", "count", "score")))]) %>%
  left_join(subset_data[, c("id", "score")], by = "id") %>%
  select(id, score, recording)

json_data <- toJSON(ordered_data, pretty = TRUE)
write(json_data, file = "subset_recordings.json")
