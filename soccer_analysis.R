# Load necessary libraries
library(dplyr)
library(stats)
library(ggplot2)
library(readxl)
library(BSDA)

# Define file path and sheet name
soccer_data_filepath <- '/Users/tuank/Sites/Soccer/soccer_data.xlsx'
selected_sheet <- "player_metrics"

# Read and prepare data
df <- read_excel(soccer_data_filepath, sheet = selected_sheet) %>%
  rename(player_id = player_api_id) %>%
  mutate(
    overall_rating = as.numeric(overall_rating),
    height = as.numeric(height),
    weight = as.numeric(weight)
  )

# Filter data for complete cases for overall_rating and height
df_valid <- df %>%
  filter(!is.na(overall_rating), !is.na(height), is.finite(overall_rating))

# I. Simple Linear Regression: overall_rating ~ height
regression_model <- lm(overall_rating ~ height, data = df_valid)
cat("Linear Regression Summary (overall_rating ~ height):\n")
print(summary(regression_model))
correlation_value <- cor(df_valid$height, df_valid$overall_rating, use = "complete.obs")
cat("Correlation between height and overall_rating:", correlation_value, "\n")

# II. Z-test to compare height differences between high and low overall_rating groups
median_rating <- median(df_valid$overall_rating, na.rm = TRUE)
high_rated <- df_valid %>% filter(overall_rating >= median_rating)
low_rated <- df_valid %>% filter(overall_rating < median_rating)

if (nrow(high_rated) > 30 && nrow(low_rated) > 30) {
  z_test_height <- z.test(
    x = high_rated$height,
    y = low_rated$height,
    alternative = "two.sided",
    mu = 0,
    sigma.x = sd(high_rated$height, na.rm = TRUE),
    sigma.y = sd(low_rated$height, na.rm = TRUE)
  )
  cat("Z-test for Height Differences Between High and Low Rated Players:\n")
  print(z_test_height)
} else {
  cat("Z-test requires over 30 samples in each group\n")
}

# III. K-means Clustering on overall_rating
# Determine optimal number of clusters using the elbow method
wss <- sapply(1:10, function(k) {
  kmeans(df_valid %>% select(overall_rating), centers = k, nstart = 25)$tot.withinss
})
wss_data <- data.frame(Clusters = 1:10, WSS = wss)
wss_plot <- ggplot(wss_data, aes(x = Clusters, y = WSS)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Elbow Method for Optimal Clusters", x = "Number of Clusters", y = "Total Within Sum of Squares") +
  scale_x_continuous(breaks = 1:10) +
  theme_minimal()
print(wss_plot)

# Perform K-means clustering on overall_rating with 3 clusters if enough points exist
if (nrow(df_valid) > 3) {
  set.seed(123)
  kmeans_result <- kmeans(df_valid %>% select(overall_rating), centers = 3, nstart = 25)
  df_valid$cluster <- as.factor(kmeans_result$cluster)
  cluster_centers <- data.frame(overall_rating = kmeans_result$centers[, 1])
  
  # Histogram to show the clusters
  hist_plot <- ggplot(df_valid, aes(x = overall_rating, fill = cluster)) +
    geom_histogram(binwidth = 1, color = "white", alpha = 0.6) +
    geom_vline(data = cluster_centers, aes(xintercept = overall_rating), color = "black", linetype = "dashed", size = 1) +
    labs(title = "Overall Rating Clusters", x = "Overall Rating", fill = "Cluster") +
    theme_minimal()
  print(hist_plot)
} else {
  cat("Not enough data points for clustering.\n")
}


# IV. Scatter Plot: Visualize clustering by plotting overall_rating vs. height
scatter_plot <- ggplot(df_valid, aes(x = height, y = overall_rating, color = cluster)) +
  geom_point() +
  theme_minimal() +
  labs(title = "Player Clustering based on Height vs. Overall Rating",
       x = "Height", y = "Overall Rating")
print(scatter_plot)