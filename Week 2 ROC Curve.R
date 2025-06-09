# Load required libraries
library(pROC)
library(ggplot2)
library(dplyr)

# Load dataset
df <- read.csv("C:/Users/mothe/Downloads/dataset 1.csv")
df <- df[, -1]  # Remove index column

# Standardize data
scaled_df <- scale(df)

# Apply PCA
pca_result <- prcomp(scaled_df)
pca_data <- as.data.frame(pca_result$x)

# K-means clustering with 4 groups
set.seed(123)
k_result <- kmeans(pca_data[, 1:3], centers = 4)
pca_data$Cluster <- as.factor(k_result$cluster)

# Function to generate ROC data for two clusters using PC1
get_roc_data <- function(df, cluster1, cluster2, predictor_col = "PC1") {
  subset_data <- df %>%
    filter(Cluster %in% c(cluster1, cluster2)) %>%
    mutate(Class = ifelse(Cluster == cluster2, 1, 0))
  
  roc_obj <- roc(subset_data$Class, subset_data[[predictor_col]])
  
  data.frame(
    FPR = 1 - roc_obj$specificities,
    TPR = roc_obj$sensitivities,
    Comparison = paste("Cluster", cluster1, "vs", "Cluster", cluster2),
    AUC = as.numeric(auc(roc_obj))
  )
}

# Define all 6 pairwise combinations
pairs <- list(
  c("1", "2"),
  c("1", "3"),
  c("1", "4"),
  c("2", "3"),
  c("2", "4"),
  c("3", "4")
)

# Generate ROC curves for all pairs
roc_all <- do.call(rbind, lapply(pairs, function(pair) {
  get_roc_data(pca_data, pair[1], pair[2])
}))

# Plot
ggplot(roc_all, aes(x = FPR, y = TPR, color = Comparison)) +
  geom_line(size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
  labs(
    title = "ROC Curves for All Cluster Pairs (Using PC1 as Predictor)",
    x = "False Positive Rate (1 - Specificity)",
    y = "True Positive Rate (Sensitivity)"
  ) +
  theme_minimal()
