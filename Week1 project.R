# Load required packages
library(tidyr)
library(ggplot2)

# Read the CSV file (remove the first column that was just an index)
data <- read.csv("C:/Users/mothe/Downloads/dataset 1.csv")[, -1]

# Add a row number column to act as X
data$X <- 1:nrow(data)

# Reorder columns so X is first
data <- data[, c("X", "A", "B", "C", "D")]

# Reshape from wide to long format
long_data <- pivot_longer(
  data,
  cols = A:D,
  names_to = "dataset",
  values_to = "Y"
)

# Plot: Zoomed-in view with free y-axis for better clarity
p <- ggplot(long_data, aes(x = X, y = Y)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  facet_wrap(~ dataset, scales = "free_y") +  # This line allows each panel to have its own Y scale
  theme_minimal() +
  labs(
    title = "Zoomed-In View: Four Random Datasets",
    x = "Observation (Row Index)",
    y = "Y Value"
  )

# Display the plot
print(p)

# Optional: Save the plot as an image to your computer
ggsave("C:/Users/mothe/Downloads/zoomed_in_plot.png", plot = p, width = 8, height = 6)

