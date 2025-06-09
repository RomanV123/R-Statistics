# Install required packages if not already installed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, FSA, rstatix)

# Read the data
data <- read.csv("C:/Users/mothe/Downloads/heart_disease_health_indicators_BRFSS2015.csv")

# Drop rows with missing values in specified variables
data_clean <- data %>%
  drop_na(HeartDiseaseorAttack, Sex, Stroke, HighBP, HighChol, BMI)

# Create group factor variable
data_clean <- data_clean %>%
  mutate(group_f = factor(paste(HeartDiseaseorAttack, Sex, Stroke, HighBP, HighChol, sep = "_")))

# Perform Kruskal-Wallis test
kw_test <- kruskal.test(BMI ~ group_f, data = data_clean)
print(kw_test)

# Perform Dunn's post-hoc test with Holm correction
dunn_test <- dunn_test(BMI ~ group_f, data = data_clean, p.adjust.method = "holm")

# Extract significant comparisons
sig_comparisons <- dunn_test %>%
  filter(p.adj < 0.05) %>%
  mutate(
    group1_idx = as.numeric(factor(group1, levels = levels(data_clean$group_f))),
    group2_idx = as.numeric(factor(group2, levels = levels(data_clean$group_f))),
    x_coord = pmax(group1_idx, group2_idx),
    y_coord = NA,
    significance = case_when(
      p.adj < 0.001 ~ "***",
      p.adj < 0.01 ~ "**",
      TRUE ~ "*"
    )
  )

# Calculate y-coordinates for significance stars
for (i in 1:nrow(sig_comparisons)) {
  group1_data <- data_clean$BMI[data_clean$group_f == sig_comparisons$group1[i]]
  group2_data <- data_clean$BMI[data_clean$group_f == sig_comparisons$group2[i]]
  sig_comparisons$y_coord[i] <- max(c(group1_data, group2_data)) * 1.05
}

# Create boxplot with significance stars
p <- ggplot(data_clean, aes(x = group_f, y = BMI)) +
  geom_boxplot() +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
  ) +
  labs(
    x = "Group",
    y = "BMI",
    title = "BMI Distribution by Group",
    subtitle = paste("Kruskal-Wallis test: χ² =", round(kw_test$statistic, 2),
                    "p =", format.pval(kw_test$p.value, digits = 3))
  )

# Add significance stars
p <- p + geom_text(
  data = sig_comparisons,
  aes(x = x_coord, y = y_coord, label = significance),
  size = 4
)

# Save the plot
ggsave("kruskal_dunn_boxplot.png", p, width = 12, height = 8, dpi = 300) 