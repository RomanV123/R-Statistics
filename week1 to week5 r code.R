# week1_to_week5_analysis.R
#Roman Vasilyev
#920502918
#Fushin Hsei
#STA 106
#Midterm Project


# -----------------------------
# Week 1 Project
# -----------------------------
# Load required packages

# Histogram
# Load necessary libraries
library(ggplot2)
library(gridExtra)

# Read your dataset
df <- read.csv("C:/Users/mothe/Downloads/dataset 1.csv")

# Create individual histograms
p1 <- ggplot(df, aes(x = A)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  ggtitle("Histogram of A") +
  theme_minimal()

p2 <- ggplot(df, aes(x = B)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  ggtitle("Histogram of B") +
  theme_minimal()

p3 <- ggplot(df, aes(x = C)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  ggtitle("Histogram of C") +
  theme_minimal()

p4 <- ggplot(df, aes(x = D)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  ggtitle("Histogram of D") +
  theme_minimal()

# Arrange them in a 2x2 grid
grid.arrange(p1, p2, p3, p4, ncol = 2)



#Zoomed in Data sets

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


# -----------------------------
# Week 2 Project
# -----------------------------
# Load required libraries
# Load required libraries
library(pROC)
library(ggplot2)

# 1) Read the dataset 
data <- read.csv("C:/Users/mothe/Downloads/dataset 1.csv", check.names = FALSE)

# 2) Specify which columns to compare
vars <- c("A", "B", "C", "D")

# 3) Prepare a list to collect each ROC data.frame
roc_list <- list()

# 4) Loop over all unique variable‐pairs
for (i in seq_len(length(vars) - 1)) {
  for (j in seq(i + 1, length(vars))) {
    v1 <- vars[i]
    v2 <- vars[j]
    
    # Build response vector: 1 for v1, 0 for v2
    response <- c(rep(1, nrow(data)), rep(0, nrow(data)))
    scores   <- c(data[[v1]],       data[[v2]])
    
    # Compute ROC
    roc_obj <- roc(response, scores)
    
    # Store FPR, TPR, pair name, and AUC
    roc_list[[paste(v1, "vs", v2)]] <- data.frame(
      FPR  = 1 - roc_obj$specificities,
      TPR  = roc_obj$sensitivities,
      pair = paste(v1, "vs", v2),
      AUC  = as.numeric(auc(roc_obj))
    )
  }
}

# 5) Combine all into one data.frame
roc_all <- do.call(rbind, roc_list)

# 6) Plot all ROC curves
ggplot(roc_all, aes(x = FPR, y = TPR, color = pair)) +
  geom_line(size = 1.2) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(
    title = "Combined ROC Curves",
    x     = "False Positive Rate",
    y     = "True Positive Rate",
    color = "Variable Pair"
  ) +
  theme(legend.position = "bottom")

#PCA 

# Load necessary libraries
library(tidyverse)
library(pROC)

# Load and standardize dataset
df <- read.csv("C:/Users/mothe/Downloads/dataset 1.csv")
scaled_data <- scale(df[, c("A", "B", "C", "D")])

# Perform PCA
pca_model <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
pca_df <- as.data.frame(pca_model$x)

# Create binary outcome: 1 if C > median, 0 otherwise
pca_df$Label <- ifelse(df$C > median(df$C), 1, 0)

# Logistic regression using PC1 and PC2
model <- glm(Label ~ PC1 + PC2, data = pca_df, family = binomial)

# Predict probabilities
pca_df$prob <- predict(model, type = "response")

# Generate ROC curve
roc_obj <- roc(pca_df$Label, pca_df$prob)

# Plot ROC curve
plot(roc_obj, col = "blue", lwd = 2,
     main = "ROC Curve Using PC1 and PC2",
     legacy.axes = TRUE)
abline(a = 0, b = 1, lty = 2, col = "gray")
text(0.6, 0.2, paste("AUC =", round(auc(roc_obj), 3)), col = "blue", cex = 1.2)

#PC1 vs PC2

# Load required libraries
library(tidyverse)
library(ggplot2)

# Load your dataset
df <- read.csv("C:/Users/mothe/Downloads/dataset 1.csv")

# Keep numeric variables and scale them
df_scaled <- scale(df[, c("A", "B", "C", "D")])

# Perform PCA
pca_model <- prcomp(df_scaled, center = TRUE, scale. = TRUE)

# Create binary label: C > median
group_c <- ifelse(df$C > median(df$C), "Above Median", "Below Median")

# Create a PCA results dataframe
pca_df <- as.data.frame(pca_model$x)
pca_df$Group <- factor(group_c)

# Plot PC1 vs PC2, colored by binary C group
ggplot(pca_df, aes(x = PC1, y = PC2, color = Group)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "PCA Projection (PC1 vs PC2)",
       subtitle = "Colored by whether C is above or below median",
       x = "Principal Component 1",
       y = "Principal Component 2") +
  theme_minimal() +
  scale_color_manual(values = c("Below Median" = "steelblue", "Above Median" = "darkorange"))


# -----------------------------
# Week 3 Project
# -----------------------------

#Box plot information: 
# 1. Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)

# 2. Read in your data and keep only A–D
df <- read.csv("C:/Users/mothe/Downloads/dataset 1.csv", header = TRUE) %>%
  select(A, B, C, D)

# 3. Pivot to long format
df_long <- df %>%
  pivot_longer(
    cols       = everything(),
    names_to   = "Group",
    values_to  = "Value"
  )

# 4. Run all 6 pairwise Welch t‑tests
pair_list <- combn(unique(df_long$Group), 2, simplify = FALSE)

test_results <- lapply(pair_list, function(p) {
  sub <- df_long %>% filter(Group %in% p)
  tt  <- t.test(Value ~ Group, data = sub)  # Welch’s t‑test by default
  data.frame(
    Group1  = p[1],
    Group2  = p[2],
    t_stat  = round(as.numeric(tt$statistic), 3),
    df      = round(as.numeric(tt$parameter), 1),
    p_value = signif(tt$p.value, 4),
    mean_1  = round(tt$estimate[1], 3),
    mean_2  = round(tt$estimate[2], 3),
    stringsAsFactors = FALSE
  )
})

results_df <- do.call(rbind, test_results)

# 5. View results
print(results_df)

# Load required libraries
library(tidyverse)
library(patchwork)

# Load the dataset
data <- read.csv("C:/Users/mothe/Downloads/dataset 1.csv")

# Reshape the data into long format
data_long <- data %>%
  pivot_longer(cols = c(A, B, C, D), names_to = "Group", values_to = "Value")

# Function to create annotated boxplots with t-test results
make_annotated_plot <- function(group1, group2) {
  df <- data_long %>% filter(Group %in% c(group1, group2))
  test <- t.test(Value ~ Group, data = df)
  t_val <- round(test$statistic, 2)
  p_val <- signif(test$p.value, 3)
  subtitle_text <- paste0("t = ", t_val, ",  p = ", p_val)
  
  ggplot(df, aes(x = Group, y = Value, fill = Group)) +
    geom_boxplot(width = 0.6, outlier.size = 1.5) +
    theme_minimal(base_size = 11) +
    labs(
      title = paste("Boxplot:", group1, "vs", group2),
      subtitle = subtitle_text,
      x = "",
      y = "Value"
    ) +
    theme(
      legend.position = "none",
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10, margin = margin(b = 5))
    )
}

# Generate the six pairwise plots
plot_ab <- make_annotated_plot("A", "B")
plot_ac <- make_annotated_plot("A", "C")
plot_ad <- make_annotated_plot("A", "D")
plot_bc <- make_annotated_plot("B", "C")
plot_bd <- make_annotated_plot("B", "D")
plot_cd <- make_annotated_plot("C", "D")

# Combine plots in a 2x3 layout
combined_plot <- (plot_ab + plot_ac + plot_ad) /
  (plot_bc + plot_bd + plot_cd)

# Add unified theme and margin
combined_plot + plot_layout(guides = "collect") & 
  theme(plot.margin = margin(5, 5, 5, 5))



#Other box plot

library(dplyr)
library(tidyr)
library(ggplot2)

df3 <- read.csv("C:/Users/mothe/Downloads/dataset 1.csv") %>%
  select(A, B, C, D)

df3_long <- df3 %>%
  pivot_longer(cols = everything(),
               names_to  = "Group",
               values_to = "Value")

# Pairwise Welch t-tests
pair_list <- combn(unique(df3_long$Group), 2, simplify = FALSE)
test_results <- lapply(pair_list, function(p) {
  sub <- filter(df3_long, Group %in% p)
  tt  <- t.test(Value ~ Group, data = sub)
  data.frame(
    Group1  = p[1],
    Group2  = p[2],
    t_stat  = round(as.numeric(tt$statistic), 3),
    df      = round(as.numeric(tt$parameter), 1),
    p_value = signif(tt$p.value, 4),
    mean_1  = round(tt$estimate[1], 3),
    mean_2  = round(tt$estimate[2], 3)
  )
})
results_df <- do.call(rbind, test_results)
print(results_df)

# Combined boxplot
p3 <- ggplot(df3_long, aes(x = Group, y = Value, fill = Group)) +
  geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.7) +
  geom_jitter(aes(color = Group),
              width = 0.2, size = 1.5, alpha = 0.7) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Boxplot of Groups A, B, C & D",
    x     = "Group",
    y     = "Value"
  ) +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
print(p3)



# -----------------------------
# Week 4 Project
# -----------------------------
# Load & prepare
data4 <- read.csv("C:/Users/roman/Downloads/dataset.csv")
data4 <- data4[, -1]  # drop index col

# ANOVA function
perform_anova <- function(df, groups) {
  values <- unlist(df[groups])
  group  <- factor(rep(groups, each = nrow(df)))
  aov(values ~ group)
}

cat("ANOVA for (A, B, C):\n")
print(summary(perform_anova(data4, c("A","B","C"))))
cat("\nANOVA for (A, B, D):\n")
print(summary(perform_anova(data4, c("A","B","D"))))
cat("\nANOVA for (A, C, D):\n")
print(summary(perform_anova(data4, c("A","C","D"))))
cat("\nANOVA for (B, C, D):\n")
print(summary(perform_anova(data4, c("B","C","D"))))

# Zoomed-in boxplots (base R)
par(mfrow = c(2,2))
triplets4 <- list(c("A","B","C"), c("A","B","D"), c("A","C","D"), c("B","C","D"))
titles4   <- c("(A, B, C)", "(A, B, D)", "(A, C, D)", "(B, C, D)")
ylims4    <- list(NULL, c(-7,8), c(-3,5), c(-4,5))
for (i in seq_along(triplets4)) {
  vals  <- unlist(data4[triplets4[[i]]])
  grp   <- factor(rep(triplets4[[i]], each = nrow(data4)))
  boxplot(vals ~ grp,
          main = paste("Boxplot of", titles4[i]),
          xlab = "Group", ylab = "Value",
          col  = c("skyblue","lightgreen","salmon"),
          ylim = ylims4[[i]])
}

# Violin plots
library(gridExtra)
gg1 <- function(df, trip, title) {
  df %>% select(all_of(trip)) %>%
    pivot_longer(everything(),
                 names_to  = "Group",
                 values_to = "Value") %>%
    ggplot(aes(x = Group, y = Value, fill = Group)) +
    geom_violin(trim = FALSE, alpha = 0.6) +
    geom_boxplot(width = 0.1, fill = "white", outlier.size = 1) +
    labs(title = title, x = "Group", y = "Value") +
    theme_minimal()
}
v1 <- gg1(data4, c("A","B","C"), "Violin: (A,B,C)")
v2 <- gg1(data4, c("A","B","D"), "Violin: (A,B,D)")
v3 <- gg1(data4, c("A","C","D"), "Violin: (A,C,D)")
v4 <- gg1(data4, c("B","C","D"), "Violin: (B,C,D)")
grid.arrange(v1, v2, v3, v4, ncol = 2)


# -----------------------------
# Week 5 Project
# -----------------------------
library(tidyverse)
library(car)
library(ggpubr)

# Read & reshape
df5 <- read.csv("C:/Users/mothe/Downloads/dataset 1.csv", check.names = FALSE) %>%
  select(A, B, C, D) %>%
  pivot_longer(everything(),
               names_to  = "Group",
               values_to = "Value") %>%
  mutate(Group = factor(Group))

# Define triplets
triplets5 <- list(
  ABC = c("A","B","C"),
  ABD = c("A","B","D"),
  ACD = c("A","C","D"),
  BCD = c("B","C","D")
)

#Levene’s Test + boxplots per triplet
for (nm in names(triplets5)) {
  sub <- filter(df5, Group %in% triplets5[[nm]])
  cat("\n---", nm, "---\n")
  print(leveneTest(Value ~ Group, data = sub))
  p <- ggplot(sub, aes(Group, Value, fill = Group)) +
    geom_boxplot() +
    labs(title = paste("Boxplot of", nm),
         subtitle = "Compare spreads for variance homogeneity")
  print(p)
}

# Shapiro–Wilk + QQ‑plots per triplet (zoomed 1st–99th)
for (nm in names(triplets5)) {
  sub <- filter(df5, Group %in% triplets5[[nm]])
  cat("\n\n===== Normality for", nm, "=====\n")
  print(by(sub$Value, sub$Group, shapiro.test))
  lims <- sub %>% summarize(lo = quantile(Value, .01),
                            hi = quantile(Value, .99))
  pqq <- ggqqplot(sub, x = "Value", facet.by = "Group") +
    coord_cartesian(xlim = c(lims$lo, lims$hi),
                    ylim = c(lims$lo, lims$hi)) +
    labs(title = paste("Zoomed Q–Q:", nm),
         subtitle = "1st–99th percentile")
  print(pqq)
}

# Welch's ANOVA per triplet
for (nm in names(triplets5)) {
  sub <- filter(df5, Group %in% triplets5[[nm]])
  cat("\n===== Welch ANOVA for", nm, "=====\n")
  print(oneway.test(Value ~ Group, data = sub, var.equal = FALSE))
}


#Other Tests

# Required packages
# install.packages(c("tidyverse", "ggpubr"))
library(tidyverse)
library(ggpubr)

# Load and reshape data
df <- read.csv("C:/Users/mothe/Downloads/dataset 1.csv", check.names = FALSE) %>%
  select(A, D, B, C) %>%
  pivot_longer(everything(), names_to = "Group", values_to = "Value") %>%
  mutate(Group = factor(Group))

# Define triplets
triplets <- list(
  ABC = c("A", "B", "C"),
  ABD = c("A", "B", "D"),
  ACD = c("A", "C", "D"),
  BCD = c("B", "C", "D")
)

# For each triplet: run Shapiro-Wilk test and plot QQ plots
for (nm in names(triplets)) {
  gs <- triplets[[nm]]
  sub <- df %>% filter(Group %in% gs)
  
  cat("\n\n===== Normality for", nm, "=====\n")
  print(by(sub$Value, sub$Group, shapiro.test))
  
  # QQ plot of raw values by group
  p <- ggqqplot(sub, x = "Value", facet.by = "Group") +
    labs(title = paste("QQ Plots of", nm))
  print(p)
}


#Levene's tests 

# Load required libraries
library(tidyverse)
library(car)

# Load your dataset
df <- read.csv("C:/Users/mothe/Downloads/dataset 1.csv")

# Reshape to long format for testing and plotting
df_long <- df %>%
  pivot_longer(cols = c(A, B, C, D), names_to = "Group", values_to = "Value")

# Define the triplets
triplets <- list(
  ABC = c("A", "B", "C"),
  ABD = c("A", "B", "D"),
  ACD = c("A", "C", "D"),
  BCD = c("B", "C", "D")
)

# Loop through each triplet
for (nm in names(triplets)) {
  sub <- df_long %>% filter(Group %in% triplets[[nm]])
  
  # Levene’s Test
  cat("\n---", nm, "---\n")
  print(leveneTest(Value ~ Group, data = sub))
  
  # Boxplot
  p <- ggplot(sub, aes(x = Group, y = Value, fill = Group)) +
    geom_boxplot() +
    labs(
      title = paste("Boxplot of", nm),
      subtitle = "Compare box widths to assess variance homogeneity"
    ) +
    theme_minimal()
  
  print(p)
}

#More Tests 

# packages
# install.packages(c("tidyverse","ggpubr","gridExtra"))
library(tidyverse)
library(ggpubr)
library(gridExtra)

# 1) Load & reshape (if you haven’t already)
df <- read.csv("C:/Users/mothe/Downloads/dataset 1.csv", check.names = FALSE) %>%
  select(A, B, C, D) %>%
  pivot_longer(everything(), names_to="Group", values_to="Value") %>%
  mutate(Group = factor(Group))

# 2) Filter to the ABD triplet
sub_abd <- df %>% filter(Group %in% c("A","B","D"))

# 3) Compute 1st–99th percentile limits for A and B
lims_ab <- sub_abd %>%
  filter(Group %in% c("A","B")) %>%
  summarize(lo = quantile(Value, .01), hi = quantile(Value, .99))

# 4a) Zoomed Q–Q for A and B
p_ab <- ggqqplot(sub_abd %>% filter(Group %in% c("A","B")),
                 x = "Value", facet.by = "Group") +
  coord_cartesian(xlim = c(lims_ab$lo, lims_ab$hi),
                  ylim = c(lims_ab$lo, lims_ab$hi)) +
  labs(title = "Zoomed Q–Q (A & B, 1st–99th percentile)")

# 4b) Full‑scale Q–Q for D
p_d <- ggqqplot(sub_abd %>% filter(Group == "D"),
                x = "Value") +
  labs(title = "Full‑scale Q–Q (D)")

# 5) Combine vertically
grid.arrange(p_ab, p_d, ncol = 1)

#More Tests 

# common setup (run once)
# install.packages(c("tidyverse","ggpubr","gridExtra"))
library(tidyverse)
library(ggpubr)
library(gridExtra)

df <- read.csv("C:/Users/mothe/Downloads/dataset 1.csv", check.names = FALSE) %>%
  select(A, B, C, D) %>%
  pivot_longer(everything(), names_to="Group", values_to="Value") %>%
  mutate(Group = factor(Group))


#More Tests

# filter to A, C, D
sub_acd <- df %>% filter(Group %in% c("A","C","D"))

# compute 1st–99th percentile for A & C
lims_ac <- sub_acd %>%
  filter(Group %in% c("A","C")) %>%
  summarize(lo = quantile(Value, .01), hi = quantile(Value, .99))

# zoomed Q–Q for A & C
p_ac <- ggqqplot(sub_acd %>% filter(Group %in% c("A","C")),
                 x = "Value", facet.by = "Group") +
  coord_cartesian(xlim = c(lims_ac$lo, lims_ac$hi),
                  ylim = c(lims_ac$lo, lims_ac$hi)) +
  labs(title = "Zoomed Q–Q (A & C, 1st–99th percentile)")

# full‑scale Q–Q for D
p_d1 <- ggqqplot(sub_acd %>% filter(Group == "D"),
                 x = "Value") +
  labs(title = "Full‑scale Q–Q (D)")

# stack them
grid.arrange(p_ac, p_d1, ncol = 1)

#More Stats

# filter to B, C, D
sub_bcd <- df %>% filter(Group %in% c("B","C","D"))

# compute 1st–99th percentile for B & C
lims_bc <- sub_bcd %>%
  filter(Group %in% c("B","C")) %>%
  summarize(lo = quantile(Value, .01), hi = quantile(Value, .99))

# zoomed Q–Q for B & C
p_bc <- ggqqplot(sub_bcd %>% filter(Group %in% c("B","C")),
                 x = "Value", facet.by = "Group") +
  coord_cartesian(xlim = c(lims_bc$lo, lims_bc$hi),
                  ylim = c(lims_bc$lo, lims_bc$hi)) +
  labs(title = "Zoomed Q–Q (B & C, 1st–99th percentile)")

# full‑scale Q–Q for D
p_d2 <- ggqqplot(sub_bcd %>% filter(Group == "D"),
                 x = "Value") +
  labs(title = "Full‑scale Q–Q (D)")

# stack them
grid.arrange(p_bc, p_d2, ncol = 1)

