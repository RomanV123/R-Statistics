
library(dplyr)
library(ggplot2)

# Option A: read first, then transform
file_path <- "C:\\Users\\mothe\\Downloads\\heart_disease_health_indicators_BRFSS2015.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

df2 <- df %>%
  mutate(group = case_when(
    Heart_disease == 0 & Stroke == 0 ~ "Neither",
    Heart_disease == 1 & Stroke == 0 ~ "HD only",
    Heart_disease == 0 & Stroke == 1 ~ "Stroke only",
    Heart_disease == 1 & Stroke == 1 ~ "Both"
  ))

# Now you can plot, summarize, etc.
boxplot(BMI ~ group, data = df2, 
        main = "BMI by Disease Status", 
        ylab = "BMI", xlab = "Group")


library(dplyr)
library(ggplot2)

df2 <- df %>%
  mutate(group = case_when(
    HeartDiseaseorAttack == 0 & Stroke == 0 ~ "Neither",
    HeartDiseaseorAttack == 1 & Stroke == 0 ~ "HD only",
    HeartDiseaseorAttack == 0 & Stroke == 1 ~ "Stroke only",
    HeartDiseaseorAttack == 1 & Stroke == 1 ~ "Both"
  ))

ggplot(df2, aes(x = BMI)) +
  geom_density() +
  facet_wrap(~ group, ncol = 2, scales = "free_y") +
  labs(
    title = "BMI Density by Disease-Status Group",
    x     = "BMI",
    y     = "Density"
  ) +
  theme_minimal()


# Load required packages
library(dplyr)
library(ggplot2)

# Read the data (adjust path if needed)
file_path <- "C:/Users/mothe/Downloads/heart_disease_health_indicators_BRFSS2015.csv"
df <- read.csv(file_path, stringsAsFactors = FALSE)

# Create the 4‐level grouping variable
df2 <- df %>%
  mutate(group = case_when(
    HeartDiseaseorAttack == 0 & Stroke               == 0 ~ "Neither",
    HeartDiseaseorAttack == 1 & Stroke               == 0 ~ "HD only",
    HeartDiseaseorAttack == 0 & Stroke               == 1 ~ "Stroke only",
    HeartDiseaseorAttack == 1 & Stroke               == 1 ~ "Both",
    TRUE                                                ~ NA_character_
  ))

# —— 1) Base R: 2×2 histogram panel ——
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))
for (g in unique(df2$group)) {
  hist(
    df2$BMI[df2$group == g],
    main   = g,
    xlab   = "BMI",
    ylab   = "Count",
    breaks = 20
  )
}
par(mfrow = c(1, 1))  # reset to single panel

# —— 2) ggplot2: overlaid histogram with color encoding ——
ggplot(df2, aes(x = BMI, fill = group)) +
  geom_histogram(
    position = "identity",
    alpha    = 0.5,
    bins     = 30
  ) +
  labs(
    title = "BMI Histogram by Disease-Status Group",
    x     = "BMI",
    y     = "Count",
    fill  = "Group"
  ) +
  theme_minimal()

 # Contingency Table

# 1) Create a 2×2 contingency table and save it as “ct”
ct <- table(
  HeartDisease = df$HeartDiseaseorAttack,
  Stroke       = df$Stroke
)

# 2) View the counts
ct

# Row percentages (multiply by 100 to get %)
prop.table(ct, margin = 1) * 100

#HC Table
library(dplyr)

# 1) Compute summary stats for each group
stat_df <- df2 %>%
  group_by(group) %>%
  summarize(
    mean_BMI   = mean(BMI, na.rm = TRUE),
    median_BMI = median(BMI, na.rm = TRUE),
    sd_BMI     = sd(BMI, na.rm = TRUE)
  )

# 2) Turn that into a matrix (rows = groups, cols = stats)
stat_mat <- stat_df %>%
  column_to_rownames("group") %>%
  as.matrix()

# 3) Compute pairwise distances (Euclidean by default)
d <- dist(stat_mat)

# 4) Do the hierarchical clustering
hc <- hclust(d)

# 5) Plot
plot(
  hc,
  main = "HC‐Tree of BMI Distributions by Group",
  xlab = "",
  sub  = "",
  ylab = "Height (distance)"
)

#HeatMap

# 1) Make sure df2 exists (with your 4 groups defined)
#    (If you haven’t done it yet, recreate df2 here)
df2 <- df %>%
  mutate(group = case_when(
    HeartDiseaseorAttack == 0 & Stroke == 0 ~ "Neither",
    HeartDiseaseorAttack == 1 & Stroke == 0 ~ "HD only",
    HeartDiseaseorAttack == 0 & Stroke == 1 ~ "Stroke only",
    HeartDiseaseorAttack == 1 & Stroke == 1 ~ "Both",
    TRUE ~ NA_character_
  ))

# 2) Build the count matrix
counts    <- table(df2$group)         # a named vector of length 4
counts_mat <- matrix(counts, ncol = 1) # turn it into a 4×1 matrix
rownames(counts_mat) <- names(counts) # assign the group names as rownames
colnames(counts_mat) <- "N"           # name the single column “N”

# 3) (Re)compute hc if needed
stat_df <- df2 %>%
  group_by(group) %>%
  summarize(
    mean_BMI   = mean(BMI,   na.rm=TRUE),
    median_BMI = median(BMI, na.rm=TRUE),
    sd_BMI     = sd(BMI,     na.rm=TRUE)
  ) %>%
  as.data.frame()
rownames(stat_df) <- stat_df$group
stat_df$group     <- NULL
hc <- hclust(dist(as.matrix(stat_df)))

# 4) Now you can call pheatmap
library(pheatmap)
pheatmap(
  counts_mat,
  cluster_rows     = hc,
  cluster_cols     = FALSE,
  display_numbers  = TRUE,
  color            = colorRampPalette(c("white","blue"))(100),
  main             = "Counts Heatmap with HC-Tree"
)

#Week 7 and 8 

# Load necessary libraries
library(ggplot2)
library(dplyr)

# Convert the binary variables to factors
df <- df %>%
  mutate(across(c(HeartDiseaseorAttack, Sex, Stroke, HighBP, HighChol), as.factor))

# Create a new sample ID by pasting the binary values
df <- df %>%
  mutate(SampleID = paste(HeartDiseaseorAttack, Sex, Stroke, HighBP, HighChol, sep = "_"))

# Check how many unique combinations (should be 32)
length(unique(df$SampleID))  # Should be 32

# Plot BMI distributions using boxplot
ggplot(df, aes(x = SampleID, y = BMI)) +
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "BMI Distribution Across 32 Groups (5 Binary Variables)",
    x = "Sample ID (Binary Combination)",
    y = "BMI"
  )

# HC Tree

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Ensure SampleID exists from earlier step
df <- df %>%
  mutate(SampleID = paste(HeartDiseaseorAttack, Sex, Stroke, HighBP, HighChol, sep = "_"))

# Compute average BMI for each of the 32 groups
group_means <- df %>%
  group_by(SampleID) %>%
  summarise(mean_BMI = mean(BMI), .groups = 'drop')

# Turn SampleID into rownames
row.names(group_means) <- group_means$SampleID
group_means$SampleID <- NULL

# Compute distance matrix
dist_matrix <- dist(group_means)

# Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot dendrogram
plot(hc, main = "HC-tree of 32 Groups Based on Mean BMI", xlab = "SampleID", sub = "")


#Heat Map

# Load necessary libraries
library(dplyr)
library(pheatmap)
library(RColorBrewer)

# Set the path to your CSV file
file_path <- "C:/Users/mothe/Downloads/heart_disease_health_indicators_BRFSS2015.csv"

# Load the dataset and select relevant binary columns
features <- c("HeartDiseaseorAttack", "Sex", "Stroke", "HighBP", "HighChol")
df <- read.csv(file_path)[features]

# Select the first 32 rows
data <- df[1:32, ]

# Set row names as Sample_1, Sample_2, ..., Sample_32
rownames(data) <- paste0("Sample_", seq_len(nrow(data)))

# Ensure values are numeric
data_matrix <- data %>% 
  mutate(across(everything(), as.numeric)) %>% 
  as.matrix()

# Define a smooth diverging color palette (green → yellow → red)
diverging_palette <- colorRampPalette(c("#006837", "#78c679", "#ffffbf", "#fd8d3c", "#bd0026"))(100)

# Plot the heatmap with hierarchical clustering
pheatmap(
  mat = data_matrix,
  color = diverging_palette,
  clustering_distance_rows = "euclidean",
  clustering_distance_cols = "euclidean",
  clustering_method = "ward.D2",
  cluster_rows = TRUE,
  cluster_cols = TRUE,
  main = "Heatmap with Hierarchical Clustering of 32 Samples",
  fontsize_row = 8,
  fontsize_col = 10,
  legend = TRUE,
  border_color = NA
)


#R code for Tukey 

# -------------------------------
# Continuing from the Week 8 script
# (assumes you've already created `df` and `group_stats`
#  exactly as in the previous answer)
# -------------------------------

# 1) Convert group_id to a factor (so that aov() treats it as a categorical predictor)
df$group_f <- factor(df$group_id)

# 2) Fit one‐way ANOVA: BMI ~ group_f
anova_model <- aov(BMI ~ group_f, data = df)

# 3) Display the ANOVA table
summary(anova_model)

# 4) Tukey–Kramer post‐hoc comparisons (since group sizes differ, TukeyHSD is appropriate)
tukey_out <- TukeyHSD(anova_model, "group_f", conf.level = 0.95)

# 5) Print a concise version of TukeyHSD results:
#    (This shows all pairwise group‐difference estimates, confidence intervals, and adjusted p‐values.)
print(tukey_out)

# 6) (Optional) If you only want to extract the significant pairs where p.adj < 0.05:
tukey_df <- as.data.frame(tukey_out$group_f)
tukey_df$pair <- rownames(tukey_df)
sig_pairs <- subset(tukey_df, `p.adj` < 0.05)
rownames(sig_pairs) <- NULL
print("Significant Tukey‐Kramer pairs (p.adj < 0.05):")
print(sig_pairs)

# 7) (Optional) Visual check: plot the Tukey confidence intervals for all pairwise group comparisons:
plot(tukey_out, las = 1, cex.axis = 0.6)
#   – las=1 makes y‐labels horizontal; cex.axis=0.6 shrinks the axis font so all 496 lines can be seen 



#Heatmap

# -------------------------------
# Heatmap of (mean_BMI, var_BMI) for 32 groups with HC‐tree
# -------------------------------

# 1) Load required packages
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
if (!requireNamespace("tidyr", quietly = TRUE)) {
  install.packages("tidyr")
}
if (!requireNamespace("pheatmap", quietly = TRUE)) {
  install.packages("pheatmap")
}
library(dplyr)
library(tidyr)
library(pheatmap)

# 2) Read the CSV (only the 6 columns we need: 5 binary + BMI)
file_path <- "C:\\Users\\mothe\\Downloads\\heart_disease_health_indicators_BRFSS2015.csv"
df_raw <- read.csv(file_path, stringsAsFactors = FALSE)[
  , c("HeartDiseaseorAttack", "Sex", "Stroke", "HighBP", "HighChol", "BMI")
]

# 3) Drop rows with any NA in those six columns
df <- df_raw %>%
  drop_na(HeartDiseaseorAttack, Sex, Stroke, HighBP, HighChol, BMI)

# 4) Ensure the five “flag” columns are integers (0/1), BMI is numeric
df <- df %>%
  mutate(
    HeartDiseaseorAttack = as.integer(HeartDiseaseorAttack),
    Sex                  = as.integer(Sex),
    Stroke               = as.integer(Stroke),
    HighBP               = as.integer(HighBP),
    HighChol             = as.integer(HighChol),
    BMI                  = as.numeric(BMI)
  )

# 5) Create the 5‐bit group_id = HD*16 + Sex*8 + Stroke*4 + HighBP*2 + HighChol*1
df <- df %>%
  mutate(
    group_id = HeartDiseaseorAttack * 16 +
      Sex                  *  8 +
      Stroke               *  4 +
      HighBP               *  2 +
      HighChol             *  1
  )

# 6) Compute, for each group_id (0–31), the count, mean_BMI, and var_BMI
group_stats <- df %>%
  group_by(group_id) %>%
  summarise(
    count    = n(),
    mean_BMI = mean(BMI),
    var_BMI  = var(BMI)
  ) %>%
  arrange(group_id)

# 7) Build a data matrix with rows = group_id and columns = mean_BMI, var_BMI
#    We will use this matrix for both the heatmap and clustering.
mat <- group_stats %>%
  select(mean_BMI, var_BMI) %>%
  as.data.frame()
rownames(mat) <- as.character(group_stats$group_id)
mat <- as.matrix(mat)

# 8) Compute a row‐wise hierarchical clustering using Euclidean distance & Ward’s method
dist_rows <- dist(mat, method = "euclidean")
hc_rows   <- hclust(dist_rows, method = "ward.D2")

# 9) Draw the heatmap with the row dendrogram
#    – cluster_rows = TRUE tells pheatmap to use our hc_rows
#    – cluster_cols = FALSE because we do not want to cluster the two columns
#    – show_rownames = TRUE to display “0 … 31”
#    – color palette: let pheatmap choose a default gradient
pheatmap(
  mat,
  cluster_rows = hc_rows,
  cluster_cols = FALSE,
  show_rownames = TRUE,
  fontsize_row = 8,
  fontsize_col = 10,
  main = "Heatmap of mean_BMI & var_BMI (32 groups) with HC‐tree"
)

# ------------------------------------------------------------
# R Script for Week 7 Analysis: BMI by 32 Health-Indicator Groups
# ------------------------------------------------------------

# 1) Install and load required packages (if not already installed)
if (!require("car")) {
  install.packages("car")
}
library(car)

# 2) Read the CSV file (adjust the path as needed)
data_path <- "C:/Users/roman/Downloads/heart_disease_health_indicators_BRFSS2015 (1).csv"
df <- read.csv(data_path, stringsAsFactors = FALSE)

# 3) Drop rows with missing values in any of the five binary indicators or BMI
vars <- c("HeartDiseaseorAttack", "Sex", "Stroke", "HighBP", "HighChol", "BMI")
df <- df[complete.cases(df[, vars]), ]

# 4) Create a single factor "group" with up to 32 levels by concatenating the five binaries
#    (Each variable is assumed to be coded 0/1 already.)
df$group <- with(df, paste(
  HeartDiseaseorAttack,
  Sex,
  Stroke,
  HighBP,
  HighChol,
  sep = "_"
))
df$group <- factor(df$group)
# At this point, df$group has one level for each unique 5-bit combination (up to 32 levels).

# 5) Levene’s Test for Homogeneity of Variance
#    H0: all group variances are equal. If Pr(>F) < 0.05, reject H0.
levene_result <- car::leveneTest(BMI ~ group, data = df)
print(levene_result)

# 6) Fit a one-way ANOVA model
aov_model <- aov(BMI ~ group, data = df)

#    (Optional) View ANOVA summary:
# summary(aov_model)

# 7) Extract residuals from the ANOVA fit
residuals_all <- residuals(aov_model)

# 8) Since Shapiro–Wilk in R is valid only up to n = 5000, take a random sample of 5000 residuals
set.seed(123)
if (length(residuals_all) > 5000) {
  res_sample <- sample(residuals_all, 5000)
} else {
  res_sample <- residuals_all
}

# 9) Shapiro–Wilk test on the sampled residuals
shapiro_result <- shapiro.test(res_sample)
print(shapiro_result)


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

# 1) Display the plot in your R session
print(p)

# 2) Show the working directory where ggsave wrote the file
cat("ggsave wrote the file to:\n", getwd(), "\n")

# 3) If you want to save to Downloads instead:
ggsave("C:/Users/mothe/Downloads/kruskal_dunn_boxplot.png", p,
       width = 12, height = 8, dpi = 300)
cat("Also saved copy to Downloads.\n")

# 4) Check how many significant Dunn comparisons you have
cat("Number of significant Dunn comparisons (p.adj < 0.05): ",
    nrow(sig_comparisons), "\n")


# 1. Load libraries (install if missing)
if (!require(tidyverse))   install.packages("tidyverse", repos="https://cloud.r-project.org")
if (!require(multcomp))    install.packages("multcomp", repos="https://cloud.r-project.org")
library(tidyverse)  # brings in readr, dplyr, etc.
library(multcomp)   # for alternative Tukey summary

# 2. Read the data
file_path <- "C:/Users/mothe/Downloads/heart_disease_health_indicators_BRFSS2015.csv"
df <- read_csv(file_path)

# 3. Take only the first 32 rows
df32 <- df[1:32, ]

# 4. Ensure the five indicators are 0/1 integers
df32$Sex                  <- as.integer(df32$Sex)
df32$HighBP               <- as.integer(df32$HighBP)
df32$HighChol             <- as.integer(df32$HighChol)
df32$HeartDiseaseorAttack <- as.integer(df32$HeartDiseaseorAttack)
df32$Stroke               <- as.integer(df32$Stroke)

# 5. Build the 5-bit code and make it a factor
df32$code <- factor(
  paste0(
    df32$Sex,
    df32$HighBP,
    df32$HighChol,
    df32$HeartDiseaseorAttack,
    df32$Stroke
  )
)

# 6. Subset to just code + BMI using base R
df2 <- df32[, c("code", "BMI")]

# 7. Run one‐way ANOVA
anova_mod <- aov(BMI ~ code, data = df2)
cat("\n--- ANOVA Table ---\n")
print(summary(anova_mod))

# 8. Run Tukey–Kramer post‐hoc
tukey_out <- TukeyHSD(anova_mod, "code", conf.level = 0.95)
cat("\n--- TukeyHSD Results ---\n")
print(tukey_out)

# (Optional) Alternative summary via multcomp
# tuk_glht <- glht(anova_mod, linfct = mcp(code = "Tukey"))
# summary(tuk_glht)

# 1. Libraries (install if needed)
if (!require(tidyverse)) install.packages("tidyverse", repos="https://cloud.r-project.org")
library(tidyverse)

# 2. Load the data
file_path <- "C:/Users/mothe/Downloads/heart_disease_health_indicators_BRFSS2015.csv"
df <- read_csv(file_path)

# 3. Create the 5-bit code and keep BMI
df2 <- df %>%
  mutate(across(c(Sex, HighBP, HighChol, HeartDiseaseorAttack, Stroke),
                ~ as.integer(.))) %>%      # ensure 0/1
  mutate(code = paste0(Sex, HighBP, HighChol, HeartDiseaseorAttack, Stroke)) %>%
  select(code, BMI)

# 4. Summarize BMI by group
group_stats <- df2 %>%
  group_by(code) %>%
  summarize(
    n         = n(),
    meanBMI   = mean(BMI,   na.rm=TRUE),
    medianBMI = median(BMI, na.rm=TRUE),
    sdBMI     = sd(BMI,     na.rm=TRUE),
    IQR_BMI   = IQR(BMI,    na.rm=TRUE)
  ) %>%
  arrange(desc(meanBMI))

print(group_stats)

# 5. Visualize with a boxplot (ordered by mean BMI)
group_stats <- group_stats %>%
  mutate(code = fct_reorder(code, meanBMI))

ggplot(df2, aes(x=factor(code, levels=levels(group_stats$code)), y=BMI)) +
  geom_boxplot(outlier.size=0.8) +
  coord_flip() +
  labs(
    x = "5-bit group code (Sex, BP, Chol, HD, Stroke)",
    y = "BMI",
    title = "BMI distributions across the 32 binary-defined groups"
  ) +
  theme_minimal(base_size=12)

# 6. Cluster the 32 groups on their summary stats
#    We'll use mean, median, sd, and IQR as features, all scaled
stat_mat <- group_stats %>%
  select(meanBMI, medianBMI, sdBMI, IQR_BMI) %>%
  scale()

diss <- dist(stat_mat)                   # Euclidean distance on scaled stats
hc   <- hclust(diss, method="ward.D2")   # Ward clustering

# 7. Plot the dendrogram
plot(hc,
     labels = group_stats$code,
     main   = "HC-tree of the 32 BMI Distributions",
     xlab   = "Binary group code",
     ylab   = "Distance (on BMI-stat features)",
     cex    = 0.8)


# 1. Load libraries (install if needed)
if (!require(tidyverse)) install.packages("tidyverse", repos="https://cloud.r-project.org")
library(tidyverse)

# 2. Read your 32-sample slice
file_path <- "C:/Users/mothe/Downloads/heart_disease_health_indicators_BRFSS2015.csv"
df32 <- read_csv(file_path) %>%
  slice(1:32) %>%
  # build the 5-bit code
  mutate(across(c(Sex, HighBP, HighChol, HeartDiseaseorAttack, Stroke),
                ~ as.integer(.))) %>%
  mutate(code = paste0(Sex, HighBP, HighChol, HeartDiseaseorAttack, Stroke))

# 3. Define your three HC clusters by code
low_codes  <- c("10010","10000","10100")
mid_codes  <- c("00000","00001","00100")
high_codes <- c("01110","01111","11110")

df32 <- df32 %>%
  mutate(cluster3 = case_when(
    code %in% low_codes  ~ "Low-BMI",
    code %in% mid_codes  ~ "Mid-BMI",
    code %in% high_codes ~ "High-BMI",
    TRUE                 ~ "Other"      # if any code fell outside those three
  )) %>%
  # Drop any “Other” (shouldn’t happen if your 32 codes all belonged to one cluster)
  filter(cluster3 != "Other") %>%
  mutate(cluster3 = factor(cluster3, levels=c("Low-BMI","Mid-BMI","High-BMI")))

# 4. Run one-way ANOVA on the three clusters
anova_3 <- aov(BMI ~ cluster3, data = df32)
cat("\n=== ANOVA on 3 Aggregated Clusters ===\n")
print(summary(anova_3))

# 5. Tukey–Kramer post-hoc
tukey_3 <- TukeyHSD(anova_3, "cluster3", conf.level = 0.95)
cat("\n=== Tukey–Kramer on 3 Clusters ===\n")
print(tukey_3)


# 1. Install/load packages
if (!require(tidyverse))   install.packages("tidyverse", repos="https://cloud.r-project.org")
if (!require(cluster))     install.packages("cluster",   repos="https://cloud.r-project.org")
if (!require(pheatmap))    install.packages("pheatmap",  repos="https://cloud.r-project.org")

library(tidyverse)
library(cluster)
library(pheatmap)

# 2. Read & prep first 32 samples
file_path <- "C:/Users/mothe/Downloads/heart_disease_health_indicators_BRFSS2015.csv"
df32 <- read_csv(file_path)

# Keep only the first 32 rows
df32 <- df32[1:32, ]

# Ensure your five indicators are integers
df32$Sex                  <- as.integer(df32$Sex)
df32$HighBP               <- as.integer(df32$HighBP)
df32$HighChol             <- as.integer(df32$HighChol)
df32$HeartDiseaseorAttack <- as.integer(df32$HeartDiseaseorAttack)
df32$Stroke               <- as.integer(df32$Stroke)

# Build the 5‐bit code labels
df32$code <- paste0(
  df32$Sex,
  df32$HighBP,
  df32$HighChol,
  df32$HeartDiseaseorAttack,
  df32$Stroke
)

# 3. Create a pure binary matrix (rows = samples, cols = indicators)
bin_mat <- as.matrix(df32[, c("Sex", "HighBP", "HighChol", "HeartDiseaseorAttack", "Stroke")])

# Assign the 5‐bit codes as row names
rownames(bin_mat) <- df32$code

# 4. Compute Gower distance and hierarchical clustering on rows
diss    <- daisy(bin_mat, metric = "gower")
hc_rows <- hclust(diss, method = "ward.D2")

# 5. Define a 2‐color palette for binary values
pal_bin <- c("white", "steelblue")

# 6. Plot heatmap with your HC‐tree
pheatmap(
  mat           = bin_mat,
  color         = pal_bin,
  cluster_rows  = hc_rows,    # use our precomputed tree
  cluster_cols  = FALSE,      # keep the five indicators in order
  show_rownames = TRUE,
  show_colnames = TRUE,
  legend        = FALSE,
  main          = "Heatmap of 5 Binary Indicators\nwith Superimposed HC-Tree",
  fontsize_row  = 8,
  fontsize_col  = 10,
  border_color  = "grey80"
)

# 0. Install/load packages
if (!require(tidyverse)) install.packages("tidyverse", repos="https://cloud.r-project.org")
if (!require(cluster))   install.packages("cluster",   repos="https://cloud.r-project.org")
if (!require(FSA))       install.packages("FSA",       repos="https://cloud.r-project.org")

library(tidyverse)
library(cluster)
library(FSA)

# 1. Read & prep the 32-sample slice
file_path <- "C:/Users/mothe/Downloads/heart_disease_health_indicators_BRFSS2015.csv"
df32 <- read_csv(file_path) %>%
  slice(1:32) %>%
  mutate(across(c(Sex, HighBP, HighChol, HeartDiseaseorAttack, Stroke), as.integer),
         code = paste0(Sex, HighBP, HighChol, HeartDiseaseorAttack, Stroke))

# 2. Compute BMI summary stats per code
stat_summary <- df32 %>%
  group_by(code) %>%
  summarize(
    meanBMI   = mean(BMI, na.rm=TRUE),
    medianBMI = median(BMI, na.rm=TRUE),
    sdBMI     = sd(BMI, na.rm=TRUE),
    IQR_BMI   = IQR(BMI, na.rm=TRUE)
  ) %>%
  ungroup()

# 3. Build & cut the HC‐tree into 3 clusters
four_stat_mat <- scale(stat_summary[, c("meanBMI","medianBMI","sdBMI","IQR_BMI")])
hc            <- hclust(dist(four_stat_mat), method="ward.D2")
stat_summary$cluster3 <- factor(cutree(hc, k=3),
                                labels = c("Low-BMI","Mid-BMI","High-BMI"))

# 4. Merge cluster labels back onto the 32-sample data frame
df32 <- merge(
  df32,
  stat_summary[, c("code", "cluster3")],
  by = "code",
  all.x = TRUE
)

# 5. Sanity check: make sure all 32 rows have a label
print(table(df32$cluster3, useNA="ifany"))

# 6. Kruskal–Wallis test on BMI across the three clusters
kw <- kruskal.test(BMI ~ cluster3, data = df32)
print(kw)

# 7. Dunn’s post‐hoc with Bonferroni adjustment
dunn <- dunnTest(BMI ~ cluster3, data = df32, method = "bonferroni")
print(dunn)


# 1. Load libraries
library(ggplot2)
library(ggpubr)
library(rstatix)
library(dplyr)

# 2. Compute Dunn’s test
dunn_res <- df32 %>%
  dunn_test(BMI ~ cluster3, p.adjust.method = "bonferroni")

# 3. Manually assign y.positions above the tallest box
max_bmi <- max(df32$BMI, na.rm=TRUE)
step    <- (max_bmi - min(df32$BMI, na.rm=TRUE)) * 0.05

dunn_res <- dunn_res %>%
  arrange(p.adj) %>%                   # smallest p at top
  mutate(y.position = max_bmi + step * row_number())

# 4. Plot boxplots + Kruskal–Wallis + Dunn annotations
ggboxplot(df32, x = "cluster3", y = "BMI",
          color = "cluster3", palette = "jco",
          ylab = "BMI", xlab = "", 
          title = "BMI by HC‐Derived Cluster") +
  stat_compare_means(method = "kruskal.test",
                     label.y = max_bmi + step * (nrow(dunn_res) + 1)) +
  stat_pvalue_manual(dunn_res, label = "p.adj", tip.length = 0.01) +
  theme_minimal(base_size = 14)



