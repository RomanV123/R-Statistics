#data=read.csv(path of file here)
#The histogram reveals that datasets A, B, and C have 
#well-distributed values with a bell-curve shape and limited spread, suggesting clean, 
#consistent data ideal for statistical modeling. In contrast, 
#dataset D is highly concentrated around a single Y value, with minimal variance and rare but extreme outliers, 
#which significantly distort its distribution. This emphasizes the need for visualization, 
#as such skewed distributions may mislead regression or machine learning models.

library(ggplot2)
   p_hist <- ggplot(long_data, aes(x = Y)) +
    +     geom_histogram(bins = 30, fill = "skyblue", color = "black") +
    +     facet_wrap(~ dataset, scales = "free") +
    +     theme_minimal() +
    +     labs(
      +         title = "Histogram of Y Values Across Datasets",
      +         x = "Y Value",
      +         y = "Count"
      +     )
  
     print(p_hist)
   
     ggsave("C:/Users/mothe/Downloads/dataset_histograms.png", plot = p_hist, width = 8, height = 6)