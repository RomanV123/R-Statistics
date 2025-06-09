# 1. Load libraries
> library(dplyr)
> library(tidyr)
> library(ggplot2)
> 
  > # 2. Read in your data and keep only A–D
  > df <- read.csv("C:\\Users\\mothe\\Downloads\\dataset 1.csv", header = TRUE) %>%
    +     select(A, B, C, D)
  > 
    > # 3. Pivot to long format
    > df_long <- df %>%
      +     pivot_longer(
        +         cols      = everything(),
        +         names_to  = "Group",
        +         values_to = "Value"
        +     )
    > 
      > # 4. Run all 6 pairwise Welch t‑tests
      > pair_list <- combn(unique(df_long$Group), 2, simplify = FALSE)
      > 
        > test_results <- lapply(pair_list, function(p) {
          +     sub <- df_long %>% filter(Group %in% p)
          +     tt  <- t.test(Value ~ Group, data = sub)  # Welch’s t‑test by default
          +     data.frame(
            +         Group1    = p[1],
            +         Group2    = p[2],
            +         t_stat    = round(as.numeric(tt$statistic), 3),
            +         df        = round(as.numeric(tt$parameter), 1),
            +         p_value   = signif(tt$p.value, 4),
            +         mean_1    = round(tt$estimate[1], 3),
            +         mean_2    = round(tt$estimate[2], 3)
            +     )
          + })
        > 
          > results_df <- do.call(rbind, test_results)
          > print(results_df)
          Group1 Group2  t_stat     df   p_value mean_1 mean_2
          mean in group A       A      B  -0.967 1454.4 3.336e-01  0.016  0.085
          mean in group A1      A      C -21.877 1997.6 2.745e-95  0.016  0.980
          mean in group A2      A      D  -1.522 1000.6 1.283e-01  0.016  1.721
          mean in group B       B      C -12.613 1443.5 1.127e-34  0.085  0.980
          mean in group B1      B      D  -1.459 1005.5 1.449e-01  0.085  1.721
          mean in group C       C      D  -0.662 1000.5 5.083e-01  0.980  1.721
          > 
            > 
            > # 5. One combined boxplot of A, B, C & D
            > ggplot(df_long, aes(x = Group, y = Value, fill = Group)) +
            +     geom_boxplot(outlier.shape = NA, alpha = 0.6, width = 0.7) +
            +     geom_jitter(aes(color = Group),
                              +                 width = 0.2, size = 1.5, alpha = 0.7) +
            +     scale_fill_brewer(palette = "Set2") +
            +     scale_color_brewer(palette = "Set2") +
            +     labs(
              +         title = "Boxplot of Groups A, B, C & D",
              +         x     = "Group",
              +         y     = "Value"
              +     ) +
            +     theme_minimal() +
            +     theme(legend.position = "none",
                        +           plot.title     = element_text(size = 14, face = "bold", hjust = 0.5))
          > 