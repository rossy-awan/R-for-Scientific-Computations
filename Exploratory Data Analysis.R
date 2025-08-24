# Load Libraries & Data
library(dplyr)
library(tidyr)
library(moments)
library(corrplot)

# Built-in dataset
df <- iris

# Quick look
print(head(df))

# Data Wrangling & Cleaning
cat("\nCheck missing values\n")
print(colSums(is.na(df)))
df <- df %>% rename_with(tolower) # Rename columns to lowercase
cat("\nCreate a new feature\n")
df <- df %>% mutate(sepal.ratio = sepal.length / sepal.width,
                    petal.ratio = petal.length / petal.width)
print(head(df))

# Summary
cat("\nDescriptive Statistics\n")
stats_table <- df %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), list(
    min = min,
    median = median,
    mean = mean,
    max = max,
    sd = sd,
    iqr = IQR,
    skewness = skewness,
    kurtosis = kurtosis
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("Feature", "Statistic"),
               names_sep = "_",
               values_to = "Value") %>%
  pivot_wider(names_from = Statistic, values_from = Value)
print(stats_table)

# Outlier Detection
cat("\nOutlier Detection\n")
for (col in numeric_cols) {
  Q1 <- quantile(df[[col]], 0.25)
  Q3 <- quantile(df[[col]], 0.75)
  IQR_val <- Q3 - Q1
  outliers <- df[[col]][df[[col]] < (Q1 - 1.5 * IQR_val) | df[[col]] > (Q3 + 1.5 * IQR_val)]
  cat(paste("Feature:", col, "- Outliers Detected:", length(outliers), "\n"))
}

# Histogram
numeric_cols <- names(df)[sapply(df, is.numeric)]
for (col in numeric_cols) {
  hist(df[[col]],
       main = paste("Histogram of", col),
       xlab = col,
       col = "lightblue",
       breaks = 25)
}

# Boxplot
for (col in numeric_cols) {
  boxplot(df[[col]] ~ df$species,
          main = paste("Boxplot of", col, "by species"),
          ylab = col,
          xlab = "Species",
          col = c("pink", "lightgreen", "lightblue"))
}

# Correlation Matrix
corr_matrix <- cor(df %>% select(where(is.numeric)))
corrplot(corr_matrix, method = "color", addCoef.col = "black",
         tl.col = "black", tl.srt = 45, number.cex = 0.7)

# Pairwise Scatterplots
pairs(df[numeric_cols], main = "Pairwise Scatterplots of Iris Features", pch = 19, col = as.numeric(df$species))

# Principal Component Analysis
cat("\n")
pca <- prcomp(df %>% select(where(is.numeric)), scale. = TRUE)
print(summary(pca))
biplot(pca, main = "PCA Biplot of Iris Features")