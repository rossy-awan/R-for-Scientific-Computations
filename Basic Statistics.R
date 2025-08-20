# Popular built-in data set in R 
df <- mtcars
?mtcars

# Descriptive Statistics for mtcars
cat("===== Summary Statistics (base R) =====\n")
cat(dim(df), "\n")
print(summary(df))

cat("\nSummary Statistics\n")
for (col in names(df)) {
  x <- df[[col]]
  cat("==========", col, "==========\n")
  cat("Mean     :", mean(x), "\n")
  cat("Median   :", median(x), "\n")
  cat("Range    :", paste(range(x), collapse = " - "), "\n")
  cat("Variance :", var(x), "\n")
  cat("SD       :", sd(x), "\n")
  cat("IQR      :", IQR(x), "\n\n")
}

# Distribution Analysis
cat("Distribution Analysis\n")
skewness <- function(x) {
  m <- mean(x)
  s <- sd(x)
  n <- length(x)
  sum(((x - m)/s)^3) / n
}

kurtosis <- function(x) {
  m <- mean(x)
  s <- sd(x)
  n <- length(x)
  sum(((x - m)/s)^4) / n
}

par(mfrow=c(3,4))
for (col in names(df)) {
  x <- df[[col]]
  cat("==========", col, "==========\n")
  cat("Skewness :", skewness(x), "\n")
  cat("Kurtosis :", kurtosis(x), "\n\n")
  
  # Histogram + Density
  hist(x, prob=TRUE, main=paste("Distribution of", col),
       xlab=col, col="lightblue", border="white")
  lines(density(x), col="black", lwd=2)
}

par(mfrow=c(3,4))
for (col in names(df)) {
  boxplot(df[[col]], main=paste("Boxplot of", col), col="lightblue")
}

# Probability Analysis using mtcars
cat("P(cyl=4) =", round(mean(df$cyl == 4), 4), "\n")
cat("P(mpg>25) =", round(mean(df$mpg > 25), 4), "\n")
cat("P(cyl=4 | mpg>25) =", round(mean(df$cyl[df$mpg > 25] == 4), 4), "\n\n")

# Cumulative Frequency and Probability Table ----
freq_cyl <- table(df$cyl)
prob_cyl <- prop.table(freq_cyl)
cyl_table <- data.frame(
  Cyl = names(freq_cyl),
  Freq = as.vector(freq_cyl),
  CumFreq = as.vector(cumsum(freq_cyl)),
  Prob = round(as.vector(prob_cyl), 4),
  CumProb = round(as.vector(cumsum(prob_cyl)), 4)
)

cat("Cumulative Frequency & Probability (cyl):\n")
print(cyl_table)
cat("\n")

# Expected value of gear
freq_gear <- table(df$gear)
x_vals <- as.numeric(names(freq_gear))
px_vals <- as.vector(prop.table(freq_gear))
expected_gear <- sum(x_vals * px_vals)
cat("Expected value of gear =", round(expected_gear,4), "\n")
cat("Expected value of gear =", round(mean(df$gear),4), "\n\n")

# Correlation & Covariance in R with mtcars
cat("Covariance & Correlation Analysis\n")
cat("Covariance Matrix:\n")
print(round(cov(df), 4))
cat("\n")

cat("Pearson Correlation Matrix:\n")
print(round(cor(df, method="pearson"), 4))
cat("\n")

cat("Spearman's Rank Correlation Matrix:\n")
print(round(cor(df, method="spearman"), 4))
cat("\n")

cat("Kendall Correlation Matrix:\n")
print(round(cor(df, method="kendall"), 4))
cat("\n")

# Confidence Intervals
cat("Confidence Intervals\n")
for (col in names(df)) {
  cat("\n==========", col, "==========\n")
  print(t.test(df[[col]], conf.level = 0.95)$conf.int)
}