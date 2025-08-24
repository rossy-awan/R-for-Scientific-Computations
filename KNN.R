# Load Libraries
library(caTools)
library(class)
library(dplyr)

# Load & Explore Data
cat("Head of Data\n")
print(head(iris))
cat("\nSummary of Data\n")
print(summary(iris))

# Train-Test Split
set.seed(42) # reproducibility
split <- sample.split(iris$Species, SplitRatio = 0.7)
train_cl <- subset(iris, split == TRUE)
test_cl  <- subset(iris, split == FALSE)

# Feature Scaling
train_scale <- scale(train_cl[, 1:4])
test_scale  <- scale(test_cl[, 1:4])

# KNN Classification Function
knn_accuracy <- function(k) {
  pred <- knn(train = train_scale,
              test = test_scale,
              cl = train_cl$Species,
              k = k)
  acc <- mean(pred == test_cl$Species)
  return(acc)
}

# Evaluate Multiple K
k_values <- seq(1, 10, by=1)
accuracy_values <- sapply(k_values, knn_accuracy)
accuracy_data <- data.frame(K = k_values, Accuracy = accuracy_values)
cat("\nAccuracy Table\n")
print(accuracy_data)

# Confusion Matrix for Best K
best_k <- k_values[which.max(accuracy_values)]
cat("\nBest K:", best_k, "with Accuracy:", max(accuracy_values), "\n")
best_pred <- knn(train = train_scale,
                 test = test_scale,
                 cl = train_cl$Species,
                 k = best_k)
cat("\nConfusion Matrix\n")
print(table(Actual = test_cl$Species, Predicted = best_pred))