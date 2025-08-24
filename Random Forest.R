# Load Libraries
library(randomForest)
library(ggplot2)
library(dplyr)
library(caret)
library(RColorBrewer)

set.seed(42)

# Split Data (80% Train, 20% Test)
trainIndex <- sample(1:nrow(iris), 0.8 * nrow(iris))
trainData <- iris[trainIndex, ]
testData  <- iris[-trainIndex, ]

# Build Random Forest Model
iris.rf <- randomForest(
  Species ~ .,
  data = trainData,
  ntree = 300,         # number of trees
  mtry = 2,            # number of variables sampled per split
  importance = TRUE,
  proximity = TRUE
)

# Confusion Matrix & Metrics
print(iris.rf)
predictions <- predict(iris.rf, newdata = testData)
print(confusionMatrix(predictions, testData$Species))

# Plot OOB Error (Training Error)
plot(iris.rf, main = "Random Forest Error Rate", lwd = 2, col = "blue")

# Variable Importance Plot
varImpPlot(
  iris.rf,
  sort = TRUE,
  n.var = 4,
  main = "Variable Importance",
  col = brewer.pal(4, "Set2")
)