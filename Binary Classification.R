# Load Libraries
library(rpart)
library(randomForest)
library(Metrics)

# Reproducibility
set.seed(42)
n <- 100

# Function for Evaluate
get_accuracy <- function(true, pred) { mean(true == pred) }
evaluate_classification <- function(true, pred) {
  data.frame(
    Accuracy = round(mean(true == pred), 4),
    Recall = round(recall(true, pred), 4),
    F1_Score = round(f1(true, pred), 4)
  )
}

# Logistic Regression
cat("Logistic Regression\n")
train_data_logistic <- data.frame(
  predictor1 = rnorm(n, mean = 100, sd = 100),
  predictor2 = rnorm(n, mean = 50, sd = 50),
  target = sample(0:1, n, replace = TRUE)
)
log_model <- glm(target ~ predictor1 + predictor2, 
                 family = binomial, 
                 data = train_data_logistic)
log_pred <- predict(log_model, 
                    newdata = train_data_logistic, 
                    type = "response")
log_class <- ifelse(log_pred > 0.5, 1, 0)
print(evaluate_classification(train_data_logistic$target, log_class))

# Decision Tree with Grid Search
cat("\nDecision Tree (Grid Search)\n")
train_data_tree <- data.frame(
  predictor1 = rnorm(n, mean = 50, sd = 10),
  predictor2 = rnorm(n, mean = 30, sd = 5),
  target = sample(0:1, n, replace = TRUE)
)
cp_values <- seq(0.01, 0.2, by = 0.01)
best_acc_tree <- 0
best_cp <- NULL
for (cp in cp_values) {
  model_tree <- rpart(target ~ predictor1 + predictor2, 
                      data = train_data_tree, 
                      method = "class",
                      control = rpart.control(cp = cp))
    pred_tree <- predict(model_tree, 
                       newdata = train_data_tree, 
                       type = "class")
  acc <- get_accuracy(train_data_tree$target, pred_tree)
  if (acc > best_acc_tree) {
    best_acc_tree <- acc
    best_cp <- cp
    best_pred_tree <- pred_tree
  }
}
cat("Best cp:", best_cp, "\n")
print(evaluate_classification(train_data_tree$target, as.numeric(as.character(best_pred_tree))))

# Random Forest with Grid Search
cat("\nRandom Forest (Grid Search)\n")
train_data_rf <- data.frame(
  predictor1 = rnorm(n, mean = 50, sd = 10),
  predictor2 = rnorm(n, mean = 30, sd = 5),
  target = sample(0:1, n, replace = TRUE)
)
train_data_rf$target <- factor(train_data_rf$target, levels = c(0, 1))
ntree_values <- seq(5, 50, by = 5)
mtry_values <- 1:2
best_acc_rf <- 0
best_params_rf <- list()
for (ntree in ntree_values) {
  for (mtry in mtry_values) {
    model_rf <- randomForest(target ~ predictor1 + predictor2,
                             data = train_data_rf,
                             ntree = ntree,
                             mtry = mtry)
    pred_rf <- predict(model_rf, newdata = train_data_rf)
    acc <- get_accuracy(train_data_rf$target, pred_rf)
    if (acc > best_acc_rf) {
      best_acc_rf <- acc
      best_params_rf <- list(ntree = ntree, mtry = mtry)
      best_pred_rf <- pred_rf
    }
  }
}
cat("Best ntree:", best_params_rf$ntree, 
    "Best mtry:", best_params_rf$mtry, "\n")
print(evaluate_classification(train_data_rf$target, as.numeric(as.character(best_pred_rf))))