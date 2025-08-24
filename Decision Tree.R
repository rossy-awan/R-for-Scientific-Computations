# Load Libraries
library(rpart)

fit <- rpart(mpg ~ cyl + disp + hp, method = "anova", data = mtcars)
print(fit)
print(summary(fit))

# Evaluate
pred <- predict(fit, mtcars)
cat("MSE:", mean((mtcars$mpg - pred)^2), "\n")
cat("R-squared:", 1 - (sum((mtcars$mpg - pred)^2) / sum((mtcars$mpg - mean(mtcars$mpg))^2)), "\n")