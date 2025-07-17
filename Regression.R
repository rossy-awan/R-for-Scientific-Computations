set.seed(42)

# Linear Regression
x <- 1:100
y <- 2*x + rnorm(100, 0, 10)
model <- lm(y ~ x)
summary(model)
plot(x, y)
abline(model, col="red")

# Nonlinear Regression
x <- 1:10
y <- 5 * exp(0.3 * x) + rnorm(10, 0, 2)
model <- nls(y ~ a * exp(b * x), data=data.frame(x, y), start=list(a=5, b=.1))
summary(model)
plot(x, y)
lines(x, predict(model), col="red")

# Logistic Regression
x <- seq(-5, 5, length.out=100)
y <- 1 / (exp(-x) + 1) + rnorm(100, 0, .05)
model <- nls(y ~ 1 / (exp(a * x) + b), data=data.frame(x, y), start=list(a=0, b=0))
summary(model)
plot(x, y)
lines(x, predict(model), col="red")