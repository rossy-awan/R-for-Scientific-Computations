# Root Finding Methods in R

# Bisection Method
bisection <- function(f, a, b, tol = 1e-8, max_iter = 100) {
  if (f(a) * f(b) > 0) stop("f(a) and f(b) must have opposite signs")
  for (i in 1:max_iter) {
    c <- (a + b) / 2
    if (abs(f(c)) < tol || (b - a) / 2 < tol) return(list(root = c, iter = i))
    if (f(a) * f(c) < 0) b <- c else a <- c
  }
  warning("Max iterations reached")
  return(list(root = c, iter = max_iter))
}

# Newton-Raphson Method
newton_raphson <- function(f, fprime, x0, tol = 1e-8, max_iter = 100) {
  x <- x0
  for (i in 1:max_iter) {
    x_new <- x - f(x) / fprime(x)
    if (abs(x_new - x) < tol) return(list(root = x_new, iter = i))
    x <- x_new
  }
  warning("Max iterations reached")
  return(list(root = x, iter = max_iter))
}

# Secant Method
secant <- function(f, x0, x1, tol = 1e-8, max_iter = 100) {
  for (i in 1:max_iter) {
    if (f(x1) - f(x0) == 0) stop("Division by zero in secant method")
    x2 <- x1 - f(x1) * (x1 - x0) / (f(x1) - f(x0))
    if (abs(x2 - x1) < tol) return(list(root = x2, iter = i))
    x0 <- x1
    x1 <- x2
  }
  warning("Max iterations reached")
  return(list(root = x2, iter = max_iter))
}

# Example function: f(x) = x^3 - 2x - 5
f <- function(x) x^3 - 2*x - 5
fprime <- function(x) 3*x^2 - 2

# Bisection
cat("Bisection. root:", sprintf("%.8f", bisection(f, 2, 3)$root), "iteration:", bisection(f, 2, 3)$iter, "\n")

# Newton-Raphson
cat("Newton-Raphson. root:", sprintf("%.8f", newton_raphson(f, fprime, x0 = 2.5)$root), "iteration:", newton_raphson(f, fprime, x0 = 2.5)$iter, "\n")

# Secant
cat("Secant. root:", sprintf("%.8f", secant(f, 2, 3)$root), "iteration:", secant(f, 2, 3)$iter, "\n")