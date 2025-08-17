# Central difference derivative operator
central_diff_coef <- function(derivative, order) {
  coef <- NULL
  if (derivative == 1) {
    if (order == 2) coef <- c(-1/2, 0, 1/2)
    if (order == 4) coef <- c(1/12, -2/3, 0, 2/3, -1/12)
    if (order == 6) coef <- c(-1/60, 3/20, -3/4, 0, 3/4, -3/20, 1/60)
  } else if (derivative == 2) {
    if (order == 2) coef <- c(1, -2, 1)
    if (order == 4) coef <- c(-1/12, 4/3, -5/2, 4/3, -1/12)
    if (order == 6) coef <- c(1/90, -3/20, 3/2, -49/18, 3/2, -3/20, 1/90)
  } else if (derivative == 3) {
    if (order == 2) coef <- c(-1/2, 1, 0, -1, 1/2)
    if (order == 4) coef <- c(1/8, -1, 13/8, 0, -13/8, 1, -1/8)
    if (order == 6) coef <- c(-7/240, 3/10, -169/120, 61/30, 0, -61/30, 169/120, -3/10, 7/240)
  } else if (derivative == 4) {
    if (order == 2) coef <- c(1, -4, 6, -4, 1)
    if (order == 4) coef <- c(-1/6, 2, -13/2, 28/3, -13/2, 2, -1/6)
    if (order == 6) coef <- c(7/240, -2/5, 169/60, -122/15, 91/8, -122/15, 169/60, -2/5, 7/240)
  } else if (derivative == 5) {
    if (order == 2) coef <- c(-1/2, 2, -5/2, 0, 5/2, -2, 1/2)
    if (order == 4) coef <- c(1/6, -3/2, 13/3, -29/6, 0, 29/6, -13/3, 3/2, -1/6)
    if (order == 6) coef <- c(-13/288, 19/36, -87/32, 13/2, -323/48, 0, 323/48, -13/2, 87/32, -19/36, 13/288)
  } else if (derivative == 6) {
    if (order == 2) coef <- c(1, -6, 15, -20, 15, -6, 1)
    if (order == 4) coef <- c(-1/4, 3, -13, 29, -75/2, 29, -13, 3, -1/4)
    if (order == 6) coef <- c(13/240, -19/24, 87/16, -39/2, 323/8, -1023/20, 323/8, -39/2, 87/16, -19/24, 13/240)
  }
  
  return(coef)
}

# Apply central difference to approximate derivative
central_diff <- function(f, x, d = 1, order = 2) {
  h <- x[2] - x[1]
  y <- f(x)
  coef <- central_diff_coef(d, order)
  if (is.null(coef)) stop("Coefficient not available for this derivative and order")
  pad <- floor(length(coef)/2)
  deriv <- rep(NA, length(x))
  for (i in (1+pad):(length(x)-pad)) {
    deriv[i] <- sum(coef * y[(i-pad):(i+pad)]) / (h^d)
  }
  return(deriv)
}

# Example: f(x) = sin(x), compute derivative f'(x) ≈ cos(x)
x <- seq(0, 2*pi, length.out = 1000)
f <- function(x) sin(x)
approx_deriv <- central_diff(f, x, d = 1, order = 6) # 1st derivative, 6th order accuracy
plot(x, approx_deriv, type="l", col = "blue", xlab="x", ylab="f'(x)")

# Example: f(x) = x^3, compute derivative f"(x) ≈ 6x
x <- seq(-5, 5, length.out = 1000)
f <- function(x) x**3
approx_deriv <- central_diff(f, x, d = 2, order = 4) # 2st derivative, 4th order accuracy
plot(x, approx_deriv, type="l", col = "blue", xlab="x", ylab='f"(x)')