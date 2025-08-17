# Newton–Cotes integration rule implementation in R
newton_cotes <- function(f, a, b, N, O) {
  if (N %% O != 0) {
    stop("N must be a multiple of O")
  }
  h <- (b - a) / N
  y <- f(seq(a, b, length.out = N + 1))
  
  # Predefined Newton–Cotes weights (order 1 to 8)
  weights <- list(
    "1" = (h / 2) * c(1, 1),
    "2" = (h / 3) * c(1, 4, 1),
    "3" = (3 * h / 8) * c(1, 3, 3, 1),
    "4" = (2 * h / 45) * c(7, 32, 12, 32, 7),
    "5" = (5 * h / 288) * c(19, 75, 50, 50, 75, 19),
    "6" = (h / 140) * c(41, 216, 27, 272, 27, 216, 41),
    "7" = (7 * h / 17280) * c(751, 3577, 1323, 2989, 2989, 1323, 3577, 751),
    "8" = (4 * h / 14175) * c(989, 5888, -928, 10496, -4540, 10496, -928, 5888, 989)
  )
  
  w <- weights[[as.character(O)]]
  m <- length(w)
  total <- 0
  for (i in seq(1, length(y) - m + 1, by = O)) {
    total <- total + sum(y[i:(i+m-1)] * w)
  }
  return(total)
}

# Test 1: f(x) = sin(x), integral from 0 to pi (expected = 2)
cat("Integral sin(x) from 0 to pi:\n")
for (i in 1:8) {
  approx_val <- newton_cotes(function(x) sin(x), 0, pi, i, i)
  err <- abs(2 - approx_val)
  cat(i, "=>", sprintf("%.6f", approx_val), "error:", sprintf("%.4e", err), "\n")
}

# Test 2: f(x) = exp(x), integral from 0 to 1 (expected ~ 1.71828)
cat("\nIntegral exp(x) from 0 to 1:\n")
for (i in 1:8) {
  approx_val <- newton_cotes(function(x) exp(x), 0, 1, i, i)
  err <- abs((exp(1) - 1) - approx_val)
  cat(i, "=>", sprintf("%.6f", approx_val), "error:", sprintf("%.4e", err), "\n")
}

# Test 3: f(x) = x^2, integral from 0 to 3 (expected = 9)
cat("\nIntegral x^2 from 0 to 3:\n")
for (i in 1:8) {
  approx_val <- newton_cotes(function(x) x^2, 0, 3, i, i)
  err <- abs(9 - approx_val)
  cat(i, "=>", sprintf("%.6f", approx_val), "error:", sprintf("%.4e", err), "\n")
}