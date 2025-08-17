# Define objective function
f_obj <- function(par) {
  x <- par[1]
  y <- par[2]
  return((x - 2)^2 + (y + 3)^2)
}

# Initial guess
start <- c(0, 0)

# Nelder-Mead (derivative-free)
res_nm <- optim(start, f_obj, method="Nelder-Mead")
cat("Nelder-Mead.", "parameters:", res_nm$par, "value:", sprintf("%.6e", res_nm$value), "\n")

# BFGS (gradient-based)
res_bfgs <- optim(start, f_obj, method="BFGS")
cat("BFGS.", "parameters:", res_bfgs$par, "value:", sprintf("%.6e", res_bfgs$value), "\n")

# Plotting
x <- seq(-10, 10, 0.1)
y <- seq(-10, 10, 0.1)
z <- outer(x, y, function(x,y) (x-2)^2 + (y+3)^2)
contour(x, y, z, nlevels=25, col="blue")
points(2, -3, col="red", pch=19, cex=1.5)
points(res_nm$par[1], res_nm$par[2], col="black", pch=4, cex=1.5, lwd=2)
legend("bottomleft", legend=c("Exact","Nelder-Mead"), 
       col=c("red","black"), pch=c(19,4))