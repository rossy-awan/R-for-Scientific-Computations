# Numerical ODE solver in R
ode_solver <- function(f, y0, x, dx, method = "RK4") {
  y <- numeric(length(x))
  y[1] <- y0
  
  # Euler method
  if (method == "Euler") {
    for (i in 1:(length(x)-1)) {
      y[i+1] <- y[i] + dx * f(x[i], y[i])
    }
  }
  
  # Heun method (Improved Euler / Trapezoidal)
  else if (method == "Heun") {
    for (i in 1:(length(x)-1)) {
      k1 <- f(x[i], y[i])
      k2 <- f(x[i+1], y[i] + dx * k1)
      y[i+1] <- y[i] + dx * (k1 + k2) / 2
    }
  }
  
  # Midpoint method
  else if (method == "Midpoint") {
    for (i in 1:(length(x)-1)) {
      k1 <- f(x[i], y[i])
      k2 <- f(x[i] + dx/2, y[i] + dx * k1 / 2)
      y[i+1] <- y[i] + dx * k2
    }
  }
  
  # Runge-Kutta 4
  else if (method == "RK4") {
    for (i in 1:(length(x)-1)) {
      k1 <- f(x[i], y[i])
      k2 <- f(x[i] + dx/2, y[i] + dx * k1 / 2)
      k3 <- f(x[i] + dx/2, y[i] + dx * k2 / 2)
      k4 <- f(x[i] + dx, y[i] + dx * k3)
      y[i+1] <- y[i] + dx * (k1 + 2*k2 + 2*k3 + k4) / 6
    }
  }
  
  else {
    stop("Unknown method. Choose 'Euler', 'Heun', 'Midpoint', or 'RK4'.")
  }
  
  return(y)
}

# dy/dx = y, exp(x)
f <- function(x, y) y
x <- seq(0, 5, .25)
dx <- .25
y_exact <- exp(x)
y_euler <- ode_solver(f, 1, x, dx, "Euler")
y_heun <- ode_solver(f, 1, x, dx, "Heun")
y_mid  <- ode_solver(f, 1, x, dx, "Midpoint")
y_rk4  <- ode_solver(f, 1, x, dx, "RK4")
plot(x, y_exact, type="l", lwd=2, col="black", ylim=c(0,150), ylab="y", xlab="x")
lines(x, y_euler, col="red", lwd=2, lty=2)
lines(x, y_heun, col="blue", lwd=2, lty=3)
lines(x, y_mid, col="purple", lwd=2,lty=4)
lines(x, y_rk4, col="green", lwd=2, lty=5)
legend("topleft", legend=c("Exact", "Euler", "Heun", "Midpoint", "RK4"),
       col=c("black","red","blue","purple","green"), lty=c(1,2,3,4,5), lwd=2)