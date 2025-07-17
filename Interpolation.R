set.seed(42)

# Linear Interpolation
x <- c(1, 2, 3, 4, 5)
y <- c(2, 4, 1, 3, 7)
interpolated <- approx(x, y, xout = seq(1, 5, .1))
plot(x, y)
lines(interpolated, col="blue")

# Cubic Spline Interpolation
interpolated <- spline(x, y, xout = seq(1, 5, .01))
plot(x, y)
lines(interpolated, col="blue")