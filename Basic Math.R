# Aritmethic
a <- 9
b <- 10
a + b
a - b
a * b
a / b
a ^ b
sqrt(a)
exp(a)
log(b)
log10(b)
factorial(b)

# Trigonometry
sin(pi/2)
cos(pi)
tan(pi/3)

# Complex Number
z <- a + b*1i
Mod(z)
Arg(z)
Conj(z)
Re(z)
Im(z)

# Function
f <- function(x) {
  return(x^4 + x^3 + x^2 + x + 1)
}
f(b)

# Looping
for (x in 1:5) {
  print(x^2)
}

# Boolean
x <- 5
x > 0
x == 0
x != 0

# If-else
if (x > 0) {
  print("Positive")
} else {
  print("Negative")
}

# Visualization
x <- seq(0, 2*pi, length.out=1000)
y <- sin(x)
plot(x, y, type="l", col="blue", xlab="x", ylab="sin(x)")