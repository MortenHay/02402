system("R")

#  QI.1
p <- 0.27
n <- 15
dbinom(4, n, p)
pbinom(4, n, p) - pbinom(3, n, p)

# QII.1
lam <- 200
pexp(100, 1 / lam)

# QIII.1
lam <- 5
dpois(3, lam)

# QIV.1
x <- 4
n <- 52
dhyper(2, x, n - x, 5)

# QV
X <- c(2, 2, 4, 4, 4, 6, 6, 6, 6, 8)
mu <- 2 * 0.2 + 4 * 0.3 + 6 * 0.4 + 8 * 0.1
V <- mean((X - mu)^2)
V

# QVI.1
x <- c(
    875, 776, 915, 806, 1030, 1197, 768, 456,
    1171, 873, 777, 1108, 1031, 1009, 772
)
n <- length(x)
df <- n - 1
alpha <- 0.1
qt(1 - alpha / 2, df)

# QVI.2
mu0 <- 1000
tobs <- (mean(x) - mu0) / (sd(x) / sqrt(n))
2 * (1 - pt(abs(tobs), df))
t.test(x, mu = mu0)

# Q
