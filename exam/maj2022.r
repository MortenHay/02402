system("R")

# QI.1
p <- 0.03
n <- 10
pbinom(0, n, p)

# QI.2
qbinom(0.98, n, p)

# QI.3
1.7^2

# QII.1
x2 <- 1801
n2 <- 2024
p2 <- x2 / n2
s2 <- sqrt(p2 * (1 - p2) / n2)
s2

# QII.2
alpha <- 0.01
x1 <- 1920
n1 <- 2000
p1 <- x1 / n1

prop.test(x = c(x1, x2), n = c(n1, n2), conf.level = 0.99, correct = FALSE)

# QII.3
alpha <- 0.01
power <- 0.98
d <- 5
V <- 16
power.t.test(n = NULL, delta = d, sd = sqrt(V), sig.level = alpha, power = power)

# QV.1
k <- 1000000
X1 <- rnorm(k)
X2 <- rnorm(k)
Y <- exp(X1) + X2^4 + X1 * X2
sd(Y)

# QVI.1
# Manganese concentrations
x <- c(0, 0, 2, 2, 4, 4, 6, 6, 8, 8, 10, 10)
# ICP-AES values
y <- c(114, 14, 870, 1141, 2087, 2212, 3353, 2633, 3970, 4299, 4950, 5207)
fit <- lm(y ~ x)
summary(fit)
predict(fit, newdata = data.frame(x = 5), interval = "prediction", conf.level = 0.95)

# QVI.2
summary(fit)

# QVII.1
X <- matrix(c(18, 28, 22, 14, 25, 17), ncol = 2, byrow = TRUE)
chisq.test(X)

e <- 59 * 36 / 124
e

# QVII.3
alpha <- 0.01
qchisq(1 - alpha, df = 2)

# QVIII.1
alpha <- 0.05
x <- 21.5
s <- 9.8
x0 <- 23
n <- 56
df <- 56 - 1
x + c(-1, 1) * qt(1 - alpha / 2, df) * s / sqrt(n)

# QVIII.2
tobs <- (x - x0) / (s / sqrt(n))
tobs

# QIX.1
lam <- 7
ppois(1, lam, lower.tail = FALSE)

# QIX.2
dpois(0, lam / 2)

y <- c(1.89, 2.35, 1.68, 2.11, 3.15, 2.16, 2.40, 2.59, 1.54, 2.02, 2.01, 2.11)
group <- factor(c(rep(1, 4), rep(2, 4), rep(3, 4)))
fit <- lm(y ~ group)
anova(fit)
SST <- 1.01165 + 0.98117
SST

# QXI.1
alpha2 <- 0.59
mu <- 5.6

# QXI.2
DFt <- 3
DFR <- 15
MSt <- 0.08513
MSE <- 0.10414
F <- MSt / MSE
F
