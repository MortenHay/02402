system("R")

# QI.1
mu <- 376.2
s <- 571.5
n <- 14
alpha <- 0.05
qt(1 - alpha / 2, n - 1)

# QI.2
mu + c(-1, 1) * qt(1 - alpha / 2, n - 1) * s / sqrt(n)
tobs <- mu * sqrt(n) / s
2 * (1 - pt(tobs, n - 1))

# QII.1
x <- 18
y <- 14
n <- x + y
X <- c(rep(1, x), rep(0, y))
sd(X)

# QII.2
p <- x / n
p

# QII.3
p <- 0.5
pbinom(x - 1, n, p, lower.tail = FALSE)

# QIV.2
mu <- 8.31
-125.28 + 23.25 * mu

# QIV.3
alpha <- 0.05
s <- 1.74
df <- 18
beta1 <- 23.25
beta1 + c(-1, 1) * qt(1 - alpha / 2, df) * s

# QIV.4
s <- 0.05
d <- 9.6
(2 * pi / 4 * d) * s

# QV.1
d <- 1
n <- 30
alpha <- 0.05
s1 <- 1.8
n1 <- 20
s2 <- 1.4
n2 <- 30
V <- (s1^2 * (n1 - 1) + s2^2 * (n2 - 1)) / (n1 + n2 - 2)
power.t.test(n = n, delta = d, sd = sqrt(V), sig.level = alpha, power = NULL)

# QVI.1
s <- 610.2105
n <- 20
mu <- s / n
mu

# QVII.1
SST <- 1.0855 + 1.84
SST

# QVII.2
alpha <- 0.05
MSTr <- 0.36183
MSE <- 0.115
F <- MSTr / MSE
F
qf(1 - alpha, 3, 16)

# QVII.3
alpha <- 0.05

# QIX.1
y <- c(
    3.5, 3.0, 5.4, 7.2,
    7.7, 9.0, 7.0, 6.0,
    0.4, 1.1, 1.0, 1.8
)
treatm <- as.factor(c(
    1, 1, 1, 1,
    2, 2, 2, 2,
    3, 3, 3, 3
))
block <- as.factor(c(
    1, 2, 3, 4,
    1, 2, 3, 4,
    1, 2, 3, 4
))

mu <- mean(y)
muis <- tapply(y, block, mean)
beta <- muis - mu
beta

# QIX.2
anova(lm(y ~ treatm + block))

MSE <- 2.477
s <- sqrt(MSE)
s

# QX.1
alpha <- 0.05
x <- 117 + 116
n <- 662
p <- x / n
p + c(-1, 1) * qnorm(1 - alpha / 2) * sqrt(p * (1 - p) / n)

# QX.2
x1 <- 31 + 36
n1 <- 189
x2 <- 31 + 30
n2 <- 175
p1 <- x1 / n1
p2 <- x2 / n2
p <- (x1 + x2) / (n1 + n2)
zobs <- (p1 - p2) / sqrt(p * (1 - p) * (1 / n1 + 1 / n2))
zobs

# QX.3
alpha <- 0.05
x1 <- 26
x2 <- 11
n1 <- 189
n2 <- 157
p1 <- x1 / n1
p2 <- x2 / n2
s <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
p1 - p2 + c(-1, 1) * qnorm(1 - alpha / 2) * s

# QX.4
alpha <- 0.05
qchisq(1 - alpha, df = 9)
1 - pchisq(15.356, df = 9)

# QX.5
e <- 344 * 189 / 662
o <- 96
(e - 96)^2 / e
