system("R")

# QI.1
mu <- 3505.7
s <- 467.9
alpha <- 0.05
n <- 50
df <- n - 1
mu + c(-1, 1) * qt(1 - alpha / 2, df) * s / sqrt(n)

# QI.2
tobs <- 0.857
2 * (1 - pt(tobs, df))

# QI.3
mu2 <- 3619.4
s2 <- 409

df <- (s^2 / n + s2^2 / n)^2 / (((s^2 / n)^2) / (n - 1) + ((s2^2 / n)^2) / (n - 1))
df

# QII.1
n <- 200
x <- 53
p <- x / n
30 * p

# QII.2
alpha <- 0.05
x <- 84 + 63
n <- 200
p <- x / n
p + c(-1, 1) * qnorm(1 - alpha / 2) * sqrt(p * (1 - p) / n)

# QII.3
alpha <- 0.05
n2 <- 90
n1 <- 80
x2 <- 23
x1 <- 35
p1 <- x1 / n1
p2 <- x2 / n2
s <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
p1 - p2 + c(-1, 1) * qnorm(1 - alpha / 2) * s

# QII.4
alpha <- 0.05
xobs <- 10.985
r <- 3
c <- 3
df <- (r - 1) * (c - 1)
pchisq(xobs, df, lower.tail = FALSE)

# QIII.1
logt <- c(
    -1.02, -1.24, -0.92, -1.47, -0.08, -0.49, -0.71, 0.22, -0.82, -1.05,
    -1.17, -0.92, -0.58, 0.02, -0.34, -0.97, -1.51, -1.56, -1.2, -0.99,
    -1.47, -1.39, -1.2, -1.02
)
treatments <-
    as.factor(c(
        "A", "A", "A", "A", "B", "B", "B", "B", "C", "C", "C", "C",
        "D", "D", "D", "D", "A", "A", "B", "B", "C", "C", "D", "D"
    ))
fit <- lm(logt ~ treatments)
mu <- mean(logt)
mus <- tapply(logt, treatments, mean)
alphas <- mus - mu
alphas

# QIII.2
anova(fit)

# alt
SSTr <- 2.286
SSE <- 3.241
k <- 4
n <- length(logt)
F <- SSTr / (k - 1) / (SSE / (n - k))
F

# QIII.3
alpha <- 0.05
M <- k * (k - 1) / 2
m <- n / k
alpha_bon <- alpha / M
df <- n - k
MSE <- SSE / df
alphas[4] - alphas[2] + c(-1, 1) * qt(1 - alpha / 2, df) * sqrt(2 * MSE / m)

# QIV.1
q3 <- 3.75
q1 <- 0.9375
iqr <- q3 - q1
iqr

# QIV.2
qexp(0.5, 1 / 2.7017)

# QV.1
DFd <- 6
DFm <- 3
k <- DFd + 1
l <- DFm + 1
n <- k * l
n

# QV.2
DFR <- 18
Fd <- 2.2731
1 - pf(Fd, df1 = DFd, df2 = DFR)

# QVI.2
eff <- 5.10493 + 0.05259 * 5
bat <- 54
eff * bat

# QVI.3
eff_fit <- 5.10493 + 0.05259 * 2.096
eff_real <- 4.744
eff_real - eff_fit

# QVII.1
dhyper(8, 8, 2, 8) * 100

# QVII.1
dhyper(3, 3, 2, 3) * 100

# QVII.3
p <- 0.2
pbinom(1, 10, p, lower.tail = FALSE)

# QVIII.1
ME <- 0.3
alpha <- 0.05
beta <- 0.2
s <- 1.5
n <- (s * (qnorm(1 - beta) + qnorm(1 - alpha / 2)) / ME)^2
ceiling(n)
