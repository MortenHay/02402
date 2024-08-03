system("R")

# I.1
mu <- 3505.7
s <- 467.9
n <- 50
alpha <- 0.05
mu + c(-1, 1) * qt(1 - alpha / 2, n - 1) * s / sqrt(n)

# I.2
tobs <- 0.857
2 * (1 - pt(tobs, n - 1))

# I.3
vp <- s^2
np <- n
vd <- 409^2
nd <- 50
alpha <- 0.05
df <- (vp / np + vd / nd)^2 / ((vp / np)^2 / (np - 1) + (vd / nd)^2 / (nd - 1))
df

# II.1
30 * 53 / 200

# II.2
x <- 84 + 63
n <- 200
p <- x / n
alpha <- 0.05
p + c(-1, 1) * qnorm(1 - alpha / 2) * sqrt(p * (1 - p) / n)

# II.3
x1 <- 35
n1 <- 80
x2 <- 23
n2 <- 90
prop.test(c(x1, x2), c(n1, n2), correct = FALSE)

# II.4
alpha <- 0.05
xobs <- 10.985
df <- (3 - 1) * (3 - 1)
1 - pchisq(xobs, df)

# III.1
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
anova(fit)

mu <- mean(logt)
muB <- -0.5416667
aB <- muB - mu

# III.3
alpha <- 0.05
muD <- -0.6816667
muD - muB + c(-1, 1) * qt(1 - alpha / 2, 20) * sqrt(2 * 3.2414 / 20 / 6)

# IV.1
3.75 - 0.9375

# IV.2
lam <- 2.7017
qexp(0.5, 1 / lam)

# V.1
7 * 4

# V.2
Fobs <- 2.2731
1 - pf(Fobs, 6, 18)

# v.3
bn <- 54
tn <- 5

en <- 5.10493 + tn * 0.05259
en * bn

# VI.3
tn <- 2.096
en <- 5.10493 + tn * 0.05259
e0 <- 4.744
e0 - en

# VII.1
n <- 2
m <- 8
k <- 8
q <- 8
dhyper(q, m, n, k)

# VII.2
n <- 2
m <- 8 - 5
k <- 8 - 5
q <- 8 - 5
dhyper(q, m, n, k)

# VII.3
p <- 0.2
pbinom(1, 10, p, lower.tail = FALSE)

# VIII.1
delta <- 0.3
alpha <- 0.05
power <- 0.8
sd <- 1.5
ceiling((sd * (qnorm(power) + qnorm(1 - alpha / 2)) / (delta))^2)
