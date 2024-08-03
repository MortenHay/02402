system("R")

# I.1
p <- 0.03
pbinom(0, 10, p)

# I.2
qbinom(0.98, 10, p)

# II.1
x2 <- 1801
n2 <- 2024
p2 <- x2 / n2
sqrt(p2 * (1 - p2) / n2)

# II.2
alpha <- 0.01
x1 <- 1920
n1 <- 2000
p1 <- x1 / n1
prop.test(c(x1, x2), c(n1, n2), correct = FALSE, conf.level = 1 - alpha)

# III.3
alpha <- 0.01
power <- 0.98
delta <- 5
sd <- 4
power.t.test(sig.level = alpha, power = power, sd = sd, delta = delta)

# IV.1
rate <- 1 / 3

# V.1
k <- 1000000
X1 <- rnorm(k)
X2 <- rnorm(k)
Y <- exp(X1) + X2^4 + X1 * X2
sd(Y)

# VI.1
x <- c(0, 0, 2, 2, 4, 4, 6, 6, 8, 8, 10, 10)
y <- c(114, 14, 870, 1141, 2087, 2212, 3353, 2633, 3970, 4299, 4950, 5207)
fit <- lm(y ~ x)
summary(fit)

# VI.2
predict(fit, newdata = data.frame(x = 5), interval = "predict")

# VI.3
summary(fit)

# VII.1
data <- matrix(c(18, 28, 22, 14, 25, 17), nrow = 3, byrow = TRUE)
chisq.test(data)$expected

# VII.3
alpha <- 0.01
qchisq(1 - alpha, df = 2)

# VIII.1
mu <- 21.5
sd <- 9.8
n <- 56
mu0 <- 23
alpha <- 0.05
mu + c(-1, 1) * qt(1 - alpha / 2, n - 1) * sd / sqrt(n)

# VIII.2
tobs <- (mu - mu0) / (sd / sqrt(n))
tobs

# IX.1
lam <- 7
ppois(1, lam, lower.tail = FALSE)

# IX.2
dpois(0, lam / 2)

# X.1
y <- c(1.89, 2.35, 1.68, 2.11, 3.15, 2.16, 2.40, 2.59, 1.54, 2.02, 2.01, 2.11)
groups <- factor(c(rep(1:3, each = 4)))
fit <- lm(y ~ groups)
anova(fit)$Sum
sum(anova(fit)$Sum)

# XI.1
0.59 + 5.6

# XI.2
SSTr <- 0.2554
SSE <- 1.562
df1 <- 3
df2 <- 15
Fobs <- (SSTr / df1) / (SSE / df2)
Fobs
