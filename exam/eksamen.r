system("R")

# I.1
mu0 <- 18
n <- 48
mu <- 17
sd <- 4.5

tobs <- (mu - mu0) / (sd / sqrt(n))
tobs
alpha <- 0.05
c(qt(alpha / 2, n - 1), qt(1 - alpha / 2, n - 1))

# I.2
tobs <- -1.74
n <- 45
alpha <- 0.05
2 * (1 - pt(abs(tobs), n - 1))

# II.1
n <- 30
mu <- 1.01
sd <- 0.09
alpha <- 0.05
sqrt(c((n - 1) * sd^2 / qchisq(1 - alpha / 2, n - 1), (n - 1) * sd^2 / qchisq(alpha / 2, n - 1)))


# III.1
(0.17 + 0.22 + 0.28) / (0.17 + 0.22 + 0.28 + 0.33)

# III.2
mu <- 2.10
V <- (0 - mu)^2 * 0.17 + (1 - mu)^2 * 0.22 + (2 - mu)^2 * 0.28 + (4 - mu)^2 * 0.33
V

# IV.1
250 - 252.5

# IV.2
Y <- c(253.7, 241.2, 255.8, 249.3, 261.1, 244.2, 250.5, 264.9, 259.3, 257.9, 263.5, 258.6, 244.1, 244.9, 243.9, 247.1)
Location <- factor(c(1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 4, 4, 4, 4))
fit <- lm(Y ~ Location)
anova(fit)
MSE <- 36.327
s <- sqrt(MSE)
s

sqrt((915.12 - 480) / (16 - 4))
480 / 3
sqrt(anova(fit)$Mean[2])

# V.1
# PFOS concentrations
x <- c(0, 0, 2, 2, 4, 4, 6, 6, 8, 8, 12, 12)
# SPETT values
y <- c(16, 116, 1170, 841, 2287, 2012, 2653, 3333, 4270, 3999, 5750, 5407)

fit <- lm(y ~ x)
summary(fit)

# V.2
predict(fit, newdata = data.frame(x = 7), interval = "predict", level = 0.95)

# VI.1
x <- 20
n <- 85
p <- x / n
alpha <- 0.1
p + c(-1, 1) * qnorm(1 - alpha / 2) * sqrt(p * (1 - p) / n)

# VI.2
ME <- 0.1 / 2
alpha <- 0.05

n <- p * (1 - p) * (qnorm(1 - alpha / 2) / ME)^2
ceiling(n)

# VII.1
pre <- c(41, 46, 35, 49, 33, 42)
post <- c(42, 47, 43, 55, 28, 49)
t.test(pre, post, paired = TRUE)
t.test(pre - post)

# VIII.1
lam <- 2.5
ppois(4, lam, lower.tail = FALSE)
1 - ppois(4, lam)

# VIII.2
lam * 7

# VIII.3
p <- 0.78
dbinom(5, 5, p)
pbinom(4, 5, p, lower.tail = FALSE)

# IX.1
mu <- 4.03
sd <- 0.49
k <- 10000
alpha <- 0.01
set.seed(2023)
X <- replicate(k, rnorm(30, mu, sd))
Xm <- apply(X, 2, median)
quantile(Xm, c(alpha / 2, 1 - alpha / 2))
fun <- function(x) quantile(x, 0.25, type = 2)
sim_samples <- replicate(k, sample(length_cm, n, replace = TRUE))
sim_stats <- apply(sim_samples, 2, fun)
quantile(sim_stats, c(0.025, 0.975))

# X.1
n1 <- 12
n2 <- 12
mu1 <- 325
mu2 <- 286
s1 <- 40
s2 <- 44
v1 <- s1^2
v2 <- s2^2

tobs <- ((mu1 - mu2) / sqrt(v1 / n1 + v2 / n2))
df <- ((v1 / n1 + v2 / n2)^2) / ((v1 / n1)^2 / (n1 - 1) + (v2 / n2)^2 / (n2 - 1))
c(tobs, df)

# XI.2
k <- 10000000
x <- rnorm(k)^2 + rnorm(k)^2
quantile(x, 0.25)

# XII.2
4.593 * 2

(39.66 - 4 * 4.059 + 4.593 * 4) - (39.66 - 4 * 4.059 + 4.593 * 2)

# XIII.1
tbl <- matrix(c(
    12, 14, 18, 5, 15,
    38, 35, 35, 42, 35
), nrow = 2, byrow = TRUE)
chisq.test(tbl, correct = FALSE)$expected

# XIII.2
chisq.test(tbl, correct = FALSE)$expected
chisq.test(tbl, correct = FALSE)

# XIII.3
tbl2 <- matrix(c(25, 23, 28, 9, 21, 33, 19, 23, 19), nrow = 3)
chisq.test(tbl2, correct = FALSE)$expected
chisq.test(tbl2, correct = FALSE)

# XIV.1
f1 <- qf(1 - 0.432, 5, 20)
f2 <- qf(1 - 0.036, 4, 20)
1 - pf(f1, 5, 20)
1 - pf(f2, 4, 20)
c(f1, f2)
c(0.036 * f1, 0.432 * f2)

# XIV.3
k <- 5
M <- k * (k - 1) / 2
M
