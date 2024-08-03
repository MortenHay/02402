# I.1
group1 <- c(27, 22, 18, 26, 24)
group2 <- c(32, 22, 32, 25, 25)
group3 <- c(29, 25, 30, 30, 24)

data <- data.frame(group = factor(rep(1:3, each = 5)), gain = c(group1, group2, group3))
fit <- lm(gain ~ group, data = data)
anova(fit)

# I.2
df <- 15 - 3
MSE <- 440.8 / df
MSE

# II.2
a <- 10
b <- -1
k <- 10000000
X0 <- rnorm(k, 0, 2)
X10 <- rnorm(k, 10, 2)
X20 <- rnorm(k, 20, 2)

Y0 <- 1 / (1 + exp(a + b * X0))
Y10 <- 1 / (1 + exp(a + b * X10))
Y20 <- 1 / (1 + exp(a + b * X20))

c(var(Y0), var(Y10), var(Y20))

# III.2
v1 <- 169.1
v2 <- 402.7
n1 <- 589
n2 <- 240

(v1 / n1 + v2 / n2)^2 / ((v1 / n1)^2 / (n1 - 1) + (v2 / n2)^2 / (n2 - 1))

# IV.1
x <- 709
n <- 2201
p <- x / n
alpha <- 0.05
p + c(-1, 1) * qnorm(1 - alpha / 2) * sqrt(p * (1 - p) / n)

# IV.2
prop.test(c(178, 212), c(706, 885), correct = FALSE)

# IV.3
prop.test(c(202, 117, 178, 212), c(325, 285, 706, 885), correct = FALSE)
qchisq(1 - alpha, 3)

# IV.4
alpha <- 0.05
p0 <- 0.20
prop.test(c(202, 709 - 202), c(325, 2201 - 325), correct = FALSE)

# V.1
sd <- 0.2
v <- sd^2
v20 <- 20 * v
sd20 <- sqrt(v20)
sd20

# V.2
n <- 21
m <- 6
k <- 5
q <- 3
dhyper(q, m, n - m, k)

# V.3
p <- 0.32
n <- 18
m <- 3
dbinom(m, 18, 0.32)

# VI.1
n <- 26
mu <- 200.3
sd <- 0.75
alpha <- 0.05
c(sqrt((n - 1) * sd^2 / qchisq(1 - alpha / 2, n - 1)), sqrt((n - 1) * sd^2 / qchisq(alpha / 2, n - 1)))

# VI.2
mu0 <- 200
tobs <- (mu - mu0) / (sd / sqrt(n))
2 * (1 - pt(abs(tobs), n - 1))
qt(0.95, n - 1)
tobs

# VI.3
ME <- 0.3
sd <- 0.75
alpha <- 0.05
beta <- 0.05
power.t.test(n = NULL, delta = ME, sd = 0.75, sig.level = alpha, power = 1 - beta, type = "one.sample")

# VII.4
v <- 0.137
n <- 168
se <- v / sqrt(n)
se

# VIII.1
wind <- c(
    1063, 1450, 879, 1980, 406, 1542, 1212,
    1157, 1730, 1105, 775, 856, 802, 851
)
elpris <- c(
    26.84, 24.87, 21.65, 13.26, 24.49, 21.90, 23.29,
    22.47, 19.26, 27.86, 27.96, 20.85, 21.83, 34.04
)

fit <- lm(elpris ~ wind)
alpha <- 0.01
summary(fit)
df <- 12
beta1 <- -0.006556
s1 <- 0.002688
beta1 + c(-1, 1) * qt(1 - alpha / 2, df) * s1

# VIII.2
alpha <- 0.05
x0 <- 1000
predict(fit, newdata = data.frame(wind = x0), interval = "predict", level = 1 - alpha)

# IX.2
alpha <- 0.05
qf(1 - alpha, 1, 2)

# XI.1
lam <- 4
dpois(0, lam)

# XI.2
ppois(4, 2 * lam, lower.tail = FALSE)
