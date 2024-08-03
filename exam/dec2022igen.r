system("R")

# II.1
y <- c(59, 52, 42, 59, 24, 24, 40, 32, 63, 55, 34, 24)
x <- c(298, 303, 270, 287, 236, 245, 265, 233, 286, 290, 264, 239)

fit <- lm(y ~ x)
summary(fit)

# II.3
alpha <- 0.05
qt(1 - alpha / 2, df = 10)

# III.2
tobs <- 45.117
mu <- 6.461533
se <- mu / tobs
alpha <- 0.01
mu + c(-1, 1) * qt(1 - alpha / 2, 34) * se

# III.3
mu <- 1.857769
V <- 0.01643549
n <- 35
alpha <- 0.05
mu + c(-1, 1) * qt(1 - alpha / 2, n - 1) * sqrt(V / n)

# III.5
sd <- 0.8
ME <- 0.1
alpha <- 0.05
ceiling((qnorm(1 - alpha / 2) * sd / ME)^2)

# IV.1
14.163 + 20.305

# V.1
p <- 0.35
n <- 6
pbinom(1, n, p, lower.tail = FALSE)

# V.2
n * p * (1 - p)

# V.3
rate <- 1 / 15
pexp(20, rate, lower.tail = FALSE)

# V.4
qexp(0.9, rate)

# VII.1
tobs <- 0.371
2 * (1 - pt(tobs, df = 10))

# VII.2
df <- 10
k <- 3
n <- df + k
n

# VIII.1
Fl <- 2.1576
FTr <- 7.1192
1 - pf(Fl, 2, 8)
1 - pf(FTr, 4, 8)

# VIII.3
alpha <- 0.05
k <- 5
M <- k * (k - 1) / 2
alpha_bon <- alpha / M
alpha_bon

# X.1
x <- 852
n <- 1268
prop.test(x, n, correct = FALSE)

# X.2
x1 <- 746
n1 <- 852
x2 <- 339
n2 <- 416
prop.test(c(x1, x2), c(n1, n2), correct = FALSE)

# X.3
416 * 1085 / 1268

# X.4
alpha <- 0.05
qchisq(1 - alpha, df = 2)
