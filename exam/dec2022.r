# QI.1
mu_X <- 2
mu_Y <- -1
V_X <- 2
V_Y <- 3
2 * mu_X + mu_Y

# QII.1
y <- c(59, 52, 42, 59, 24, 24, 40, 32, 63, 55, 34, 24)
x <- c(298, 303, 270, 287, 236, 245, 265, 233, 286, 290, 264, 239)

fit <- lm(y ~ x)
summary(fit)

# QII.2
qqnorm(fit$residuals)
qqline(fit$residuals)

# QII.3
alpha <- 0.05
qt(1 - alpha / 2, df = 10)

# QIII.2
alpha <- 0.01
mu <- 6.461533
df <- 34
t95 <- qt(0.975, df = df)
s <- (6.752586 - mu) / t95
mu + c(-1, 1) * qt(1 - alpha / 2, df = df) * s

# alt
alpha <- 0.01
mu <- 6.461533
df <- 34
tval <- 45.117
se <- mu / tval
mu + c(-1, 1) * qt(1 - alpha / 2, df = df) * se

# QIII.3
alpha <- 0.05
mu <- 1.857769
V <- 0.01643549
s <- sqrt(V) / sqrt(df + 1)
mu + c(-1, 1) * qt(1 - alpha / 2, df = df) * s

# QIII.5
alpha <- 0.05
s <- 0.8
ME <- 0.1
n <- (qnorm(1 - alpha / 2) * s / ME)^2
ceiling(n)

# QIV.1
SST <- 14.163 + 20.305
SST

# QV.1
p <- 0.35
n <- 6
pbinom(1, n, p, lower.tail = FALSE)

# QV.2
V <- n * p * (1 - p)
V

# QV.3
lam <- 15
pexp(20, rate = 1 / lam, lower.tail = FALSE)

# QV.4
p <- 0.9
qexp(p, 1 / lam)

# QVI.1
runif(50, 0, 100)

# VII.1
tobs <- 0.371
df <- 10
2 * (1 - pt(tobs, df))

# QVII.2
p <- 2
n <- df + p + 1
n

# QVIII.1
alpha <- 0.05
DFl <- 2
Fl <- 2.1576
DFt <- 4
Ft <- 7.1192
DFR <- 8
PVl <- pf(Fl, df1 = DFl, df2 = DFR, lower.tail = FALSE)
PVt <- pf(Ft, df1 = DFt, df2 = DFR, lower.tail = FALSE)
c(PVl, PVt) * 100

# QVIII.2
k <- DFt + 1
l <- DFl + 1
n <- k * l
n

# QVIII.3
alpha <- 0.05
M <- k * (k - 1) / 2
alpha_bon <- alpha / M
alpha_bon * 100

# QX.1
x <- 852
n <- 1268
p <- x / n
alpha <- 0.05
p + c(-1, 1) * qnorm(1 - alpha / 2) * sqrt(p * (1 - p) / n)

# QX.2
alpha <- 0.05
x_m <- 339
n_m <- 416
x_f <- 746
n_f <- 852
p_m <- x_m / n_m
p_f <- x_f / n_f
s <- sqrt(p_f * (1 - p_f) / n_f + p_m * (1 - p_m) / n_m)
p_f - p_m + c(-1, 1) * qnorm(1 - alpha / 2) * s

# QX.3
x <- 1085
n <- 1268
p <- x / n
p * 416

# QX.4
alpha <- 0.05
r <- 2
c <- 3
df <- (r - 1) * (c - 1)
qchisq(1 - alpha, df = df)
