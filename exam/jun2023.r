# QI.1
group1 <- c(27, 22, 18, 26, 24)
group2 <- c(32, 22, 32, 25, 25)
group3 <- c(29, 25, 30, 30, 24)

groups <- factor(c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3))
table1 <- c(group1, group2, group3)
anova((lm(table1 ~ groups)))

# Alt
data <- data.frame(
    group = factor(rep(1:3, each = 5)),
    weight_gain = c(group1, group2, group3)
)
model <- lm(weight_gain ~ group, data = data)
anova(model)

# QI.2
k <- 3
n <- 3 * 5
DFE <- n - k
SSE <- 440.8
MSE <- SSE / DFE
c(DFE, MSE)

# QII.2
a <- 10
b <- -1
k <- 100000
x1 <- rnorm(k, mean = 0)
x2 <- rnorm(k, mean = 10)
x3 <- rnorm(k, mean = 20)
var(1 / (1 + exp(a + b * x1)))
var(1 / (1 + exp(a + b * x2)))
var(1 / (1 + exp(a + b * x3)))

# QIII.2
alpha <- 0.1
1 - alpha / 2
nA <- 589
nB <- 240
VA <- 169.1
VB <- 402.7
df <- ((VA / nA + VB / nB)^2) / ((VA / nA)^2 / (nA - 1) + (VB / nB)^2 / (nB - 1))
df

# QIV.1
alpha <- 0.05
x <- 709
n <- 2201
p <- x / n
CI <- p + c(-1, 1) * qnorm(1 - alpha / 2) * sqrt(p * (1 - p) / n)
CI

# Alt
prop.test(x, n, correct = FALSE)

# QIV.2
alpha <- 0.05
x1 <- 178
n1 <- 706
x2 <- 212
n2 <- 885
p1 <- x1 / n1
p2 <- x2 / n2
p <- (x1 + x2) / (n1 + n2)
zobs <- (p1 - p2) / sqrt(p * (1 - p) * (1 / n1 + 1 / n2))
zobs
pval <- 2 * (1 - pnorm(zobs))
pval

# Alt
x <- c(x1, x2)
n <- c(n1, n2)
y <- n - x
chisq.test(cbind(x, y), correct = FALSE)

# QIV.3
alpha <- 0.05
x <- c(202, 117, 178, 212)
n <- c(325, 285, 706, 885)
df <- 3
y <- n - x
chisq.test(cbind(x, y), correct = FALSE)
qchisq(1 - alpha, df = 3)

# QIV.4
alpha <- 0.05
x1 <- 202
n1 <- 325
xr <- 117 + 178 + 212
nr <- 285 + 706 + 885
p1 <- x1 / n1
pr <- xr / nr
p <- (x1 + xr) / (n1 + nr)
s <- sqrt(p1 * (1 - p1) / n1 + pr * (1 - pr) / nr)
CI <- (p1 - pr) + c(-1, 1) * qnorm(1 - alpha / 2) * s
CI

# QV.1
mu <- 1
s <- 0.2
n <- 20
V <- s^2
Vn <- n * V
sn <- sqrt(Vn)
sn

# QV.2
x <- 6
n <- 21
p <- x / n
dhyper(3, x, n - x, 5)

# QV.3
p <- 0.32
dbinom(3, 18, p)

# QVI.1
n <- 26
mu <- 200.3
s <- 0.75
alpha <- 0.05
df <- n - 1
sqrt((n - 1) * s^2 / c(qchisq(1 - alpha / 2, df = df), qchisq(alpha / 2, df = df)))

# QVI.2
alpha1 <- 0.05
alpha2 <- 0.1
mu0 <- 200
tobs <- (mu - mu0) / (s / sqrt(n))
T1 <- qt(1 - alpha1 / 2, df = df)
T2 <- qt(1 - alpha2 / 2, df = df)
c(tobs, T1, T2)

# QVI.3
alpha <- 0.05
beta <- 0.05
ME <- 0.3
s <- 0.75
n <- (s * (qnorm(1 - beta) + qnorm(1 - alpha / 2)) / (ME))^2
ceiling(n)

# QVII.1
VlogC <- 0.137
x1 <- function(h) {
    sin(2 * pi * h / 24)
}
x2 <- function(h) {
    cos(2 * pi * h / 24)
}
p <- 2
df <- 165
n <- df + p + 1
n

# QVII.2
t0 <- 6.43959 / 0.01468
t1 <- 0.40303 / 0.02076
t2 <- 0.20019 / 0.02076
c(t0, t1, t2)

# QVII.4
VlogC / sqrt(n)

# QVIII.1
wind <- c(
    1063, 1450, 879, 1980, 406, 1542, 1212,
    1157, 1730, 1105, 775, 856, 802, 851
)
elpris <- c(
    26.84, 24.87, 21.65, 13.26, 24.49, 21.90, 23.29,
    22.47, 19.26, 27.86, 27.96, 20.85, 21.83, 34.04
)
D <- data.frame(x = wind, y = elpris)
fit <- lm(elpris ~ wind, data = D)
confint(fit, level = 1 - alpha)

# QVIII.2
alpha <- 0.05
out <- data.frame(wind = 1000)
predict(fit, newdata = out, interval = "prediction", level = 1 - alpha)

# QIX.2
alpha <- 0.05
df1 <- 1
df2 <- 2
qf(1 - alpha, df1, df2)

# QXI.1
mu <- 4
dpois(0, mu)

# QXI.2
mu <- 8
1 - ppois(4, mu)
