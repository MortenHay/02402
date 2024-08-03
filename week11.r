k <- 3
m <- 6
n <- k * m
SST <- 11.4944
SSE <- 4.1060
SSTr <- SST - SSE
MSTr <- SSTr / (k - 1)
MSE <- SSE / (n - k)
F <- MSTr / MSE
alpha <- 0.05
cv <- qf(p = 1 - alpha, df1 = k - 1, df2 = n - k)
c(k - 1, SSTr, MSTr, n - k)

c(F, cv)

alpha <- 0.1
mu_2011 <- 4.0483
mu_1998 <- 5.5517
(mu_2011 - mu_1998) + c(-1, 1) * qt(p = 1 - alpha / 2, df = n - k) * sqrt(MSE * (1 / 6 + 1 / 6))

mu_1998 <- 5.552
mu_2003 <- 5.190
mu_2011 <- 4.048
mus <- c(mu_1998, mu_2003, mu_2011)

var_1998 <- 0.2366
var_2003 <- 0.1313
var_2011 <- 0.4533
vars <- c(var_1998, var_2003, var_2011)

mu <- 4.93

SSTr <- m * sum((mus - mu)^2)
SSTr
SSE <- sum((m - 1) * vars)
SSE
SST <- SSTr + SSE
SST

nitrogen <- c(
    5.01, 5.59, 3.02,
    6.23, 5.13, 4.76,
    5.98, 5.33, 3.46,
    5.31, 4.65, 4.12,
    5.13, 5.52, 4.51,
    5.65, 4.92, 4.42
)

var(nitrogen)
SST <- (n - 1) * var(nitrogen)
SST

year <- factor(rep(c("1998", "2003", "2011"), 6))
anova(lm(nitrogen ~ year))

alpha <- 0.05
MSE <- 0.2737
M <- k * (k - 1) / 2
alphaM <- alpha / M

t1 <- (mu_1998 - mu_2003) / (sqrt(MSE) * sqrt(1 / m + 1 / m))
t2 <- (mu_1998 - mu_2011) / (sqrt(MSE) * sqrt(1 / m + 1 / m))
t3 <- (mu_2003 - mu_2011) / (sqrt(MSE) * sqrt(1 / m + 1 / m))

pv1 <- 2 * pt(-abs(t1), df = n - k)
pv2 <- 2 * pt(-abs(t2), df = n - k)
pv3 <- 2 * pt(-abs(t3), df = n - k)

alphaM
c(pv1, pv2, pv3) < alphaM


k <- 5
m <- 20
n <- k * m
SSTr <- 62
SSE <- 362.71
MSTr <- SSTr / (k - 1)
MSE <- SSE / (n - k)
F <- MSTr / MSE
alpha <- 0.05
1 - pf(F, df1 = k - 1, n - k)

mus <- c(31.4, 30.6, 30.5, 31.3, 29.2)

CI <- mus[1] - mus[4] + c(-1, 1) * qt(1 - alpha / 2, df = n - k) * sqrt(2 * MSE / m)
CI

M <- k * (k - 1) / 2
alphaM <- alpha / M
LSD <- qt(p = 1 - alpha / 2, df = n - k) * sqrt(2 * MSE / m)
sort(mus)
diff(sort(mus)) > LSD

MSE <- 23.983
k <- 6
m <- 5
n <- k * m
SSE <- MSE * (n - k)
SSE

mu1 <- mean(c(82.5, 83.7, 80.9, 95.2, 80.8))
mu2 <- mean(c(82.7, 81.9, 78.9, 83.6, 78.6))
c(mu1, mu2)

alpha <- 0.05
CI <- mu1 - mu2 + c(-1, 1) * qt(1 - alpha / 2, df = n - k) * sqrt(2 * MSE / m)
CI
