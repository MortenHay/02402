system("R")
# QI.1
mu_H <- 0.6
mu_R <- 0.23
s_H <- 5e-3
s_R <- 3e-3
V_H <- s_H^2
V_R <- s_R^2
vol <- function(H, R) {
    pi * H * R^2
}
vol_H <- pi * mu_R^2
vol_R <- pi * mu_H * 2 * mu_R
V <- vol_H^2 * V_H + vol_R^2 * V_R
V
c(vol_H, vol_R)

# QII.1
p <- 0.9
p^2

# alt
dbinom(x = 2, size = 2, prob = p)

# QIII.1
mu <- 55
s <- 19
p <- 0.85
qnorm(p, mu, s)

# QIV.1
p <- 0.7
1 - phyper(1, 7, 3, 3)

# QVI.1
x <- c(8500, 10300, 6800, 10600, 4900, 6200, 10800, 5700, 5100, 9000)
alpha <- 0.01
t.test(x, conf.level = 1 - alpha)

# QVI.2
alpha <- 0.05
mu0 <- 10000
t.test(x, mu = mu0)

# QVI.3
n <- 14
alpha <- 0.01
beta <- 0.2
ME <- 1000
power.t.test(n = n, delta = ME, power = 1 - beta, sig.level = alpha, sd = NULL)

# QVIII.1
beta1 <- 0.027634
beta1 * 5

# QVIII.2
alpha <- 0.01
tcrit <- qt(1 - alpha / 2, df = 24)
tcrit

# QVIII.3
year <- 1990:2015
DO <- c(
    1.52, 2.88, 1.60, 2.24, 2.45, 1.84, 2.03, 2.33, 2.81,
    2.46, 2.36, 2.23, 2.81, 2.70, 2.63, 2.00, 2.40, 2.45,
    2.48, 2.51, 2.55, 2.77, 2.70, 2.23, 2.88, 3.09
)
fit <- lm(DO ~ year)
predict(fit, newdata = data.frame(year = 2022), interval = "prediction")

# QIX.1
lam <- 2
pexp(2, rate = 1 / lam) - pexp(1, rate = 1 / lam)

# QX.1
heights <- c(
    162, 172, 178, 154, 173, 174, 166, 166,
    166, 164, 167, 163, 165, 170, 177
)
set.seed(1234)
k <- 1000
sim_samples <- replicate(k, rnorm(15, mean(heights), sd(heights)))
sim_stats <- apply(sim_samples, 2, median)
quantile(sim_stats, c(0.025, 0.975))

# QX.2
qlnorm(runif(10))
rlnorm(10)

# QXI.1
k <- 4
l <- 2
k * l

# QXI.2
alpha <- 0.05
M <- k * (k - 1) / 2
M

# QXII.1
consumption <- c(
    5.4, 4.5, 4.6, 4.4, 4.9, 3.3, 4.1, 4.6, 4.8, 4.6,
    5.2, 4.7, 4.4, 4.8, 4.8, 5.2, 4.9, 4.8, 5.6, 5.5
)
month <- factor(c(rep("Feb", 5), rep("May", 5), rep("Aug", 5), rep("Nov", 5)))
fit <- lm(consumption ~ month)
anova(fit)
SST <- 2.1215 + 2.948
SST

# QXII.2
alpha <- 0.05

# QXIII.1
lam <- 48

# QXIII.2
1 - ppois(1, lam * 5 / 60)

# QXIV.1
x <- 24
n <- 100
alpha <- 0.05
p <- x / n

z <- qnorm(1 - alpha / 2)
c(p, z, p * (1 - p))

# QXIV.2
x <- 5
n <- 101
p <- 5 / 101
n * p

x_2 <- x + 2
n_2 <- n + 4
p_2 <- x_2 / n_2
c(p_2, p_2 * (1 - p_2), n_2)

# QXIV.3
emp <- c(24, 32, 28, 12, 5)
uem <- c(10, 15, 27, 24, 24)
x2021 <- matrix(c(emp, uem), nrow = 5)
x2021
chisq.test(x2021)

# QXIV.4
x <- 82
n <- 503
p <- x / n
e <- p * 101
e

# XIV.5
