# QII.1
dfTr <- 9
dfE <- 50
SSTr <- 207
pval <- 0.03
SST <- 707
k <- dfTr + 1
n <- dfE + k
dfT <- n - 1
SSE <- SST - SSTr
MSTr <- SSTr / dfTr
MSE <- SSE / dfE
Fobs <- MSTr / MSE
A <- dfT
B <- SSE
C <- MSE
D <- MSTr
E <- Fobs
c(A, B, C, D, E)

# QII.2
m <- n / k
alpha <- 0.05
LSD <- qt(1 - alpha / 2, dfE) * sqrt(2 * MSE / m)
LSD

# QIII.1
n <- 234
t0 <- 160.53
mu <- 20.22413
mu0 <- 20
se <- mu / t0
tobs <- (mu - mu0) / se
2 * (1 - pt(abs(tobs), n - 1))

# III.4
alpha <- 0.01
qt(1 - alpha / 2, 53.627)

# IV.1
k <- 5
n <- k + 56
n

# V.1
p <- 0.05
n <- 100
k <- 10
pbinom(k - 1, n, p, lower.tail = FALSE)

# V.2
lam <- 200
ppois(250, lam)

# VI.1
X <- 1.2
Y <- 1.7
k <- 1000000

sim <- rexp(k, X) + rexp(k, Y)
mean(sim > 3)

# VII.1
Time_Driver_1 <- c(100.391, 101.506, 102.241, 103.058, 103.766, 104.801)
Time_Driver_2 <- c(100.495, 101.455, 102.468, 103.350, 104.230, 105.391)
Time_Reserve <- c(100.617, 101.623, 102.750, 103.617, 104.411, 105.346)

times <- cbind(Time_Driver_1, Time_Driver_2, Time_Reserve)
mu <- mean(times)
mu_R <- mean(Time_Reserve)
alpha_R <- mu_R - mu
mu_1 <- mean(times[2, ])
beta_1 <- mu_1 - mu
c(alpha_R, beta_1)

# VII.2
times <- c(Time_Driver_1, Time_Driver_2, Time_Reserve)
compund <- factor(c(rep(0:5, 3)))
driver <- factor(c(rep(1, 6), rep(2, 6), rep("R", 6)))
fit <- lm(times ~ compund + driver)
anova(fit)

# VII.3
alpha <- 0.05
M <- 3 * (3 - 1) / 2
alpha_bon <- alpha / M
n <- 6
SSE <- 0.188
mu_2 <- mean(Time_Driver_2)
mu_R <- mean(Time_Reserve)
df <- 10
(mu_R - mu_2) + c(-1, 1) * qt(1 - alpha_bon / 2, df) * sqrt(2 * SSE / df / n)

# VIII.1
T <- 19.95
se <- 0.09284
beta_1 <- T * se
beta_1

# IX.1
ME <- 0.01
alpha <- 0.05
n <- 1 / 4 * (qnorm(1 - alpha / 2) / ME)^2
ceiling(n)

# IX.2
alpha <- 0.1
p <- 0.25
n <- 400
n * (p - qnorm(1 - alpha / 2) * sqrt(p * (1 - p) / n))

# X.1
s <- 1620.042
n <- 47
mu <- s / n
mu
53.33333 - 16.66667

# XI.1
table1 <- matrix(c(17, 28, 21, 15, 73, 138, 105, 72), nrow = 2, byrow = TRUE)

p <- 81 / 469
p * 126

# XI.2
chisq.test(table1, correct = FALSE)

# XI.3
table2 <- matrix(c(35, 64, 42, 20, 30, 60, 55, 45, 8, 14, 8, 7), nrow = 3, byrow = TRUE)
chisq.test(table2, correct = FALSE)

chisq.test(table2, correct = FALSE)$expected
