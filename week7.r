# 4.1 a
set.seed(82719)
k <- 10000
mu_A <- 2
mu_B <- 3
mu_C <- 5

lam_A <- 1 / mu_A
lam_B <- 1 / mu_B
lam_C <- 1 / mu_C

x_A <- rexp(k, lam_A)
x_B <- rexp(k, lam_B)
x_C <- rexp(k, lam_C)

mean(x_A)
mean(x_B)
mean(x_C)

x <- cbind(x_A, x_B, x_C)

y <- apply(x, 1, min)
hist(y, col = "blue", nclass = 30)

# b
mu_y <- mean(y)
mu_y

# c
sd_y <- sd(y)
sd_y

# d
mean(y <= 1)

# e
median(y)
quantile(y, 0.1)

# 4.2
# a
q0025 <- (33.3818 + 38.3818) / 2
q00975 <- (33.5382 + 38.5391) / 2
c(q0025, q00975)

# b
q005 <- (33.3909 + 38.3918) / 2
q095 <- (33.5218 + 38.5236) / 2
c(q005, q095)

# 4.3
set.seed(6287)
x <- c(
    38.43, 38.43, 38.39, 38.83, 38.45, 38.35,
    38.43, 38.31, 38.32, 38.48, 38.50
)
k <- 1000

# a
simsamples <- replicate(k, sample(x, replace = TRUE))
simmeans <- apply(simsamples, 2, mean)
c(quantile(simmeans, 0.025), quantile(simmeans, 0.975))

# b
mu_x <- mean(x)
sd_x <- sd(x)
n <- length(x)

simsamples <- replicate(k, rnorm(n, mu_x, sd_x))
simmeans <- apply(simsamples, 2, mean)
c(quantile(simmeans, 0.025), quantile(simmeans, 0.975))
t.test(x)

# c
simsamples <- replicate(k, rlnorm(n, mean(log(x)), sd(log(x))))
simmeans <- apply(simsamples, 2, mean)
c(quantile(simmeans, 0.025), quantile(simmeans, 0.975))

# d
simsamples <- replicate(k, rnorm(n, mu_x, sd_x))
sim_q1 <- apply(simsamples, 2, quantile, 0.25)
c(quantile(sim_q1, 0.025), quantile(sim_q1, 0.975))

# e
simsamples <- replicate(k, sample(x, replace = TRUE))
sim_q1 <- apply(simsamples, 2, quantile, 0.25)
c(quantile(sim_q1, 0.025), quantile(sim_q1, 0.975))

# 4.4
TV1 <- c(1, 2, 1, 3, 2, 1, 2, 3, 1, 1)
TV2 <- c(3, 4, 2, 4, 2, 3, 2, 4, 3, 2)

k <- 1000

# a
sim1samples <- replicate(k, sample(TV1, replace = TRUE))
sim2samples <- replicate(k, sample(TV2, replace = TRUE))

simmeandiff <- apply(sim1samples, 2, mean) - apply(sim2samples, 2, mean)
c(quantile(simmeandiff, 0.025), quantile(simmeandiff, 0.975))

# b
mean(TV1) - mean(TV2)
t.test(TV1, TV2)

# c
n <- length(TV1)
sim1samples <- replicate(k, rnorm(n, mean(TV1), sd(TV1)))
sim2samples <- replicate(k, rnorm(n, mean(TV2), sd(TV2)))

simmeandiff <- apply(sim1samples, 2, mean) - apply(sim2samples, 2, mean)
c(quantile(simmeandiff, 0.025), quantile(simmeandiff, 0.975))

# 4.5
# a
P <- 240.48
V <- 9.987
sd_P <- 0.03
sd_V <- 0.002
R <- 8.31
R_inv <- 1 / R

T <- P * V * R_inv
TdP <- T / P
TdV <- T / V

T
varT <- (V^2 * sd_P^2 + P^2 * sd_V^2) * R_inv^2
sdT <- sqrt(varT)
sdT

# b
P <- 240.48
T <- 289.12
sd_P <- 0.03
sd_T <- 0.02
R <- 8.31
R_inv <- 1 / R

V <- R * T / P
VdP <- -V / P
VdT <- V / T


varV <- VdT^2 * sd_T^2 + VdP^2 * sd_P^2
sdV <- sqrt(varV)
sdV

# c
V <- 9.987
T <- 289.12
sd_V <- 0.002
sd_T <- 0.02
R <- 8.31

P <- R * T / V
PdV <- -R * T / V^2
PdT <- R / V

varP <- PdV^2 * sd_V^2 + PdT^2 * sd_T^2
sdP <- sqrt(varP)
sdP

# d
k <- 10000
Vs <- rnorm(k, V, sd_V)
Ts <- rnorm(k, T, sd_T)
Ps <- R * Ts / Vs
mean(Ps)
sd(Ps)
