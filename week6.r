A_mean <- 1.93
A_sd <- 0.45
B_mean <- 1.49
B_sd <- 0.58
n <- 9

alpha <- 0.05

t_obs <- (A_mean - B_mean) / sqrt((A_sd**2 / n) + (B_sd**2 / n))
df <- (A_sd**2 / n + B_sd**2 / n)**2 / ((A_sd**2 / n)**2 / (n - 1) + (B_sd**2 / n)**2 / (n - 1))
p_value <- 2 * (pt(-t_obs, df))
p_value

CI <- qt(1 - alpha / 2, df) * sqrt((A_sd**2 / n) + (B_sd**2 / n))

est <- c((A_mean - B_mean) - CI, (A_mean - B_mean) + CI)
est

power.t.test(n = n, delta = 0.4, sd = 0.5, sig.level = alpha)
power.t.test(n = n, power = 0.8, sd = 0.5, sig.level = alpha)
power.t.test(delta = 0.4, power = 0.9, sd = 0.5, sig.level = alpha)

alpha <- 0.01
mean_end <- 179.5
mean_1min <- 125
sd_end <- 5.19
sd_1min <- 8.409
sd_diff <- 5.768
n <- 13
df <- n - 1
CI <- qt(1 - alpha / 2, df) * sqrt(sd_diff**2 / n)

mean_diff <- mean_end - mean_1min
mean_diff + c(-1, 1) * CI

alpha <- 0.05
sqrt((n - 1) * sd_end**2 / qchisq(1 - alpha / 2, n - 1))
sqrt((n - 1) * sd_end**2 / qchisq(alpha / 2, n - 1))


alpha <- 0.05
mean_max <- 2.508
mean_min <- 2.103
sd_max <- 0.3373
sd_min <- 0.2834
sd_diff <- 0.09664
n <- 10
mu_D <- 0.35

mean_diff <- mean_max - mean_min

df <- n - 1
CI <- qt(1 - alpha / 2, df) * sqrt(sd_diff**2 / n)

mean_diff + c(-1, 1) * CI

T <- (mean_diff - mu_D) / sqrt(sd_diff**2 / n)
T
p_value <- 2 * (1 - pt(abs(T), df))
p_value

n_before <- 50
n_after <- 24
f_before <- 13
f_after <- 3
mean_before <- 6.42
mean_after <- 7.375
sd_before <- 2.205
sd_after <- 1.813

mean_diff <- mean_before - mean_after
df <- (sd_before**2 / n_before + sd_after**2 / n_after)**2 / ((sd_before**2 / n_before)**2 / (n_before - 1) + (sd_after**2 / n_after)**2 / (n_after - 1))
t_obs <- mean_diff / sqrt(sd_before**2 / n_before + sd_after**2 / n_after)
p_value <- 2 * (pt(-abs(t_obs), df))
c(t_obs, p_value)


alpha <- 0.01
CI <- qt(1 - alpha / 2, df) * sqrt(sd_before**2 / n_before + sd_after**2 / n_after)
mean_diff + c(-1, 1) * CI

alpha <- 0.05
CI1 <- sqrt((n_after - 1) * sd_after**2 / qchisq(1 - alpha / 2, n_after - 1))
CI2 <- sqrt((n_after - 1) * sd_after**2 / qchisq(alpha / 2, n_after - 1))
c(CI1, CI2)

sd <- 3
alpha <- 0.1
mu <- 2

n <- (qnorm(1 - alpha / 2) * sd / 1)**2
n

alpha <- 0.01
n <- (qnorm(1 - alpha / 2) * sd / 1)**2
n
