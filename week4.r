mu <- 3000
x <- c(3003, 3005, 2997, 3006, 2999, 2998, 3007, 3005, 3001)

x_mean <- mean(x)
x_sd <- sd(x)

SEM <- x_sd / sqrt(length(x))

c(x_mean, x_sd, SEM)

sig <- 3

mu5 <- 5 * mu
V5 <- 5 * (sig**2)
V5

Z <- 2 * (1 - pnorm(mu5 + 10, mu5, sqrt(V5)))
Z
