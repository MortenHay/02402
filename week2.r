# Exercise 2.1
# a
dbinom(4, 10, 0.6)
# This is the probablity of getting exactly four successes in 10 trials with a probability of success of 60%

# b
F4 <- 0.1662
F5 <- 0.3669

Pleq5 <- F5
Pl5 <- F4
Pg4 <- 1 - F4
Peq5 <- F5 - F4
c(Pleq5, Pl5, Pg4, Peq5)


# c
dpois(4, 3)
# Probability of getting exactly 4 occurances in on period with a mean of 3 occurances

# d
F4 <- ppois(4, 3)
F5 <- ppois(5, 3)

Pleq5 <- F5
Pl5 <- F4
Pg4 <- 1 - F4
Peq5 <- F5 - F4
c(Pleq5, Pl5, Pg4, Peq5)

# Exercise 2.4
dhyper(0, 6, 14, 3)

# Exercise 2.5
n <- 3 # number of trials
a <- 2 # number of successes
N <- 20 # number of items

u <- n * a / N
sigma2 <- n * a * (N - a) * (N - n) / (N^2 * (N - 1))
Pgeq1 <- 1 - dhyper(0, a, N - a, n)
c(u, sigma2, Pgeq1)
