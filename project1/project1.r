## Code for statistics project 1 by s223872



library(xtable)

setwd("/home/mort/Documents/Stat/project1/soenderborg1")

D <- read.csv("soenderborg1_data.csv", header = TRUE, sep = ";", as.is = TRUE)
D$t <- as.Date(x = D$t, format = "%Y-%m-%d")
## t = Date, Ta = Ambient Temperature, G = Insolation, Ws = Wind speed, Q1:Q4 = House energy consumptions
xtable(summary(D))
summary(D)

# make the histogram
hist(D$Q1,
    main = "Histogram of house 1 heat consumption",
    xlab = "Heat consumption (house 1)",
    xlim = c(0, 7), ylim = c(0, 0.65), col = "#3f3737", prob = TRUE
)

## Plot varmeforbruget over tid
plot(D$t, D$Q1,
    type = "l", xlim = as.Date(c("2008-10-02", "2010-10-01")),
    ylim = c(0, 9), xlab = "Date", ylab = "Heat consumption", col = 2
)
lines(D$t, D$Q2, col = 3)
lines(D$t, D$Q3, col = 4)
lines(D$t, D$Q4, col = 5)
## Tilføj legend
legend("topright", legend = paste0("Q", c(1, 2, 3, 4)), lty = 1, col = 2:5)

Dsel <- subset(D, "2010-01-01" <= t & t < "2010-3-01")
boxplot(Dsel[, c("Q1", "Q2", "Q3", "Q4")],
    xlab = "House",
    ylab = "Heat consumption", col = 2:5, ylim = c(0, 7), main = "Boxplot of heat consumption in 2010", names = c("Q1", "Q2", "Q3", "Q4")
)


D_summary <- apply(Dsel[, c("Q1", "Q2", "Q3", "Q4")], 2, function(x) {
    c(
        Observations = sum(!is.na(x)),
        Mean = mean(x, na.rm = TRUE),
        Varians = var(x, na.rm = TRUE),
        "Standard Deviation" = sd(x, na.rm = TRUE),
        "First Quartile" = unname(quantile(x, 0.25, na.rm = TRUE)),
        Median = median(x, na.rm = TRUE),
        "Third Quartile" = unname(quantile(x, 0.75, na.rm = TRUE))
    )
})
D_summary <- t(D_summary)
xtable(D_summary, digits = 2)

D_summary

par(mfrow = c(2, 2))

for (i in 1:4) {
    qqnorm(Dsel[, i + 3], main = paste("Q", i, "Normal Q-Q plot"))
    qqline(Dsel[, i + 3])
}


CI_table <- matrix(NA, nrow = 4, ncol = 2)
for (i in 1:4) {
    CI_table[i, ] <- t.test(Dsel[, paste0("Q", i)], conf.level = 0.95)$conf.int[1:2]
}

rownames(CI_table) <- paste("House", 1:4)
colnames(CI_table) <- c("Lower", "Upper")
xtable(CI_table, digits = 5)

CI_table
mean(Dsel$Q1, na.rm = TRUE) + c(-1, 1) * qt(0.975, df = sum(!is.na(Dsel$Q1)) - 1) * sd(Dsel$Q1, na.rm = TRUE) / sqrt(sum(!is.na(Dsel$Q1)))

alpha <- 0.05
mu0 <- 2.38
n <- sum(!is.na(Dsel$Q1))
mu <- mean(Dsel$Q1, na.rm = TRUE)
mu
sd <- sd(Dsel$Q1, na.rm = TRUE)
tobs <- (mu - mu0) / (sd / sqrt(n))
tobs
pvalue <- 2 * (1 - pt(abs(tobs), df = sum(!is.na(Dsel$Q1)) - 1))
pvalue
pt(abs(tobs), df = n - 1)

t.test(Dsel$Q1, mu = mu0)

abs(tobs)

mu1 <- mean(Dsel$Q1, na.rm = TRUE)
mu2 <- mean(Dsel$Q2, na.rm = TRUE)
sd1 <- sd(Dsel$Q1, na.rm = TRUE)
sd2 <- sd(Dsel$Q2, na.rm = TRUE)
n1 <- sum(!is.na(Dsel$Q1))
n2 <- sum(!is.na(Dsel$Q2))
tobs <- (mu1 - mu2) / sqrt(sd1^2 / n1 + sd2^2 / n2)
tobs

df <- (sd1^2 / n1 + sd2^2 / n2)^2 / ((sd1^2 / n1)^2 / (n1 - 1) + (sd2^2 / n2)^2 / (n2 - 1))
df

pvalue <- 2 * (1 - pt(abs(tobs), df))
pvalue
t.test(Dsel$Q1, Dsel$Q2)


sx <- sd(D$Q1, na.rm = TRUE)
sy <- sd(D$G, na.rm = TRUE)
n <- sum(!is.na(D$Q1))
sxy <- sum((D$Q1 - mean(D$Q1, na.rm = TRUE)) * (D$G - mean(D$G, na.rm = TRUE)), na.rm = TRUE) / (n - 1)

r <- sxy / (sx * sy)
r


D_cor <- subset(D, complete.cases(D$Q1, D$G))
sx <- sd(D_cor$Q1, na.rm = TRUE)
sy <- sd(D_cor$G, na.rm = TRUE)
n <- sum(!is.na(D_cor$Q1))
sxy <- sum((D_cor$Q1 - mean(D_cor$Q1, na.rm = TRUE)) * (D_cor$G - mean(D_cor$G, na.rm = TRUE)), na.rm = TRUE) / (n - 1)

sxy
r <- sxy / (sx * sy)
r

cor(D[, c("Q1", "G")], use = "pairwise.complete.obs")
par(mfrow = c(1, 1))
plot(D_cor$G, D_cor$Q1, xlab = "Insolation", ylab = "Heat consumption", col = 2, main = "Scatterplot of heat consumption and insolation")
