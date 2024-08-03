# Q II.3
DFTr <- 9
DFE <- 50
SSTr <- 207
SST <- 707
pval <- 0.03
k <- DFTr + 1
n <- DFE + k
DFT <- n - 1
MSTr <- SSTr / DFTr
SSE <- SST - SSTr
MSE <- SSE / DFE
Fobs <- MSTr / MSE
A <- DFT
B <- SSE
C <- MSE
D <- MSTr
E <- Fobs
c(A, B, C, D, E)

# QII.2
alpha <- 0.05
m <- n / k
LSD <- qt(1 - alpha / 2, df = n - k) * sqrt(2 * MSE / m)
LSD


# QIII.1
tobs <- 160.53
df <- 233
pval <- 0
x_mean <- 20.22413

n <- df + 1
n

# QIII.2
h0 <- 20 # Gennemsnittet der testes
standardError <- x_mean / tobs # Udregn standardafvigelsen fra gennemsnittet
# Udregn ny p-val med målt gennemsnits afstand til ønsket gennemsnit
# over standardafvigelse i t-testen
pval20 <- 2 * (1 - pt((x_mean - h0) / standardError, df = df))
pval20

# QIII.3
df <- 53.627
pval <- 0.9793
alpha <- 0.05
x_mean <- 19.10497
y_mean <- 19.11686

# No significant difference
pval > alpha

# QIII.4
# same df but new alpha. All else is same
alpha <- 0.01
c(qt(alpha / 2, df = df), qt(1 - alpha / 2, df = df))

# QIV.1
p <- 4
df <- 56
n <- df + (p + 1)
n

# QV.1
pe <- 0.05
1 - pbinom(9, size = 100, prob = pe)

# QV.2
rate <- 200
limit <- 250
ppois(limit, rate)

# QV.3
# Se facit

# QVI.1
lamX <- 1.2
lamY <- 1.7
n <- 1000000
sim <- rexp(n, lamX) + rexp(n, lamY)
sum(sim > 3) / n
# alt
mean(sim > 3)

# QVII.1

# Indsæt data
Time_Driver_1 <- c(100.391, 101.506, 102.241, 103.058, 103.766, 104.801)
Time_Driver_2 <- c(100.495, 101.455, 102.468, 103.350, 104.230, 105.391)
Time_Reserve <- c(100.617, 101.623, 102.750, 103.617, 104.411, 105.346)

# Definer matrix rækker
Driver <- as.factor(c(rep(1, 6), rep(2, 6), rep("R", 6)))
# Definer matrix søjler
Compound <- as.factor(rep(0:5, 3))
# Definer matrix med alle tider
Time <- c(Time_Driver_1, Time_Driver_2, Time_Reserve)

# Gennemsnit af alle kørsler
mu <- mean(Time)
# Alpha er afvigelsen af hver kørers gnms fra samlet gmns
alpha <- tapply(Time, Driver, mean) - mu
# Beta er afvigelsen af hver compunds gnms fra samlet gmns
beta <- tapply(Time, Compound, mean) - mu

# Udskriv
alpha
beta

# QVII.2
sig <- 0.05
fit <- lm(Time ~ Driver + Compound)
anova(fit)

# QVII.3
df <- 10
SSE <- 0.188
MSE <- 0.0188

mus <- tapply(Time, Driver, mean)
mus
alpha_bonferoni <- sig / 3
mus[3] - mus[2] + c(-1, 1) * qt(1 - alpha_bonferoni / 2, df = df) * sqrt(2 * MSE / 6)

# QVIII.1
t_val <- 19.95
se <- 0.09284
estm <- t_val * se
estm

# QVIII.2
df <- 28
alpha <- 0.01
beta0 <- -0.43369
se0 <- 0.49844
CI <- beta0 + c(-1, 1) * qt(1 - alpha / 2, df = df) * se0
CI
CI[2] - CI[1]

CIw <- 2 * qt(1 - alpha / 2, df = df) * se0
CIw

# QIX.1
alpha <- 0.05
ME <- 0.01
p <- 1 / 2 # worst case
n <- p * (1 - p) * (qnorm(1 - alpha / 2) / ME)^2
ceiling(n)

# QIX.2
alpha <- 0.1
p <- 0.25
n <- 400
pw <- p - qnorm(1 - alpha / 2) * sqrt(p * (1 - p) / n)
c(pw, pw * n)

# QX.1
53.333 - 16.667

# QXI.1
table1 <- matrix(c(17, 28, 21, 15, 73, 138, 105, 72), nrow = 2, byrow = TRUE)
n <- sum(table1)
x <- sum(table1[1, ])
p <- x / n
E3 <- p * sum(table1[, 3])
E3

# QXI.2
chisq.test(table1, correct = FALSE)

# QXI.3
table2 <- matrix(c(35, 64, 42, 20, 30, 60, 55, 45, 8, 14, 8, 7), nrow = 3, byrow = TRUE)
chi <- chisq.test(table2, correct = FALSE)
chi$expected
chi
