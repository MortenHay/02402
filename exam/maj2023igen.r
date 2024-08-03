# I.1
VR = 2*0.6*pi*0.23
VH = pi*0.23^2
c(VH,VR)

# II.1
dbinom(2,2,0.9)

# III.1
mu = 55
sd = 19
qnorm(0.85,mu,sd)

# IV.1
phyper(1,7,3,3,lower.tail = FALSE)

# VI.1
x <- c(8500, 10300, 6800, 10600, 4900, 6200, 10800, 5700, 5100, 9000)
t.test(x,conf.level = 0.99)

# VI.2
t.test(x,mu=10000)

# VI.3
power.t.test(n=14,delta=1000,sd=NULL,sig.level=0.01,power=0.8)

# VIII.1
year <- 1990:2015
DO <- c(1.52, 2.88, 1.60, 2.24, 2.45, 1.84, 2.03, 2.33, 2.81,
2.46, 2.36, 2.23, 2.81, 2.70, 2.63, 2.00, 2.40, 2.45,
2.48, 2.51, 2.55, 2.77, 2.70, 2.23, 2.88, 3.09)
summary(lm(DO~year))
5*0.027634

# VIII.2
alpha = 0.01
qt(1-alpha/2,24)

# VIII.3
predict(lm(DO~year),newdata=data.frame(year=2022),interval="confidence")
predict(lm(DO~year),newdata=data.frame(year=2022),interval="predict")

# IX.1
rate = 1/2
pexp(2,rate) - pexp(1,rate)

# X.1
heights <- c(162, 172, 178, 154, 173, 174, 166, 166,
166, 164, 167, 163, 165, 170, 177)
set.seed(1234)
k <- 1000
sim_samples <- replicate(k, rnorm(length(heights),mean(heights),sd(heights)))
sim_stats <- apply(sim_samples, 2, median)
quantile(sim_stats, c(0.025, 0.975))

# XI.1
alpha = 0.05
k = 4
M = k*(k-1)/2
alpha_bon = alpha/M
M

# XII.1
consumption <- c(5.4, 4.5, 4.6, 4.4, 4.9, 3.3, 4.1, 4.6, 4.8, 4.6,
5.2, 4.7, 4.4, 4.8, 4.8, 5.2, 4.9, 4.8, 5.6, 5.5)
month <- c(rep("Feb",5), rep("May",5), rep("Aug", 5), rep("Nov", 5))
anova(lm(consumption~month))
SST = 2.1215 + 2.9480
SST

# XIII.2
lam = 5*48/(60)
ppois(1,lam,lower.tail = FALSE)

# XIV.1
p = 24/100
p*(1-p)

# XIV.2
x = 5
n = 101
p = x/n
n*p
x0 = x+2
n0 = n+4
p0 = x0/n0
c(p0,p0*(1-p0),n0)

# XIV.3
D = matrix(c(10,15,27,24,24,24,32,28,12,5),ncol=2)
chisq.test(D)

# XIV.4
e23 = 82*101/503
e23
