
# Maj 2023 eksamen

#### Opgave I: Vandtønder ####
#### 1. formel for approks. af sd ####

# sigma_V^2 = (pi * R^2) * sigma_H^2 + (2 * pi * R * H)^2 * sigma_H^2
sigma_V^2 (pi*0.23^2) * sigma_H^2 + (2 * pi * 0.23 * 0.6)^2 * sigma_H^2

# sigma_V = sqrt((pi * R^2) * sigma_H^2 + (2 * pi * R * H)^2 * sigma_H^2)
sigma_V = sqrt((pi*0.23^2) * sigma_H^2 + (2 * pi * 0.23 * 0.6)^2 * sigma_H^2)

#reduceret
sigma_V = sqrt(0.166^2 * sigma_H^2 + 0.867^2 * sigma_H^2)

# svar: 3 = sqrt(0.166^2 * sigma_H^2 + 0.867^2 * sigma_H^2)


#### 2. normalfordelt simulering ####

# volumen = H * pi * Radius^2

volumen = rnorm(n,mean,sd) * pi * rnorm(n,mean,sd)^2

rnorm(1000, 0.6, 0.005) * pi * rnorm(1000, 0.23, 0.003)^2

# svar = 3


#### Opgave II: Radar og missildetektion ####
#### 3. sansynlighed ####

# sansynlighed for en radar er 0.9, så sansynlighed for 2 radarerer er
0.9*0.9
# eller
dbinom (x= 2,size = 2, prob=0.9)

# svar: 0.81




#### Opgave III: Eksamensresultat ####
#### 4. karakter fra standardafvigelse ####

qnorm(c(0.85), mean=55, sd=19)

# svar: 74.69



#### Opgave IV: Købsadfærd ####
#### 5. sansynlighed, hypergeometrisk eksperiment ####

dhyper(x=2, m=7, n=3, k=3) + dhyper(x=3, m=7, n=3, k=3)

# svar: 0.81666



#### Opgave V: Solceller ####
#### 6. Aflæsning af histogram og Q-Q plot ####

# Der er en klar afvigelse fra normalfordelingen, og normalfordelingsantagelsen er dermed
# ikke opfyldt, men da stikprøvestørrelse er over 30 er normalfordelingsantagelsen, ifølge
# den centrale grænsesætning, ikke betydende og dermed er konfidensintervallet gyldigt.


#### 7. formel for t_obs ####

# enkelt sample test kan benyttes

# svar: 5


#### Opgave VI: skridt tæller ####
#### 8. 99% konfidens interval ####
x <- c(8500, 10300, 6800, 10600, 4900, 6200, 10800, 5700, 5100, 9000)

t.test(x, conf.level = 0.99)

# svar: [5399.683 ; 10180.317]

#### 9. nulhypotese og p-værdi ####

t.test(x, mu = 10000)

# p-værdi er 0.01484
# p-værdi er under alpha, og skal forkastes

# svar: nulhypotese forkastes fordi p < alpha

#### 10. power t-test ####

# bestem sd ud fra n, sig. niveau, styrke og gennemsnit

power.t.test(n=14, delta=1000, sd=NULL, sig.level=0.01, power=0.8)

# svar: sd = 724.2351


#### Opgave VII: multipel lineær regression ####
#### 11. aflæsning af summary ####

# vi er givet: 
##      Estimate   Std. Error   t value     Pr(>|t|)
## x1   1.2933    0.3481        3.715       0.00231

#1: Nej, Std. error viser sigma_beta_1 (?)
#2: Nej, Std. error (?)
#3: Nej t-værdi (?)
#4: Ja, t-værdi kan benyttes til vurdering af sammenhæng mellem x1 og y
#5: Nej, både std. error og t-værdi har med usikkerhed at gøre. 

#### 12. aflæsning af Q-Q plots ####

# altid resudialerne!!



#### Opgave VII: lineær regression ####
#### 13. estimat fra summary ####

year <- 1990:2015
DO <- c(1.52, 2.88, 1.60, 2.24, 2.45, 1.84, 2.03, 2.33, 2.81,
        2.46, 2.36, 2.23, 2.81, 2.70, 2.63, 2.00, 2.40, 2.45,
        2.48, 2.51, 2.55, 2.77, 2.70, 2.23, 2.88, 3.09)

summary(lm(DO ~ year))

# estimat efter 5 år
0.027634*5

# svar: 0.14

#### 14. kritiske værdier på 1% ####

qt (0.995, df = 24)

# kritiske værdier er 2.8
# t_obs findes i summary fra tidligere opgave, og er 3.163

# svar: kritiske værdier er +-2.8, og nulhypotese afvises fordi t_obs > 2.8

#### 15. prædiktions interval ####

fit = lm(DO ~ year)
predict(fit, newdata = data.frame(year=2022), interval="prediction", level=0.95)

# prædiktionsinterval er [2.17 ; 3.75]

# passer fint med modellen


#### Opgave IX: sansynlighed ####
#### 16. eksponentiel sansynlighedsfordeling ####

pexp(2, rate =1/2) - pexp(1, rate =1/2)

# svar: 0.2386


#### Opgave X: simulering og bootstrapping ####
#### 17. udfyld kode ####

heights <- c(162, 172, 178, 154, 173, 174, 166, 166,
             166, 164, 167, 163, 165, 170, 177)
set.seed(1234)
k <- 1000
sim_samples <- replicate(k, rnorm(15, mean = mean(heights), sd = sd(heights)))
sim_stats <- apply(sim_samples, 2, median)
quantile(sim_stats, c(0.025, 0.975))

# svar: [164.29 ; 171.64]

#### 18. log normalfordeling ####

#alpha = 0 og beta = 1

# runif(10) for uniform fordeling

qlnorm(runif(10))

#### 19. ikke parametrisk bootstrap ####

# skal inkludere denen kode
replicate(10000, sample(x, replace = TRUE))



#### Ogpave XI: ANOVA ####
#### 20. antal observationer i ANOVA ####

# frihedsgrader er antal grupper -1, hvilket er k=4
# men fordi vi har 2 metoder, giver dette i alt n=8 observationer

# svar: 8

#### 21. bon ferroni ####

# M = k(k-1)/2
# bon ferroni = alpha / M

4*(4-1)/2 # giver 6
bon_ferroni = 0.005/6

# svar : 0.05/6


#### Opgave XII: grupperet data ####
#### 22. total variation SST ####

consumption <- c(5.4, 4.5, 4.6, 4.4, 4.9, 3.3, 4.1, 4.6, 4.8, 4.6,
                 5.2, 4.7, 4.4, 4.8, 4.8, 5.2, 4.9, 4.8, 5.6, 5.5)
month <- c(rep("Feb",5), rep("May",5), rep("Aug", 5), rep("Nov", 5))

# bestem totale variation

var(consumption)*(length(consumption)-1)
# eller
SST <- sum((consumption - mean(consumption))^2)
SST

#### 23. nulhypotese for grupperet data (ANOVA) ####

anova(lm(consumption ~ month))

# ser at p-værdi er 0.03029
# forkaster nulhypotese fordi p-værdi < alpha



#### Opgave XIII: Poission fordeling #### ¨
#### 24. varians af poisson ####

# varians af poisson er det samme som gennemsnittet
# så det er bare 48


#### 25. 1-P(x<1) ####

# bestemmer frekvensen af opkald hver 5. minut
48/60*5 # giver 4

# skal finde chance for 2 eller flere opkald venter
P(x>2) = 1-P(x<1)
1 - ppois(1,lambda=4)

# svar = 0.9084


#### Opgave XIV: proportioner ####
#### 26. 95% konfidensinterval fra tabel ####

# for ledige som har svaret meget svært
n <- 100 # antal totale svar
p <- 24/n # meget svært delt med totalt antal svar
p +-  qnorm(0.975) * sqrt(p * (1 - p) / n)

# svar:
0.24 + - 1.96 * sqrt(0.1824/100)

#selve konfidensintervallet er
p + c(-1, 1) * sqrt(p * (1 - p) / n) * qnorm(0.975)


#### 27. 95% konfidensinterval fra tabel ####

# for beskæftigede som har svaret meget svært
n <- 101 # antal totale svar
p <- 5/n # meget svært delt med totalt antal svar

n*p # giver 5, så er under 15

# bruger plus-2 approach, hvor man siger 
n <- 101+4 # tilføjer 4 til n
p <- (5+2)/n # tilføjer 2 til x

p +-  qnorm(0.975) * sqrt(p * (1 - p) / n)

# svar:
0.066666 +- 1.96 * sqrt(0.062222/105)


#### 28. signifikant forskel, 5% signifikansniveau ####

# vi skal lave chi^2 test

# laver en matrix med 5 rows og 2 columns, ligesom i tabellen
obs2021 <- matrix(c(10,15,27,24,24,24,32,28,12,5), nrow=5)

chisq.test(obs2021)

# svar: der er signifikant forskel fordi p-værdi på 1.04 e-5

#### 29. forventet værdi ####

# forventet antal fra ledig som svarer "nemt" i 2019

(sum19 * sumNemt)/n

(101*82)/503

# svar: 16.46521


#### 30. bestem p-værdi for ændring over tid ####

# finder p-værdi for chi^3 fordeling

# 1 - pchisq(q,df)

1 - pchisq(4.5303, df=16) = 0.9976





