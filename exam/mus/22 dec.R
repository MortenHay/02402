
# December 2022 eksamen

#### Opgave I: uafhængige stokastiske variable ####
#### 1. gennemsnit af linær kombination ####

#theorem 2.56
# E(2X+Y) = E(2X)+E(Y) = 2X+Y

2*2+(-1)

# svar: 3


#### Opgave II: Lineær regression ####
#### 2. variationskoefficient ####
y <- c(59, 52, 42, 59, 24, 24, 40, 32, 63, 55, 34, 24)
x <- c(298, 303, 270, 287, 236, 245, 265, 233, 286, 290, 264, 239)

summary(lm(x~y))

# svar: multiple R squared = 0.8334

#### 3. normalitetsantagelse ####
fit <- lm(y~x)

# normalitetsantagelsen skal undersøges med qq-plot af residualerne

# svar:
qqnorm(fit$residuals)
qqline(fit£residuals)


#### 4. aflæs konfidensinterval MANGLER !!!! ####

# estimate for x +-  * std. error for x


# svar: 




#### Opgave III: Summary ####
#### 5. nulhypotese ####

# Vi ser at konfidensintervallet er [6.17 ; 6.75], hvilket betyder at middeleffekten må være her imellem
# effektiviteten er altså større end 6km/kwh

# svar: den er mere effektiv end 6km/kWh, fordi 6 < 6.17


#### 6. 99% konfidensinterval ####

# gns +- t_0.995 * s/sqrt(n)
6.461533+c(-1,1) * qt(0.995,df=34) * 6.461533/45.117

# svar: [6.07 ; 6.85]

#### 7. konfidensinterval udfra gns og varians ####

m <-  1.857769
v <-  0.01643549

m+c(-1,1) * qt(0.975,df=34) * sqrt(v)/sqrt(35)

# svar: [1.81 ; 1.90]

#### 8. signifikant forskel kode ####

t.test(log(range1), log(range2), paired = TRUE)


#### 9. stikprøvestørrelse fra fejlmargin ####

# n = (1.96 * standardafvigelse / ønsket fejlmargin)^2

(1.96 * 0.8 / 0.1)^2

# svar: 246



#### Opgave IV: ANOVA ####
#### 10. variations SST ####

# Sætning 8.2: SST = SS(Tr) + SSE
# SST = sum sq for variation mellem grupper + sum sq for variation inden for grupperne

14.163 + 20.305

# svar: 34.468

#### 11. aflæsning af ANOVA tabel ####

# p-værdi er 0.02061

# svar: På signifikansniveau 5% er der påvist en signifikant forskel i vægt som følge af dyrknings-
# forholdene, da 0.02061 < 0.05.




#### Opgave V: sansynlighed ####
#### 12. sansynlighed pbinom ####

1 - pbinom(q = 1, size = 6, prob = 0.35)

# svar: 0.681


#### 13. varians for binomial random variabel####

# varians = np(1-p)

6*0.35 * (1-0.35)

# svar: 1.37

#### 14. fordeling per interval ####

ppois(0, lambda = 20/15)
# eller
1 - pexp(20, rate = 1/15)

# svar: 26.4% 

#### 15. eksponentiel fordeling ####

qexp(0.90, rate = 1/15)

#svar: 34.5 minutter


#### Opgave VI: simulering ####
#### 16. tilfældige udfald kode ####

# runif(n, min, max)

# svar: runif(50, 0, 100)


#### Opgave VII: multipel lineær regression ####
#### 17. p-værdi ####

# P(T>x)=2(1-pt(x,n-1))

# her er x t-værdi for x2

2*(1-pt(0.371, 10))

# svar: 0.7184

#### 18. antal observationer ####

# der er 10 frihedsgrader, som er givet ved n-3 fordi der er 3 parametre

# svar: 13

#### 19. konklusion fra summary ####

# svar: p-værdien for vægt (x1) er mindre end 0.05, derfor er der en signifikant 
# sammenhæng mellem blodtryk og vægt.



#### Ogpave VIII: tovejs ANOVA ####
#### 20. aflæsning af ANOVA ####

# svar: Der er ingen signifikant effekt af Placering, men der er signifikant effekt af 
# Behandling, da de relevante p-værdier er henholdsvis 17.8% og 0.95%.

#### 21. udsagn om model ####

#1: Nej. 
#2: Nej.
#3: Nej, 
#4: Ja, det er dens prædiktioner
#5: Nej, 

#### 22. post hoc sammenligninger ####

k <- 5
k*(k-1)/2

# hvad?????



#### Opgave IX: ELISA test ####
#### 23. 95% konfidensinterval for standardafvigelse, ikke parametrisk ####

# 95% konfidensinterval for standardafvigelsen af målt glutenindhold ved ikke-parametrisk bootstrap?

simsamples <- replicate(10000, sample(glutenA, replace = TRUE))
simmeans <- apply(simsamples, 2, sd)
quantile(simmeans, c(0.025, 0.975))

#### 24. 95% konfidensinterval for standardafvigelse, parametrisk ####

# 95% konfidensinterval for standardafvigelsen af målt glutenindhold ved parametrisk bootstrap?

simsamples <- replicate(10000, rnorm(10,mean(glutenA),sd(glutenA)))
simsds <- apply(simsamples, 2, sd)
quantile(simsds, c(0.025, 0.975))

#### 25. 95% konfidensinterval for forskel i sd, parametrisk ####

# 95% parametrisk konfidensinterval for forskellen i standardafvigelserne mellem de to sæt målinger?

simAsamples <- replicate(10000, rnorm(10,mean(glutenA),sd(glutenA)))
simBsamples <- replicate(10000, rnorm(10,mean(glutenB),sd(glutenB)))
simDifsds <- apply(simAsamples,2,sd) - apply(simBsamples,2,sd)
quantile(simDifsds, c(0.025, 0.975))

#### 26. Udsagn om konfidensinterval ####

# [-4.43 ; 1.53]

# konfidens interval indeholder 0, så vi ved ikke om sd er ens
#vi ved ikke om en er bedre end den anden

# svar: 5. Eftersom konfidensintervallet indeholder 0, kan det ikke afvises at standardafvigelserne
# er ens. Vi kan derfor ikke konkludere at den ene mikser er bedre end den anden.




#### Opgave X: Grupperet data ####
#### 27. 95% konfidensinterval fra tabel ####

n <- 1268 # antal totale svar
p <- 852/n # kvinder delt med totalt antal svar
p + c(-1, 1) * sqrt(p * (1 - p) / n) * qnorm(0.975)

# svar: [0.646 ; 0.698]

#### 28. 95% konfidensinterval forskel ####

p1 <- 746/852
p2 <- 339/416
p1-p2 + c(-1,1) * sqrt(p1*(1-p1)/852+p2*(1-p2)/416)*qnorm(0.975)

# svar: [0.017; 0.104]

#### 29. forventet antal under nulhypotese ####

# enige i alt * (mænd i alt / deltagere i alt)

1085 * (416 / 1268)

# svar: 355.96

#### 30. kritisk værdi i nulhypotese ####

qchisq(0.95, 2)

# svar: 5.991




