
# Juni 2023 eksamen

#### Opgave I: ensidet variansanalyse ANOVA ####
#### 1. opstil ANOVA test og undersøg nulhypotese ####
group1 <- c(27, 22, 18, 26, 24)
group2 <- c(32, 22, 32, 25, 25)
group3 <- c(29, 25, 30, 30, 24)

# grupperer data
data <- data.frame(group = factor(rep(1:3, each = 5)),
                   weight_gain = c(group1, group2, group3))

# opstiller model
model <- lm(weight_gain ~ group, data = data)

# laver anova
anova(model)

# svar: p-værdi er 0.1879 så ingen signifikant forskel på grupperne. 

#### 2. manglende data i ANOVA ####

# Mangler frihedsgrader df for residualerne
n (antal observationer) - k(antal grupper)
15-3 # giver 12

# Mangler Mean Sq for residualerne
Sum sq(residuals)/df(residuals)
440.80/12 #36.7333

# svar: df = 12 og mean sq(residuals) = 36.7333



#### Opgave II: Tæthedsfunktion af stokastisk variabel ####
#### 3. middelværdi og varians ####

# Givet funktion
Y1 = a1 + b1 + X + b2*X

#Middelværdi (vi husker at E(X)=my)
E(Y1) = E(a1 + b1*X + b2*X)
E(Y1) = E(a1) + E(b1*X + b2*X)
E(Y1) = E(a1) + E((b1 + b2)*X)
E(Y1) = E(a1) + (b1 + b2)*E(X)
E(Y1) = a1 + (b1 + b2)*my

# Varians (vi husker at V(X) = sigma^2)
V(Y1) = V(a1 + b1*X + b2*X)
V(Y1) = V(a1) + V(b1*X + b2*X)
V(Y1) = V(a1) + V((b1 + b2)*X)
V(Y1) = V(a1) + (b1 + b2)^2 * V(X)
V(Y1) = (b1 + b2)^2 * 4


#### 4. varians af non-lineær funktion ####

# Benyt error propagation method 4.3

# Vi ser at funktionen afhænger af x, hvor ved x=0 og x=20 er funktionen flad
# men den stiger ved x=10, hvilket må betyde at variansen ved x=10 er meget større

# svar: V(x=0) = V(x=20) < V(x=10)



#### Opgave III: Analyse med to stikprøver ####
#### 5. analyse af to parrede stikprøver ####

# Der skal benyttes parret t-test
# ville se således ud i R
dif <- x1-x2
t.test(dif)

# svar: parret t-test

#### 6. 90% konfidensinterval ####

# Hvilken fraktil fra t-fordelingen og hvor mange frihedsgrader?

# Fraktil er t = 1-alpha/2
1-(0.1/2)

# Frihedsgrader bestemmes ved sætning 3.50 med Welch two sample stat
# se maple ark

# svar: 95% fraktil og df=323.9



#### Opgave IV: Inferens for andele ####
#### 7. 95% konfidensinterval for én andel ####

# Metode 7.3 proportions estimat og konfidens interval

x <- 709 # Overlevende
n <- 2201 # observationer i alt

phat <- x/n
phat + c(-1, 1) * qnorm(0.975) * sqrt(phat * (1 - phat) / n)

# Kan også gøres ved følgende:
prop.test(x,n,correct=FALSE)

# svar: [0.30 ; 0.34]

#### 8. Hypotesetest for to andele (testsørrelse og p-værdi) ####

# Metode 7.18 til at bestemme teststørrelsen

# 1 er passagerer og 2 er besætning
x1 <- 178
n1 <-706
phat1 <- x1/n1

x2 <- 212
n2 <-885
phat1 <- x2/n2

phat <- (x1+x2) / (n1+n2)

z = (phat1-phat2) / (sqrt((phat*(1-phat)) * ((1/n1) + (1/n2))))
z

# teststørrelsen er ingen af mulighederne

# Bestemmer p-værdi
prop.test(c(x1,x2), c(n1,n2), correct=FALSE)


# svar: Nej, da p-værdien for den relevante test er 0.56

#### 9. Hypotesetest for flere andele: teststørrelse og kritisk værdi ####

# Hypotese om at overlevelse er den samme for alle grupper

# Bestemmer teststørrelse med metode 7.20
x <- c(202, 117, 178, 212) # antal overlevende
n <- c(325, 285, 706, 885) # antal observationer

samlet <- matrix(c(x,n-x), ncol=2)

chisq.test(samlet)

# Bestemmer kritisk værdi
qchisq(0.95,df=3)

# svar: q = 187.11 og CV = 7.814728, der er signifikant forskel fordi q har stor værdi


#### 10. Hypotesetest for to andele andel (konfidensinterval) ####

x1 <- 202 # overlevende 1. klasse
n1 <- 325 # observationer 1. klasse

x2 <- sum(c(117, 178, 212)) # antal overlevende resten
n2 <- sum(c(285, 706, 885)) # antal observationer resten

# hypotesetest for én andel
prop.test(c(x1,x2),c(n1,n2),correct=FALSE)

# konfidensinterval er [0.29 ; 0.41]

# svar: konfidensinterval er [0.29 ; 0.41] og overlevelses raten for 1. klasse
# er mindst 20% større end resten



#### Opgave V: Sansynlighed ####
#### 11. Uafhængigheds antagelse ####

# standardafvigelse
sqrt(n)*standardafvigelse

sqrt(20)*0.2

# svar: 0.8944272

#### 12. hyper geometrisk fordeling ####

# hypergeometrisk fordeling, benytter dhyper(x,m,n,k)
# x = antal som der er sansynlighed for er en success = 3
# m = antal successer = 6
# n = observationer-antal successer = 21-6 = 15
# k = antal træk = 5

dhyper(3,6,15,5) 

# svar: 0.10


#### 13. binom fordeling ####

# binom fordeling, dbinom(x, size, prob)
# x = antal som der er sansynlighed for er success = 3
# size = population størrelse = 18
# prob = chance for success = 0.32

dbinom(3,18,0.32)

# svar: 0.082



#### Opgave VI: konfidensinterval og hypotesetest ####
#### 14. Konfidensinterval for standardafvigeles ####

#Benytter metode 3.19 for konf. int. for standardafvigelse
n <- 26
s <- 0.75

# Bestemmer chi^2 med v=n-1 firhedsgrader
chi1 <- qchisq(1-0.05/2,n-1)
chi2 <- qchisq(0.05/2,n-1)


# Bestemmer konfidensinterval
sqrt((n-1)*s^2/chi1)
sqrt((n-1)*s^2/chi2)
     
# svar: [0.588 ; 1.035]


#### 15. Hypotesetest med én stikprøve #####

#Metode 3.23 for at bestemme teststørrelsen t_obs = (gns-h0) / (s/sqrt(n))
gns <- 200.3
h0 <-200
n <- 26
s <- 0.75

(gns-h0) / (s/sqrt(n)) # giver 2.039608

# Bestemmer nu den kritisk værdi
qt(0.975,n-1) # giver 2.059539
qt(0.95,n-1) # giver 1.708141

# skal forkastes hvis t_obs > p-værdi

# svar: Nulhypotesen forkastes på et 10% signifikansniveau, da teststørrelsen er 
# større end t_0.95(25)

#### 16. Stikprøvestørrelse #####

# Bruger metode 3.65 n = (sd * (z1-beta + z1 - alpha/2) / h0-h1 )^2
(0.75 * (1.645 + 1.96) / 0.3 )^2 # giver 81.22, så minimum 82

# R bruger normalfordelings approksimation
power.t.test(delta=0.3,sd=0.75,sig.level=0.05,power=0.95,type="one.sample")

# svar: 82 eller 84




#### Opgave VII: Parameter estimation #####
#### 17. antal observationer #####

# vi ved at df=n-(p+1) hvilket betyder at n=df+(p+1)
165+3

# svar: 168

#### 18. rækkefølge af p-værdier #####

# Vi bestemmer test størrelsen

estimates <- c(6.4,0.4,0.2)
stdError <- c(0.014,0.02,0.02)

estimates/stdError

# svar: jo højere teststørrelse, jo lavere p-værdi, så det er pv1 < pv2 < pv3

#### 19. Aflæsning af plots #####

#1) Figur A er et histogram af dataen, ikke residualerne. 
#2) Figur C does not hold information on independence, hence the statement is false.
#3) Figur A A er et histogram af dataen, ikke residualerne, kan ik bruges til normalitetsantagelser
#4) Figur B viser ikke noget om normalfordelings antagelsen
#5) Der er klare systematiske effekter i residualerne som en funktion af tid, så JA

# svar: D er sand. 

#### 20. Bestem beta_0 ud fra varians #####

# vi er givet variansen af den naturlige logaritme i starten s=0.137
# s/sqrt(n)
0.137 / sqrt(168)

# svar: 0.01056978


#### Opgave VIII: Lineær regression #####
#### 21. konfidensinterval #####
wind <- c(1063, 1450, 879, 1980, 406, 1542, 1212,
          1157, 1730, 1105, 775, 856, 802, 851)
elpris <- c(26.84, 24.87, 21.65, 13.26, 24.49, 21.90, 23.29,
            22.47, 19.26, 27.86, 27.96, 20.85, 21.83, 34.04)

fit <- lm(elpris~wind)
confint(fit, level=0.99)

# svar: [-0.0148 ; 0.0017]

#### 22. prædiktionsinterval #####

predict(fit, newdata=data.frame(wind = 1000), interval = "prediction", level=0.95)

# svar: [15.15 ; 33.76]



#### Opgave XI: Tovejs ANOVA ####
#### 23. Udsagn om model ####

#αi angiver effekten for behandlingen

# svar: αi angiver effektstørrelsen for gødning i. αi ̸== 0 indebærer, at forventet plantevækst 
# afhænger af gødningstype


#### 24. Kritisk f-værdi ####

qf(0.95,df1=1,df2=2)

# Fordi F_obs = 78.77 > F_crit = 18.51, skal hypotesen forkastes

# svar: F_crit=18.51. Vi afviser nulhypotesen fordi F_obs > F_crit

#### 25. Vurdering af normalitetsantagelsen ####

# gøres ved brug af et normalfordelt QQ-plot af residualerne
# skal lave fit af parametrene

lm1 <- lm(Plant_Growth ~ Fertilizer + Watering_Frequency, data)
qqnorm(lm1$residuals)
qqline(lm1$residuals)

# svar: se ovenstående


#### Opgave X: Karakterer ####
#### 26. Aflæsning af boxplot ####

# Vælg den som er FORKERT af de følgende:

#1) Mere end halvdelen af de studerende i stikprøven havde en negativ forskel i score.
Ja.

#2) Mere end 20% af de studerende i stikprøven havde en positiv forskel i score.
Ja.

#3) Mindst en studerende i stikprøven havde en forskel større end 40 point i score.
Ja.

#4) 60% af de studerende i stikprøven havde en positiv forskel i score.
Nej.

#5) Ingen studerende i stikprøven havde en forskel i score større end 50 point.
Ja.


#### 27. Konklusion om nulhypotese ####

# Vi kigger på konfidensintervallerne og ser at alpha=0.1 ikke inkluderer 0
# derfor er der signifikant forskel

# svar: P˚a et signifikansniveau α = 0.1 detekteres en signifikant forskel i score mellem
# første og sidste halvdel.

#### 28. Konklusion om middelværdi ####

# svar: En signifikant forskel mellem de to grupper er detekteret, da konfidensintervallet
# for forskellen i middelværdi ikke inkluderer nul.


#### Opgave XI: Poisson ####
#### 29. Density poisson ####

# density poisson, hvor x=chance og lambda=rate per interval
dpois(0,lambda=4)

# svar: 0.018


#### 30. Probability poisson ####

# benytter ppois, som beregner chance for q eller lavere
# 1 - ppois fordi så får vi chance for det bliver højere end q

# q = 4
# rate per interval er fordoblet så lambda = 8

1 - ppois(4,lambda=8)

# svar: 0.900



