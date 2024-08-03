
# Maj 2022 Eksamen

#### Opgave I: sansynlighed ####
#### 1. dbinom ####

dbinom(0, size=10, prob=0.03)

# svar: 0.737

#### 2. qbinom ####

# bruger quantile binom
qbinom(0.98, size=10, prob=0.03)

# svar: 2

#### 3. varians i tæthedsfunktion ####

# Varians = (X-E(X))^2 * f(x)

(0-1.7)^2 # 2.89
(1-1.7)^2 # 0.49
(2-1.7)^2 # 0.09
(3-1.7)^2 # 1.69

# svar: V(X) = 0.1*2.89 + 0.3*0.49 + 0.4*0.09 + 0.2*1.69



#### Opgave II: ####
#### 4. standardafvigelse ####

x1 <- 1920
n1 <- 2000

x2 <- 1801
n2 <- 2024

p2 <- x2/n2
sqrt(p2 * ((1-p2)/n2))

# svar: 0.006959748

#### 5. nulhypotese ####

p1 <- x1/n1
p2 <- x2/n2

# p1 = 0.96
# p2 = 0.8898

# nulhypotese afvises fordi [p2 ; p1] ikke indeholder 0

# svar: Nulhypotesen afvises da 0 = [0:049; 0:091], derfor er de to andele signifikant forskellige.




#### Opgave III: To uparrede stikprøver ####
#### 6. pooled standardafvigelse ####

# Bruger metode 3.52 for pooled varians
# fordi n1 = n2, skal vi bare bestemme gennemsnittet af de to varianser
s1 <- 3.6
s2 <- 3.3

# følgende er et udtryk for varians
(s1^2 + s2^2) / 2

# kvadratroden af dette er standard afvigelsen
sqrt((s1^2 + s2^2) / 2)


# svar: sqrt((3.6^2 + 3.3^2) / 2)

#### 7. udsagn om pooled t-test ####

# 95% konfidensintervallet indeholder ikke 0, så vi forkaster nulhypotesen
# Risiko for at lave en type I fejl er lig signifikansniveauet, så alpha = 0.05 = 5%


#### 8. stikprøve størrelse for to stikprøver ####

# Vi bruger power t test!
# delta = den ønskede detekterede middelværdi
# sd = standard afvigelse = sqrt(varians)
# sig.level = signifikans niveau
# power = sansynlighed

power.t.test(delta=5, sd=sqrt(16), sig.level=0.01, power=0.98, type="two.sample")

# n = 29.15 hvilket betyder mindst 30

# svar: mindst 30


#### Opgave IV: uafhængig eksponentiel fordeling ####
#### 9. sansynlighed ####

# sansynlighed for at 1 komponent holder længere end de 3 års levetid

# funktion skal være probability exp
# q skal være 1
# rate skal være 1/3 fordi den skal dø inden for 1 år

1 - pexp(1, rate=1/3)


#### 10. aflæs histogram ####

# hvor mange observationer?

# det kan du ikke se på et density histogram lol

#### 11. parametrisk bootstrapet 95% konfidensinterval ####

# kvantiler på [1.561813 ; 2.914021] hvilket dækker 2

# svar: vi accepterer nulhypotesen, og middelværdi er måske 2



#### Opgave V: sd og varians for stokastiske variable ####
#### 12. simulering af normalfordeling ####

# Vi skal simulere en normalfordeling som gøres med rnorm(n, mean, sd)

k <- 1000000 # vi kører k antal simulationer
x1 <- rnorm(k,0,1) # simulerer x med 
x2 <- rnorm(k,0,1)
sd(exp(x1) + x2^4 + x1*x2)

# svar: 10

#### 13. lineær tilnærmelse (propagation of error) ####

# Vi benytter metode 4.3¨
# Vi skal differentiere og opløfte i anden

V(Y) = (X1^3)'^2 + (5*X2)'^2
V(Y) = (3*X1^2)^2 + (5*1)^2
V(Y) = 3^2*X1^4 + 5^2*1^2
V(Y) = 9*x1^4 + 25

# til sidst ganger vi sigma1^2 og sigma1^2 på hver deres observation
V(Y) = 9*x1^4 * sigma1^2 + 25*sigma2^2

# svar: V(Y) = 9x1^4*sigma1^2 + 25*sigma2^2




#### Opgave VI: lineær regression ####
#### 14. estimat for beta_1 ####

# Mangankoncentrationer
x <- c(0, 0, 2, 2, 4, 4, 6, 6, 8, 8, 10, 10)
# ICP-AES værdier
y <- c(114, 14, 870, 1141, 2087, 2212, 3353, 2633, 3970, 4299, 4950, 5207)

fit <- lm(y ~x)
summary(fit)

# vi aflæser estimatet af beta_1 til at være 504.33

# svar: 504.33

#### 15. prædiktionsinterval ####

predict(fit, newdata=data.frame(x = 5), interval = "prediction", level=0.95)

# svar: [2087.363 ; 3054.303]

#### 16. nulhypotese ####

# vi aflæser summary igen lol
summary(fit)

# p-værdi er 0.655 for beta_0
# vi accepterer nulhypotesen da p-værdien er super høj

# svar: Vi accepterer nulhypotesen, da p-værdien er 0.655.

#### 17. aflæs residual- og normal Q-Q plot ####

# Residualplottet er tvivlsomt
# Dette indikerer et problem med antagelsen om lineær sammenhæng



#### 18. fortolkning af summary ####

# Vi ser at R^2 = 0.99 hvilket betyder den forklarede varians i data

# svar: Vi kan med modellen forklare mere end 99% af den observerede variation i data.




#### Opgave VII: data tabel ####
#### 19. forventet værdi ####

# (sumIkkeBesluttet * SumEfter) / n
(42*59) / 124


#### 20. chi^2 test konklusion ####

# p-værdi = 0.74 er større end signifikans niveau på alpha = 0.05

# svar: vi accepterer fordi p-værdi > sig. level

#### 21. kritisk niveau (chi^2 fraktil) ####

# vi benytter frihedgraderne fra tidligere, og signifikans niveau 0.01 aka p=0.99
# vi bruger chi^2 fordelings funktion quantile chisq
qchisq(0.99, 2)

# svar: 9.21034





#### Opgave VIII: Hypotesetest med én stikprøve ####
#### 22. konfidensinterval ####

n <- 56
gns <- 21.5
s <- 9.8
h0 <- 23
gns + c(-1,1) * qt(0.975,n-1) * s/sqrt(n)

# konfidensintervallet [18.88 ; 24.12] dækker h0=23, så vi kan ikke forkaste hypotesen

# svar: [18.88 ; 24.12] og godkender hypotese

#### 23. t_obs ####

t_obs = (gns-h0) / (s/sqrt(n))
t_obs

# svar: -1.145405





#### Opgave IX: poisson sansynlighed ####
#### 24. ppois ####

# sansylighed for 2 eller mere 
1 - ppois(1,lambda=7)

# svar: 0.99

#### 25. dpois ####

# sansynlighed for ingen i et halv minut
dpois(0, lambda=7/2)

# svar: 0.03




#### Opgave X: Envejs ANOVA ####
#### 26. SST, SS(Tr) og SSE  ####

y <- c(1.89, 2.35, 1.68, 2.11, 3.15, 2.16, 2.40, 2.59, 1.54, 2.02, 2.01, 2.11)
grp<-c(rep("a",4),rep("b",4),rep("c",4))
anova(lm(y~grp))

# svar: SST = 1.99, SS(Tr) = 1.01, SSE = 0.98


#### 27. aflæs boksplot ####

#1 De sorte linjer i boksene angiver gennemsnittet af hver stikprøve.
Nej det er medianen

#2 Medianerne er cirka 2.0, 2.5 og 2.0 for henholdsvis gruppe 1, 2 og 3.
Yep 

#3 Boksbredden defineres som forskellen mellem øvre og nedre kvartil, altså 
# forskellen mellem 95. og 5. percentil.
Nej, øvre og nedre er 75 og 25

#4 Boksbredden er defineret som forskellen mellem øvre og nedre kvartil, altså 
# forskellen mellem 90. og 10. percentil.
Nej, øvre og nedre er 75 og 25

#5 Whiskers på boxplot definerer Interquartile Range, dvs. IQR = Q3-Q1.
Nej det er 0 til 25 og 75 til 100



#### Opgave XI: tovejs ANOVA ####
#### 28. forentet (forudsagt) værdi ####

# y_21 = m^u + alphahat^2 = 5.6 + 0.59
# y_24 = m^u + alphahat^2 = 5.6 + 0.59
# beta_j is not added in both cases because the ANOVA table indicates that treatment is not 
# significant, i.e. the p-value is greater than 0.05.

#### 29. Bestem værdier i ANOVA ####

# Bestem frihedsgrader. Her er J antal behandlinger
df = J-1
df = 4-1

# Bestem teststørrelse
F(treat) = MSE(treat) / MSE(Res)
F(treat) = 0.08513 / 0.10414

# svar: df=3 og teststørrese = 0.8174573

#### 30. nulhypotese konklusion ####

# sansynlighed for at lave type I fejl = signifikans niveau = 5%

# Under the assumption that H0 is true, The p-value is the probability of obtaining an at least
# as extreme test statistic as the oberved. For an F test we only look at more extreme values
# regarding the right tail of the distribution.

# Sandsynligheden for at teststatistikken er højere end den observerede teststatistik er
# 1.33% givet at nulhypotesen er sand.

