Strength <- c(242, 214, 254, 248, 248, 214, 248, 247, 236, 211, 245, 243)
Joiningmethod <- factor(c("A", "A", "A", "A", "B", "B", "B", "B", "C", "C", "C", "C"))
Material <- factor(c(1, 2, 3, 4, 1, 2, 3, 4, 1, 2, 3, 4))
SSE1 <- 84.5
MSE1 <- SSE1 / (3 - 1)
F1 <- MSE1 / 8.25
c(MSE1, F1)

k <- 3
l <- 4
MSE <- 0.10556
SSE <- MSE * (l - 1) * (k - 1)
SSE

MSTr <- 0.16333
MSBl <- 1.84556

FTr <- MSTr / MSE
FBl <- MSBl / MSE

c(SSE, FTr, FBl)

k <- 3
l <- 3

MSE <- 0.05833
FFm <- 4.2857
MSFm <- MSE * FFm
SSFm <- MSFm * (k - 1)

SSIs <- 1.44667
MSIs <- SSIs / (l - 1)
FIs <- MSIs / MSE
c(SSFm, MSFm, MSIs, FIs)


k <- 3
l <- 4
SSJm <- 84.5
MSMt <- 825
SSE <- 49.5
MSE <- 8.25

MSJm <- SSJm / (k - 1)
FJm <- MSJm / MSE
SSMt <- MSMt * (l - 1)
FMt <- MSMt / MSE
c(k - 1, MSJm, FJm)

c(l - 1, SSMt, FMt)

MJm <- k * (k - 1) / 2
alpha <- 0.05
alphaJm <- alpha / MJm

LSDJm <- qt(1 - alphaJm / 2, df = (l - 1) * (k - 1)) * sqrt(2 * MSE / l)
LSDJm
tapply(Strength, Joiningmethod, mean)

MMt <- l * (l - 1) / 2
alphaMt <- alpha / MMt

LSDMt <- qt(1 - alphaMt / 2, df = (l - 1) * (k - 1)) * sqrt(2 * MSE / k)
LSDMt
tapply(Strength, Material, mean)
