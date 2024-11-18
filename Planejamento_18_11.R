y = c(65,64,
      81,71,
      57,83,
      66,59,
      82,65,
      82,56,
      67,69,
      59,74,
      75,82,
      70,79)

Formula = factor(rep(c(1,2),10))

Formula

cbind(y, Formula)

install.packages("ExpDes.pt")
library(ExpDes.pt)

summary(y)
tapply(y, Formula, mean)
tapply(y, Formula, sd)
tapply(y, Formula, min)
tapply(y, Formula, max)
tapply(y, Formula, length)
table(Formula)
100*tapply(y, Formula, sd)/tapply(y, Formula, mean) #CV(Coeficiente de Variação)

boxplot(y~Formula)
points(Formula,y, pch = 19)

boxplot(y)
points(y, pch = 19) #Rever se está correto

# Problema 2-21

dic(Formula, y, quali = T, hvar = "bartlett")
saida1 = dic(Formula, y, quali = T, mcomp = "tukey", hvar = "bartlett")

names(saida1)

plot(saida1$residuos/sqrt(86.778), ylim = c(-3,3))
abline(h = 0)
abline(h = 2)
abline(h = -2)

#Problema 3.5

y1=c(3129,3000,2865,2890,
    3200,3300,2975,3150,
    2800,2900,2985,3050,
    2600,2700,2600,2765)

Mistura = factor(rep(seq(1:4), each = 4))

cbind(y1, Mistura)

boxplot(y1 ~ Mistura)
points(Mistura, y1)

saida2 = dic(Mistura, y1, quali = T, mcomp = "tukey", hvar = "bartlett")
