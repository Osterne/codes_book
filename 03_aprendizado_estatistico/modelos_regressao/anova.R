# Subject: Regression Models
# Lecture: Anova
# Author: Vinicius Osterne (www.osterne.com)



# ------------------------------------------------------------------------------
# Application 1
# ------------------------------------------------------------------------------

# download the data
dados = c(16, 12, 14, 15, 19, 20,
          12, 11, 13, 18, 17, 21,
          16, 19, 18, 19, 17, 19,
          20, 21, 18, 20, 21, 23)

g1 = as.factor(rep(1:4, each = 6))
g2 = as.factor(rep(1:6, times = 4))



# ajustando modelo
a = aov(dados~g1+g2)
summary(a)

# teste de Tukey
TukeyHSD(aov(dados~g1+g2))
plot(TukeyHSD(a))
boxplot(TukeyHSD(a))
