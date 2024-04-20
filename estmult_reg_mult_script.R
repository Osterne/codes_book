# Subject: Multivariate Statistic
# Lecture: Multivariate Regression
# Author: Vinicius Osterne (www.osterne.com)





data(mtcars)
head(mtcars, n=8)

summary(mtcars)

mtcars$cyl = factor(mtcars$cyl)
Y = as.matrix(mtcars[,c("mpg","disp","hp","wt")])

model_mult = lm(Y ~ cyl + am + carb, data=mtcars)
summary(model_mult)



























































# $Y_1$: comprimento na quebra
# $Y_2$: módulo de elasticidade
# $Y_3$: estresse na falha
# $Y_4$: resistência à quebra

# $Z_1$: comprimento da fibra
# $Z_2$: fração de fibra grossa
# $Z_3$: fração de fibra fina
# $Z_4$: extensão à tração nula


library(robustbase)
library(GGally)

fibras = pulpfiber
View(fibras)

ggpairs(pulpfiber, upper = "blank")

ggcorr(fibras)

names(fibras)

colnames(fibras)[1:4] = c('Z1','Z2','Z3','Z4')

attach(fibras)

modelo1 = lm(Y1 ~ Z1+Z2+Z3+Z4)
modelo2 = lm(Y2 ~ Z1+Z2+Z3+Z4)
modelo3 = lm(Y3 ~ Z1+Z2+Z3+Z4)
modelo4 = lm(Y4 ~ Z1+Z2+Z3+Z4)

summary(modelo1)
summary(modelo2)
summary(modelo3)
summary(modelo4)


# Diagnóstico
plot(modelo1, 1)
plot(modelo2, 1)
plot(modelo3, 1)
plot(modelo4, 1)


# Modelo multivariado
modelo_multivariado <- lm(cbind(Y1,Y2,Y3,Y4) ~ Z1+Z2+Z3+Z4, data=fibras)
summary(modelo_multivariado)





