# Subject: Multivariate Statistic
# Lecture: Discriminant Analysis
# Author: Vinicius Osterne (www.osterne.com)


#https://rpubs.com/Joaquin_AR/233932


# ---------------------------------------------------------------------------
# Application 1
# ---------------------------------------------------------------------------


input <- ("
especie pata abdomen organo_sexual 
a 191 131 53
a 185 134 50
a 200 137 52
a 173 127 50
a 171 128 49
a 160 118 47
a 188 134 54
a 186 129 51
a 174 131 52
a 163 115 47
b 186 107 49
b 211 122 49
b 201 144 47
b 242 131 54
b 184 108 43
b 211 118 51
b 217 122 49
b 223 127 51
b 208 125 50
b 199 124 46
")

datos <- read.table(textConnection(input), header = TRUE)
datos$especie <- as.factor(datos$especie)



library(ggplot2)
library(ggpubr)

p1 <- ggplot(data = datos, aes(x = pata, fill = especie)) +
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = datos, aes(x = abdomen, fill = especie)) +
  geom_histogram(position = "identity", alpha = 0.5)
p3 <- ggplot(data = datos, aes(x = organo_sexual, fill = especie)) +
  geom_histogram(position = "identity", alpha = 0.5)

ggarrange(p1, p2, p3, nrow = 3, common.legend = TRUE)


pairs(x = datos[, c("pata","abdomen","organo_sexual")],
      col = c("firebrick", "green3")[datos$especie], pch = 19)

library(scatterplot3d)
scatterplot3d(datos$pata, datos$abdomen, datos$organo_sexual,
              color = c("firebrick", "green3")[datos$especie], pch = 19,
              grid = TRUE, xlab = "pata", ylab = "abdomen",
              zlab = "organo sexual", angle = 65, cex.axis = 0.6)
legend("topleft",
       bty = "n", cex = 0.8,
       title = "Especie",
       c("a", "b"), fill = c("firebrick", "green3"))


# Representación mediante Histograma de cada variable para cada especie 
par(mfcol = c(2, 3))
for (k in 2:4) {
  j0 <- names(datos)[k]
  #br0 <- seq(min(datos[, k]), max(datos[, k]), le = 11)
  x0 <- seq(min(datos[, k]), max(datos[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(datos$especie)[i]
    x <- datos[datos$especie == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("especie", i0), xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}


# Representación de cuantiles normales de cada variable para cada especie
for (k in 2:4) {
  j0 <- names(datos)[k]
  x0 <- seq(min(datos[, k]), max(datos[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(datos$especie)[i]
    x <- datos[datos$especie == i0, j0]
    qqnorm(x, main = paste("especie", i0, j0), pch = 19, col = i + 1)
    qqline(x)
  }
}



# Contraste de normalidad Shapiro-Wilk para cada variable en cada especie
library(reshape2)
library(knitr)
library(dplyr)
datos_tidy <- melt(datos, value.name = "valor")
kable(datos_tidy %>% group_by(especie, variable) %>% summarise(p_value_Shapiro.test = shapiro.test(valor)$p.value))


# Misma operación con aggregate
#aggregate(formula = valor ~ especie + variable, data = datos_tidy,
#          FUN = function(x){shapiro.test(x)$p.value})



library(biotools)
boxM(data = datos[, 2:4], grouping = datos[, 1])




modelo_lda <- lda(formula = especie ~ pata + abdomen + organo_sexual,
                  data = datos)
modelo_lda



nuevas_observaciones <- data.frame(pata = 194, abdomen = 124, organo_sexual = 49)
predict(object = modelo_lda, newdata = nuevas_observaciones)


predicciones <- predict(object = modelo_lda, newdata = datos[, -1],
                        method = "predictive")
table(datos$especie, predicciones$class,
      dnn = c("Clase real", "Clase predicha"))


trainig_error <- mean(datos$especie != predicciones$class) * 100
paste("trainig_error=", trainig_error, "%")








# ---------------------------------------------------------------------------
# Application 2
# ---------------------------------------------------------------------------

data("iris")
kable(head(iris, n = 3))


library(ggplot2)
library(ggpubr)

plot1 <- ggplot(data = iris, aes(x = Sepal.Length)) +
  geom_density(aes(colour = Species)) + theme_bw()
plot2 <- ggplot(data = iris, aes(x = Sepal.Width)) +
  geom_density(aes(colour = Species)) + theme_bw()
plot3 <- ggplot(data = iris, aes(x = Petal.Length)) +
  geom_density(aes(colour = Species)) + theme_bw()
plot4 <- ggplot(data = iris, aes(x = Petal.Width)) +
  geom_density(aes(colour = Species)) + theme_bw()
# la función grid.arrange del paquete grid.extra permite ordenar
# graficos de ggplot2
ggarrange(plot1, plot2, plot3, plot4, common.legend = TRUE, legend = "bottom")


pairs(x = iris[, -5], col = c("firebrick", "green3", "blue")[iris$Species],
      pch = 20)


#representación mediante histograma de cada variable para cada especie 
par(mfcol = c(3, 4))
for (k in 1:4) {
  j0 <- names(iris)[k]
  x0 <- seq(min(iris[, k]), max(iris[, k]), le = 50)
  for (i in 1:3) {
    i0 <- levels(iris$Species)[i]
    x <- iris[iris$Species == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste("especie", i0),
         xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}



#representación de cuantiles normales de cada variable para cada especie 
for (k in 1:4) {
  j0 <- names(iris)[k]
  x0 <- seq(min(iris[, k]), max(iris[, k]), le = 50)
  for (i in 1:3) {
    i0 <- levels(iris$Species)[i]
    x <- iris[iris$Species == i0, j0]
    qqnorm(x, main = paste(i0, j0), pch = 19, col = i + 1) 
    qqline(x)
  }
}






#Contraste de normalidad Shapiro-Wilk para cada variable en cada especie
library(reshape2)
library(knitr)
library(dplyr)
datos_tidy <- melt(iris, value.name = "valor")
kable(datos_tidy %>% group_by(Species, variable) %>% summarise(p_value_Shapiro.test = round(shapiro.test(valor)$p.value,5)))





library(biotools)
boxM(data = iris[, -5], grouping = iris[, 5])



library(MASS)
modelo_lda <- lda(Species ~ Sepal.Width + Sepal.Length + Petal.Length +
                    Petal.Width, data = iris)
modelo_lda


predicciones <- predict(object = modelo_lda, newdata = iris[, -5])
table(iris$Species, predicciones$class, dnn = c("Clase real", "Clase predicha"))



trainig_error <- mean(iris$Species != predicciones$class) * 100
paste("trainig_error =", trainig_error, "%")




library(klaR)
partimat(Species ~ Sepal.Width + Sepal.Length + Petal.Length + Petal.Width,
         data = iris, method = "lda", prec = 200,
         image.colors = c("darkgoldenrod1", "snow2", "skyblue2"),
         col.mean = "firebrick")








# ---------------------------------------------------------------------------
# Application 3 - Quadratic
# ---------------------------------------------------------------------------

library(mclust)
library(knitr)
data(banknote)
#se recodifican las clases de la variable Status: verdadero = 0, falso = 1
levels(banknote$Status)


levels(banknote$Status) <- c("falso","verdadero")
kable(head(banknote))

library(ggplot2)
library(ggpubr)
p1 <- ggplot(data = banknote, aes(x = Length, fill = Status)) +
  geom_histogram(position = "identity", alpha = 0.5)
p2 <- ggplot(data = banknote, aes(x = Left, fill = Status)) +
  geom_histogram(position = "identity", alpha = 0.5)
p3 <- ggplot(data = banknote, aes(x = Right, fill = Status)) +
  geom_histogram(position = "identity", alpha = 0.5)
p4 <- ggplot(data = banknote, aes(x = Bottom, fill = Status)) +
  geom_histogram(position = "identity", alpha = 0.5)
p5 <- ggplot(data = banknote, aes(x = Top, fill = Status)) +
  geom_histogram(position = "identity", alpha = 0.5)
ggarrange(p1, p2, p3, p4, p5, nrow = 5, common.legend = TRUE, legend = "bottom")





pairs(x = banknote[,-1], col = c("firebrick", "green3")[banknote$Status],
      pch = 20)



# Representación mediante Histograma de cada variable para cada tipo de billete 
par(mfcol = c(2, 6))
for (k in 2:7) {
  j0 <- names(banknote)[k]
  x0 <- seq(min(banknote[, k]), max(banknote[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(banknote$Status)[i]
    x <- banknote[banknote$Status == i0, j0]
    hist(x, proba = T, col = grey(0.8), main = paste( i0), xlab = j0)
    lines(x0, dnorm(x0, mean(x), sd(x)), col = "red", lwd = 2)
  }
}




# Representación de cuantiles normales de cada variable para cada tipo de billete
for (k in 2:7) {
  j0 <- names(banknote)[k]
  x0 <- seq(min(banknote[, k]), max(banknote[, k]), le = 50)
  for (i in 1:2) {
    i0 <- levels(banknote$Status)[i]
    x <- banknote[banknote$Status == i0, j0]
    qqnorm(x, main = paste(i0, j0), pch = 19, col = i + 1) 
    # los colores 2 y 3 son el rojo y verde
    qqline(x)
  }
}





#Contraste de normalidad Shapiro-Wilk para cada variable en cada tipo de billete
library(reshape2)
library(knitr)
library(dplyr)
datos_tidy <- melt(banknote, value.name = "valor")
datos_tidy %>% group_by(Status, variable) %>%
  summarise(p_value_Shapiro.test = round(shapiro.test(valor)$p.value,5))





library(biotools)
boxM(data = banknote[, -1], grouping = banknote[, 1])



library(MASS)
modelo_qda <- qda(formula = Status ~ ., data = banknote, prior = c(0.01, 0.99))
modelo_qda



predicciones <- predict(object = modelo_qda, newdata = banknote)
table(banknote$Status, predicciones$class,
      dnn = c("Clase real", "Clase predicha"))



trainig_error <- mean(banknote$Status != predicciones$class) * 100
paste("trainig_error=", trainig_error, "%")



library(klaR)
partimat(formula = Status ~ ., data = banknote, prior = c(0.01, 0.99),
         method = "qda", prec = 200,
         image.colors = c("darkgoldenrod1", "skyblue2"),
         col.mean = "firebrick", nplots.vert = 4)








a = c(1, 4, 8)
b = c(2, 8, 16)
c = c(3, 12, 24)
matrix_ex = matrix(cbind(a,b,c), nrow = 3)
matrix_ex
solve(matrix_ex)

I_n = diag(3)

lambda = 0.01
matrix_ex_new = matrix_ex + lambda*I_n
solve(matrix_ex_new)


