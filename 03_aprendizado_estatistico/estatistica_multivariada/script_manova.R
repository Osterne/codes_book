# Subject: Multivariate Statistic
# Lecture: Manova Analysis
# Author: Vinicius Osterne (www.osterne.com)















# ----------------------------------------------------
# Aplicação 1
# ----------------------------------------------------

# Dataset
head(iris)


# Boxplot
par(mfrow = c(1,4))
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)



# Histograma
par(mfrow = c(1,4))
hist(iris$Sepal.Length, prob = T)
hist(iris$Sepal.Width, prob = T)
hist(iris$Petal.Length, prob = T)
hist(iris$Petal.Width, prob = T)



# Boxplot
library(ggplot2)
library(gridExtra)

box_sl <- ggplot(iris, aes(x = Species, y = Sepal.Length, fill = Species)) +
  geom_boxplot() +
  theme(legend.position = "top")
box_sw <- ggplot(iris, aes(x = Species, y = Sepal.Width, fill = Species)) +
  geom_boxplot() +
  theme(legend.position = "top")
box_pl <- ggplot(iris, aes(x = Species, y = Petal.Length, fill = Species)) +
  geom_boxplot() +
  theme(legend.position = "top")
box_pw <- ggplot(iris, aes(x = Species, y = Petal.Width, fill = Species)) +
  geom_boxplot() +
  theme(legend.position = "top")

grid.arrange(box_sl, box_sw, box_pl, box_pw, ncol = 2, nrow = 2)



# Dispersão duas a duas
plot(iris$Sepal.Length, iris$Sepal.Width)
text(iris$Sepal.Length, iris$Sepal.Width, iris$Species, cex=0.7, pos=4, col="red")
plot(iris$Petal.Length, iris$Petal.Width)


# Dispersão conjunta
data(iris) 
pairs(iris[, 1:5], 
      col = as.integer(iris$Species),  
      pch = 18)

pairs(iris[, 1:4], 
      col = as.integer(iris$Species),  
      pch = 18)









# Médias
sapply(iris[1:4],mean)



# Desvio padrão
sapply(iris[1:4],sd)


# Covariância
cov(iris[1:4])


# Correlação
cor(iris[1:4])



# Manova Model
dependent_vars <- cbind(iris$Sepal.Length, iris$Sepal.Width, iris$Petal.Length, iris$Petal.Width)
independent_var <- iris$Species
manova_model <- manova(dependent_vars ~ independent_var, data = iris)
summary(manova_model) #ao menos uma média dos grupos se diferencia dos demais




# Quem é maior que quem?
library(MASS)
iris_lda <- lda(independent_var ~ dependent_vars, CV = F)
iris_lda


library(rmarkdown)
lda_df <- data.frame(
  species = iris[, "Species"],
  lda = predict(iris_lda)$x
)
paged_table(lda_df)

ggplot(lda_df) +
  geom_point(aes(x = lda.LD1, y = lda.LD2, color = species), size = 4) +
  theme_classic()
























# ----------------------------------------------------
# Aplicação 2
# ----------------------------------------------------


# Descrição dos dados
# época: a época à qual o crânio foi atribuído, um fator ordenado com níveis c4000BC, c3300BC, c1850BC, c200BC e cAD150, 
# onde os anos são dados apenas aproximadamente, é claro.

# mb: largura máxima do crânio
# bh: altura basibregmática do crânio
# bl: comprimento basialveolar do crânio
# nh: altura nasal do crânio


# Bibliotecas
library('car')
library('HSAUR')
library('heplots')
library('GGally')

# Visualização dos dados
data(skulls)
View(skulls)
skulls_df = as.data.frame(skulls)



# Boxplot
par(mfrow = c(1,4))
boxplot(skulls$mb)
boxplot(skulls$bh)
boxplot(skulls$bl)
boxplot(skulls$nh)


# Histograma
par(mfrow = c(1,4))
hist(skulls$mb, prob = T)
hist(skulls$bh, prob = T)
hist(skulls$bl, prob = T)
hist(skulls$nh, prob = T)

# Dispersão multivariada
pm <- ggpairs(data = skulls_df, 
              mapping = aes(color = epoch),
              columns = c("mb","bh","bl","nh" ),
              upper = list(combo ='blank', continuous='blank')
)
pm


# Nomes das variáveis
vlab <- c("maxBreadth", "basibHeight", "basialLength", "nasalHeight")
skulls$epoch <- factor(skulls$epoch, labels=sub("c","",levels(skulls$epoch)))

# Vetores de médias
means <- aggregate(cbind(mb, bh, bl, nh) ~ epoch, data=skulls, FUN=mean)[,-1]
rownames(means) <- levels(skulls$epoch)
means

# Gráfico para os vetores de médias
#pairs(means, vlab,panel = function(x, y) {
#  text(x, y, levels(skulls$epoch))
#  lines(x,y)})


# Teste de normalidade multivariada
library(mvShapiroTest)
mvShapiro.Test(as.matrix(skulls[,2:5])) #H0: seguem distribuição


# Manova
sk.mod <- lm(cbind(mb, bh, bl, nh) ~ epoch, data=skulls) #um modelo de regressão linear para cada variável resposta
summary(sk.mod)


manova(sk.mod)
summary(manova(sk.mod), test='Wilks')  #rejeita a hipótese nula
summary(manova(sk.mod), test='Pillai') #rejeita a hipótese nula




# Boxplot
library(lattice)
library(reshape2) 
sklong <- melt(skulls, id="epoch")
bwplot(value ~ epoch | variable, data=sklong, scales="free",
       ylab="Variable value", xlab="Epoch",
       strip=strip.custom(factor.levels=paste(vlab, " (", levels(sklong$variable), ")", sep="")),
       panel = function(x,y, ...) {
         panel.bwplot(x, y, ...)
         panel.linejoin(x,y, col="red", ...)})


#pairs(sk.mod, hypotheses=list(Lin="epoch.L", Quad="epoch.Q"), var.labels=vlab)
#heplot(sk.mod, hypotheses=list(Lin="epoch.L", Quad="epoch.Q"), xlab=vlab[1], ylab=vlab[2])




















