# Subject: Multivariate Statistic
# Lecture: Factorial Analysis
# Author: Vinicius Osterne (www.osterne.com)





# ------------------------------------------------------------------------------
# Application 1
# ------------------------------------------------------------------------------


# necessary packages
library(psych) 
library(corrplot)
library(nFactors)

# load dataset 
data(mtcars)

# show dataset
head(mtcars)

# show dataset
dim(mtcars)

# correlation graph
corrplot(cor(mtcars), method="ellipse")

# Factor analysis 
factor_analysis = factanal(mtcars, factors = 3, rotation = "varimax")
factor_analysis

# factor plots
load = factor_analysis$loadings[,1:2] 
plot(load,type="n") 
text(load,labels=names(mtcars),cex=.7, col = 1)


# factor plots
library(FactoMineR)
result <- PCA(mtcars)


# loadings
fa1 <- factanal(mtcars, factors = 3, scores = "regression")
fa1$scores


# different rotations
factor_analysis_none = factanal(mtcars, factors = 2, rotation = "none")
factor_analysis_varimax = factanal(mtcars, factors = 2, rotation = "varimax")
factor_analysis_promax = factanal(mtcars, factors = 2, rotation = "promax")

# graph for these rotations
par(mfrow = c(1, 3))
plot(factor_analysis_none$loadings[, 1],
     factor_analysis_none$loadings[, 2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     ylim = c(-1, 1),
     xlim = c(-1, 1),
     main = "No rotation")
text(factor_analysis_none$loadings[, 1] - 0.08,
     factor_analysis_none$loadings[, 2] + 0.08,
     colnames(mtcars),
     col = "blue")
abline(h = 0, v = 0)

plot(factor_analysis_varimax$loadings[, 1],
     factor_analysis_varimax$loadings[, 2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     ylim = c(-1, 1),
     xlim = c(-1, 1),
     main = "Varimax rotation")

text(factor_analysis_varimax$loadings[, 1] - 0.08,
     factor_analysis_varimax$loadings[, 2] + 0.08,
     colnames(mtcars),
     col = "blue")
abline(h = 0, v = 0)

plot(factor_analysis_promax$loadings[, 1],
     factor_analysis_promax$loadings[, 2],
     xlab = "Factor 1",
     ylab = "Factor 2",
     ylim = c(-1, 1),
     xlim = c(-1, 1),
     main = "Promax rotation")
text(factor_analysis_promax$loadings[, 1] - 0.08,
     factor_analysis_promax$loadings[, 2] + 0.08,
     colnames(mtcars),
     col = "blue")
abline(h = 0, v = 0)




# ------------------------------------------------------------------------------
# Application 2
# ------------------------------------------------------------------------------

v1 <- c(1,1,1,1,1,1,1,1,1,1,3,3,3,3,3,4,5,6)
v2 <- c(1,2,1,1,1,1,2,1,2,1,3,4,3,3,3,4,6,5)
v3 <- c(3,3,3,3,3,1,1,1,1,1,1,1,1,1,1,5,4,6)
v4 <- c(3,3,4,3,3,1,1,2,1,1,1,1,2,1,1,5,6,4)
v5 <- c(1,1,1,1,1,3,3,3,3,3,1,1,1,1,1,6,4,5)
v6 <- c(1,1,1,2,1,3,3,3,4,3,1,1,1,2,1,6,5,4)
m1 <- cbind(v1,v2,v3,v4,v5,v6)
cor(m1)
factanal(m1, factors = 3) # varimax is the default
factanal(m1, factors = 3, rotation = "promax")
# The following shows the g factor as PC1
prcomp(m1) # signs may depend on platform

## formula interface
factanal(~v1+v2+v3+v4+v5+v6, factors = 3,
         scores = "Bartlett")$scores




















# ------------------------------------------------------------------------------
# Application 3
# ------------------------------------------------------------------------------


#Cereais:

#All Bran
#Cerola Muesli
#Just right
#Kellogg's Corn Flakes
#Komplete
#NutriGrain
#Purina Muesli
#Rice Bubbles
#Special K
#Sustain
#Vitabrit
#Weetbrix


#Atributos:
  
#Satisfaz
#Natural
#Fibra
#Doce
#Fácil
#Sal
#Gratificante
#Energia
#Divertido
#Crianças
#Encharcado
#Econômico
#Saúde
#Família
#Calorias
#Simples
#Crocante
#Regular
#Açúcar
#Fruta
#Processo
#Qualidade
#Prazer
#Chato
#Nutritivo

# data
data = read_csv("rte_cereal.csv")
data = data.frame(data) 
  
# view data
head(data)

# change col names
#colnames(data)[1] <- "Voluntário"
#colnames(data)[2] <- "Cereal"
colnames(data) <- c("Voluntário", "Cereal",
                    'Satisfaz','Natural','Fibra','Doce','Fácil','Sal','Gratificante','Energia',
                    'Divertido','Crianças','Encharcado','Econômico','Saúde','Família','Calorias',
                    'Simples', 'Crocante', 'Regular', 'Açúcar', 'Fruta', 'Processo', 'Qualidade',
                    'Prazer', 'Chato', 'Nutritivo')
head(data)

# correlation graph
corrplot(cor(data), method="ellipse")

# AF
af_data = factanal(data, factors = 4) # varimax is the default
af_data

# loadings
load = af_data$loadings[,1:4]
load = data.frame(load)

# Greater loadings on the 1st two factors
load <- load[order(load$Factor1, decreasing = TRUE),]
load

# Greater loadings on the 2nd two factors
load <- load[order(load$Factor2, decreasing = TRUE),]
load

# Greater loadings on the 3th two factors
load <- load[order(load$Factor3, decreasing = TRUE),]
load

# Greater loadings on the 4th two factors
load <- load[order(load$Factor4, decreasing = TRUE),]
load

# variance explained (SS loadings)
af_data$loadings

# how do we use the factors?
score_f1 = c(af_data$loadings[,1] %*% t(data))
score_f1

score_f2 = c(af_data$loadings[,2] %*% t(data))
score_f2

# factor plots
load = af_data$loadings[,1:2] 
plot(load,type="n") 
text(load,labels=names(data),cex=.7, col = 1)


