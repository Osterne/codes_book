# Subject: Multivariate Statistic
# Lecture: Manova Analysis
# Author: Vinicius Osterne (www.osterne.com)














# Descrição dos dados
# época: a época à qual o crânio foi atribuído, um fator ordenado com níveis c4000BC, c3300BC, c1850BC, c200BC e cAD150, onde os anos são dados apenas aproximadamente, é claro.

# mb: largura máxima do crânio
# bh: altura basibregmática do crânio
# bl: comprimento basialveolar do crânio
# nh: altura nasal do crânio


# Bibliotecas
library('car')
library('HSAUR')
library('heplots')
library('GGally')



# boxplot
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





# Visualização
data(skulls)
View(skulls)
skulls_df = as.data.frame(skulls)


# Dispersão
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
mvShapiro.Test(as.matrix(skulls[,2:5]))


# Manova
sk.mod <- lm(cbind(mb, bh, bl, nh) ~ epoch, data=skulls) #um modelo de regressão linear para cada variável resposta
summary(sk.mod)


manova(sk.mod)
summary(manova(sk.mod), test='Wilks')  #rejeita a hopótese nula
summary(manova(sk.mod), test='Pillai') #rejeita a hopótese nula




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
