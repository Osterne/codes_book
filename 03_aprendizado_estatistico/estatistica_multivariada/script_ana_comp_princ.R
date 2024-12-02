# Subject: Multivariate Statistic
# Lecture: Principal Components Analysis
# Author: Vinicius Osterne (www.osterne.com)






# ------------------------------------------------------------------------------
# Application 1
# ------------------------------------------------------------------------------

# packages
library('FactoMineR') #onde os dados estão
library("devtools") #ferramenta para ACP
library("factoextra") #ferramenta para ACP


# data
data(decathlon2)
head(decathlon2[, 1:6])


# Extracting variables for principal component analysis:
decathlon2.active <- decathlon2[1:23, 1:10]
head(decathlon2.active[, 1:6])




# descriptive
decathlon2.active_stats <- data.frame(
  Min = apply(decathlon2.active, 2, min), # minimum
  Q1 = apply(decathlon2.active, 2, quantile, 1/4), # First quartile
  Med = apply(decathlon2.active, 2, median), # median
  Mean = apply(decathlon2.active, 2, mean), # mean
  Q3 = apply(decathlon2.active, 2, quantile, 3/4), # Third quartile
  Max = apply(decathlon2.active, 2, max) # Maximum
)
decathlon2.active_stats <- round(decathlon2.active_stats, 1)
head(decathlon2.active_stats)




# correlation matrix
cor.mat <- round(cor(decathlon2.active),2)
head(cor.mat[, 1:6])

# casas decimais
# 0 -- 9: uma casa decimal
## 10 -- 99: duas casas decimais
## 100 -- 999: três casas decimais






# correlation graph
library("corrplot")
corrplot(cor.mat, type="upper", order="hclust", 
         tl.col="black", tl.srt=45)

library("PerformanceAnalytics")
chart.Correlation(decathlon2.active[, 1:6], histogram=TRUE, pch=19)


# Principal Component Analysis
res.pca <- prcomp(decathlon2.active, scale = TRUE)
res.pca
names(res.pca)

#Variances of the principal components
fviz_screeplot(res.pca, ncp=10)

# circular plot
#fviz_pca_ind(res.pca,  col.ind="cos2") +
#  scale_color_gradient2(low="blue", mid="white", high="red", midpoint=0.50)+
#  theme_minimal()

# circular plot
fviz_pca_var(res.pca)



# Criar exemplo com multiplocaçao dos valores!!!!!!!!!!!!!!!!


# circular plot (change color)
fviz_pca_var(res.pca, col.var="steelblue")+
  theme_minimal()

# circular plot (change color)
fviz_pca_var(res.pca, col.var="contrib")

# circular plot (change color)
fviz_pca_var(res.pca, col.var="contrib")+
  scale_color_gradient2(low="blue", mid="white", 
                        high="red", midpoint=55)+theme_bw()

# circular plot (change color)
fviz_pca_var(res.pca, alpha.var="contrib")+
  theme_minimal()


# biplot
fviz_pca_biplot(res.pca,  geom = "text")





# ------------------------------------------------------------------------------
# Application 2
# ------------------------------------------------------------------------------

# data
head(USArrests)

# calculate principal components
results <- prcomp(USArrests, scale = TRUE)

# reverse the signs
results$rotation <- -1*results$rotation

#display principal components
results$rotation

# reverse the signs of the scores
results$x <- -1*results$x

# display the first six scores
head(results$x)

#biplot
biplot(results, scale = 0)

#display states with highest murder rates in original dataset
head(USArrests[order(-USArrests$Murder),])

#calculate total variance explained by each principal component
results$sdev^2 / sum(results$sdev^2)

#calculate total variance explained by each principal component
var_explained = results$sdev^2 / sum(results$sdev^2)

#create scree plot
qplot(c(1:4), var_explained) + 
  geom_line() + 
  xlab("Principal Component") + 
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)






# ------------------------------------------------------------------------------
# Application 3
# ------------------------------------------------------------------------------

# data
data("iris")

# exploratory
str(iris)

# graphs
library(psych)
pairs.panels(iris[,-5],
             gap = 0,
             bg = c("red", "yellow", "blue")[iris$Species],
             pch=21)


# pca
pc <- prcomp(iris[,-5],
             center = TRUE,
             scale. = TRUE)
#attributes(pc)
pc

# exploratory pca
pairs.panels(pc$x,
             gap=0,
             bg = c("red", "yellow", "blue")[iris$Species],
             pch=21)


# biplot
library(devtools)
library(ggbiplot)
g <- ggbiplot(pc,
              obs.scale = 1,
              var.scale = 1,
              groups = iris$Species,
              ellipse = TRUE,
              circle = TRUE,
              ellipse.prob = 0.68)
g <- g + scale_color_discrete(name = '')
g <- g + theme(legend.direction = 'horizontal',
               legend.position = 'top')
print(g)

















