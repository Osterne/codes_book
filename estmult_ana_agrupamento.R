# Subject: Multivariate Statistic
# Lecture: Discriminant Analysis
# Author: Vinicius Osterne (www.osterne.com)


#https://rpubs.com/rdelgado/399475


# ---------------------------------------------------------------------------
# Application 1 - k-means
# ---------------------------------------------------------------------------
# Loading data
data(iris)

# Structure 
str(iris)

# Loading package
library(ClusterR)
library(cluster)

# Removing initial label of 
# Species from original dataset
iris_1 = iris[, -5]

# Fitting K-Means clustering Model 
# to training dataset
set.seed(240) # Setting seed
kmeans.re = kmeans(iris_1, centers = 3, nstart = 20)
kmeans.re
kmeans.re$centers

# Cluster identification for 
# each observation
kmeans.re$cluster

# Confusion Matrix
cm = table(iris$Species, kmeans.re$cluster)
cm



# ---------------------------------------------------------------------------
# Application 2
# ---------------------------------------------------------------------------

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization


df <- USArrests

df <- na.omit(df)

df <- scale(df)
head(df)


distance <- get_dist(df)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# K-means
k2 <- kmeans(df, centers = 2, nstart = 25)
str(k2)
k2



fviz_cluster(k2, data = df)



df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         state = row.names(USArrests)) %>%
  ggplot(aes(UrbanPop, Murder, color = factor(cluster), label = state)) +
  geom_text()




k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)




set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 15
k.values <- 1:15

# extract wss for 2-15 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")




set.seed(123)

fviz_nbclust(df, kmeans, method = "wss")




# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")



fviz_nbclust(df, kmeans, method = "silhouette")













# ---------------------------------------------------------------------------
# Application 2 - Agrupamento hirarquico
# ---------------------------------------------------------------------------

#https://www.linkedin.com/pulse/agrupamento-hier%C3%A1rquico-com-r-an%C3%A1lise-da-base-de-dados-fl%C3%A1via-gaia-/

# Carregar a base de dados
library(HSAUR2)
data("USairpollution", package = "HSAUR2")

# Calcular as distâncias euclidianas entre as cidades
DE <- dist(USairpollution, method = "euclidean", diag = TRUE, upper = TRUE, p = 2)



# Construir o dendrograma
dendo <- hclust(DE, method = "average")
# Visualizar o dendrograma
plot(dendo)



# Visualizar o dendrograma com todas as folhas no mesmo nível
plot(as.dendrogram(dendo), horiz = TRUE)




# Agrupar as cidades em 5 grupos
grupos <- cutree(dendo, k = 5)

# Visualizar o dendrograma com os grupos
plot(dendo)
abline(h = 5, col = "red")






# Agrupar as cidades cortando a árvore em altura = 100
grupos_altura <- cutree(dendo, h = 100)

# Visualizar o dendrograma com a altura do corte
plot(dendo)
abline(h = 100, col = "blue")




# Visualizar a solução de agrupamento do exercício 5
plot(dendo)
rect.hclust(dendo, k = 5, border = "red")


# Visualizar o dendrograma com as duas versões de cluster
plot(dendo)
rect.hclust(dendo, k = 5, border = "red")
rect.hclust(dendo, h = 100, border = "blue")


# Métodos de ligação a serem testados
metodos <- c("ward.D", "single", "complete", "average", "mcquitty", "median", "centroid")

# Loop para testar diferentes métodos de ligação
for (metodo in metodos) {
  dendrograma_metodo <- hclust(DE, method = metodo)
  plot(dendrograma_metodo, main = paste("Método de ligação:", metodo))
  rect.hclust(dendrograma_metodo, k = 5, border = rainbow(length(metodos)))
}


# Por analise de cluster
# Definir o número de clusters (k)
k <- 3

# Aplicar o algoritmo K-Means aos dados
set.seed(123)  # Definir uma semente para reprodutibilidade
kmeans_result <- kmeans(USairpollution, centers = k)

# Adicionar a coluna de cluster ao conjunto de dados original
USairpollution$Cluster <- kmeans_result$cluster

# Visualizar o conjunto de dados com a coluna de cluster
head(USairpollution)







