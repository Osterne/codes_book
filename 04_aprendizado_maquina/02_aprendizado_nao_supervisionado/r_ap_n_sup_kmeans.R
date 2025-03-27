# K-means
# Vinícius Osterne


# 1. Carregar as bibliotecas necessárias
library(ggplot2)        # Para visualizações gráficas
library(cluster)         # Para visualização de agrupamentos
library(factoextra)      # Para avaliação de agrupamentos (função fviz_cluster)

# 2. Carregar o conjunto de dados Iris
data(iris)  # O conjunto de dados Iris já está carregado no R

# Visualizar as primeiras linhas dos dados
head(iris)

# 3. Pré-processamento dos dados
# Usaremos apenas as variáveis numéricas para o clustering
iris_data <- iris[, 1:4]  # Selecionando apenas as colunas numéricas

# 4. Aplicar o algoritmo K-means
set.seed(123)  # Para reprodutibilidade

# Aplicando o K-means com 3 clusters, já que sabemos que o conjunto de dados Iris tem 3 espécies
kmeans_model <- kmeans(iris_data, centers = 3, nstart = 25)

# Visualizando o resumo do modelo K-means
print(kmeans_model)

# 5. Avaliar o desempenho do agrupamento
# Verificando a tabela de clusters atribuídos
table(kmeans_model$cluster, iris$Species)

# 6. Visualizar os resultados do clustering
# Visualizando o agrupamento em um gráfico 2D (Sepal.Length vs Sepal.Width)
fviz_cluster(kmeans_model, data = iris_data, 
             ellipse.type = "convex", # Exibir elipses convexas
             ggtheme = theme_minimal()) # Tema minimalista

# Visualizando os agrupamentos no gráfico com o número de clusters
# Usando a primeira e segunda variável para visualização
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = factor(kmeans_model$cluster))) + 
  geom_point() +
  labs(title = "K-Means Clustering", color = "Cluster") +
  theme_minimal()

# 7. Avaliação adicional do modelo
# Podemos calcular a soma dos quadrados intra-cluster (total within-cluster sum of squares)
# Esse valor pode ser usado para avaliar a qualidade do agrupamento
print(paste("Total Within-Cluster Sum of Squares: ", kmeans_model$tot.withinss))







