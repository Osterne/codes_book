# Acervo completo sobre Estatística e Ciência de Dados
# Clusterização Hierárquica
# Vinícius Osterne




# Carregar as bibliotecas necessárias
library(ggplot2)        # Para visualizações gráficas
library(cluster)         # Para visualização dos agrupamentos
library(factoextra)      # Para avaliar os agrupamentos e visualizações de cluster
library(dendextend)      # Para manipulação e visualização de dendrogramas

# Carregar o conjunto de dados Iris
data(iris)  # O conjunto de dados Iris já está carregado no R

# Visualizar as primeiras linhas dos dados
head(iris)

# Pré-processamento dos dados
# Usaremos apenas as variáveis numéricas para o clustering
iris_data <- iris[, 1:4]  # Selecionando apenas as colunas numéricas

# 4. Calcular a matriz de distâncias
# Usaremos a distância euclidiana para calcular as distâncias entre as amostras
dist_matrix <- dist(iris_data, method = "euclidean")

# 5. Aplicar o algoritmo de Clusterização Hierárquica
# Usaremos o método de ligação (linkage) "complete", mas existem outros como "single", "average", etc.
hclust_model <- hclust(dist_matrix, method = "complete")

# 6. Visualizar o dendrograma
# O dendrograma nos mostra como os clusters são formados e o nível de aglomeração
plot(hclust_model, main = "Dendrograma da Clusterização Hierárquica", 
     xlab = "", ylab = "Distância Euclidiana", sub = "", cex = 0.9)

# 7. Cortar o dendrograma para definir os clusters
# Vamos cortar o dendrograma em 3 clusters
clusters <- cutree(hclust_model, k = 3)

# 8. Avaliar o desempenho do agrupamento
# Comparar os clusters formados com as classes reais (Species)
table(clusters, iris$Species)

# 9. Visualizar os resultados do clustering
# Visualizar os clusters em um gráfico 2D usando a primeira e a segunda variável (Sepal.Length e Sepal.Width)
iris_clustered <- data.frame(iris, Cluster = as.factor(clusters))

ggplot(iris_clustered, aes(x = Sepal.Length, y = Sepal.Width, color = Cluster)) +
  geom_point() +
  labs(title = "Clusterização Hierárquica", color = "Cluster") +
  theme_minimal()

# 10. Visualizar os clusters em um gráfico 3D (opcional)
# Usando a biblioteca 'scatterplot3d' para visualização 3D (caso queira uma visualização mais elaborada)
if (requireNamespace("scatterplot3d", quietly = TRUE)) {
  library(scatterplot3d)
  scatterplot3d(iris_clustered$Sepal.Length, iris_clustered$Sepal.Width, iris_clustered$Petal.Length,
                color = iris_clustered$Cluster, pch = 16,
                main = "Visualização 3D dos Clusters")
}






