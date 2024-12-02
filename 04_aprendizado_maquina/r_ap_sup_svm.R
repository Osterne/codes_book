# SVM
# Vinícius Osterne



# 1. Carregar as bibliotecas necessárias
library(caret)        # Para divisão dos dados e métricas de avaliação
library(e1071)         # Para usar o SVM
library(ggplot2)       # Para visualizações (opcional)

# 2. Carregar o conjunto de dados Iris
data(iris)  # O conjunto de dados Iris já vem carregado no R

# Visualizar as primeiras linhas dos dados
head(iris)

# 3. Dividir os dados em treino e teste
set.seed(123)  # Para reprodutibilidade
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# 4. Treinar o modelo SVM
# O modelo será treinado com a variável 'Species' como alvo e as outras variáveis como preditoras
svm_model <- svm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                 data = trainData, 
                 kernel = "radial",    # Usando o kernel radial (RBF)
                 cost = 1,             # Parâmetro de custo
                 scale = TRUE)         # Normalizar as variáveis

# 5. Resumo do modelo
summary(svm_model)

# 6. Fazer previsões no conjunto de teste
predictions <- predict(svm_model, testData)

# 7. Avaliar o desempenho do modelo
# Gerar a matriz de confusão
confMatrix <- confusionMatrix(predictions, testData$Species)
print(confMatrix)

# 8. Avaliar a acurácia
accuracy <- confMatrix$overall['Accuracy']
print(paste("Acurácia do modelo:", accuracy))

# 9. Visualizar os resultados (opcional)
# Visualizar o SVM em um gráfico de 2D para observar as margens (considerando apenas as duas primeiras variáveis)
svm_plot_data <- data.frame(x = trainData$Sepal.Length, y = trainData$Sepal.Width, Species = trainData$Species)
ggplot(svm_plot_data, aes(x = x, y = y, color = Species)) + 
  geom_point() + 
  ggtitle("Visualização SVM (Sepal.Length vs Sepal.Width)") +
  theme_minimal()

# 10. Fim do script







