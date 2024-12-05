# Random Forest
# Vinícius Osterne




# 1. Carregar as bibliotecas necessárias
library(caret)        # Para divisão dos dados e métricas de avaliação
library(randomForest) # Para construir o modelo Random Forest

# 2. Carregar o conjunto de dados Iris
data(iris)  # O conjunto de dados Iris já vem carregado no R

# Visualizar as primeiras linhas dos dados
head(iris)

# 3. Dividir os dados em treino e teste
set.seed(123)  # Para reprodutibilidade
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# 4. Treinar o modelo Random Forest
# O modelo será treinado com a variável 'Species' como alvo e as outras variáveis como preditoras
rf_model <- randomForest(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                         data = trainData, 
                         importance = TRUE)

# 5. Visualizar o modelo
print(rf_model)

# 6. Fazer previsões no conjunto de teste
predictions <- predict(rf_model, testData)

# 7. Avaliar o desempenho do modelo
# Gerar a matriz de confusão
confMatrix <- confusionMatrix(predictions, testData$Species)
print(confMatrix)

# 8. Avaliar a acurácia
accuracy <- confMatrix$overall['Accuracy']
print(paste("Acurácia do modelo:", accuracy))

# 9. Visualizar a importância das variáveis
print(rf_model$importance)
# Gráfico de importância das variáveis
varImpPlot(rf_model)

# 10. Fim do script







