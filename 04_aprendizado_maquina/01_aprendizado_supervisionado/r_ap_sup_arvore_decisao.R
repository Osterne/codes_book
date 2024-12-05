# Arvore de Decisão
# Vinícius Osterne




# Carregar as bibliotecas necessárias
library(caret)  # Para divisão dos dados e métricas de avaliação
library(rpart)  # Para construir a árvore de decisão
library(rpart.plot)  # Para visualizar a árvore

# Carregar o conjunto de dados Iris
data(iris)  # O conjunto de dados Iris já vem carregado no R

# Visualizar as primeiras linhas dos dados
head(iris)

# Dividir os dados em treino e teste
set.seed(123)  # Para reprodutibilidade
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# Treinar o modelo de árvore de decisão
# O modelo será treinado usando a variável 'Species' como alvo e as outras variáveis como preditoras
model <- rpart(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
               data = trainData, 
               method = "class")

# Visualizar a árvore de decisão
rpart.plot(model)

# Fazer previsões no conjunto de teste
predictions <- predict(model, testData, type = "class")

# Avaliar o desempenho do modelo
# Gerar a matriz de confusão
confMatrix <- confusionMatrix(predictions, testData$Species)
print(confMatrix)

# Avaliar a acurácia
accuracy <- confMatrix$overall['Accuracy']
print(paste("Acurácia do modelo:", accuracy))

# Verificar a importância das variáveis
print(model$variable.importance)










