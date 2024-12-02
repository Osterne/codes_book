# Redes Neurais
# Vinícius Osterne




# 1. Carregar as bibliotecas necessárias
library(caret)       # Para divisão dos dados e métricas de avaliação
library(nnet)        # Para redes neurais (função neuralnet)
library(ggplot2)     # Para visualização (opcional)

# 2. Carregar o conjunto de dados Iris
data(iris)  # O conjunto de dados Iris já vem carregado no R

# Visualizar as primeiras linhas dos dados
head(iris)

# 3. Dividir os dados em treino e teste
set.seed(123)  # Para reprodutibilidade
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# 4. Treinar a rede neural
# A função 'multinom' é usada para treinar redes neurais para problemas de classificação múltipla
nn_model <- multinom(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                     data = trainData)

# 5. Resumo do modelo
summary(nn_model)

# 6. Fazer previsões no conjunto de teste
predictions <- predict(nn_model, testData)

# 7. Avaliar o desempenho do modelo
# Gerar a matriz de confusão
confMatrix <- confusionMatrix(predictions, testData$Species)
print(confMatrix)

# 8. Avaliar a acurácia
accuracy <- confMatrix$overall['Accuracy']
print(paste("Acurácia do modelo:", accuracy))

# 9. Visualizar os pesos da rede neural (opcional)
# Para redes neurais mais simples, podemos tentar visualizar os pesos de cada camada
# No caso do modelo multinomial, esta parte é mais complexa, mas você pode inspecionar as funções de cada variável
coef(nn_model)

# 10. Fim do script











