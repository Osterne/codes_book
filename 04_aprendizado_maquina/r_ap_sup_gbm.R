# Arvore de Decisão
# Vinícius Osterne



# 1. Carregar as bibliotecas necessárias
library(caret)        # Para divisão dos dados e métricas de avaliação
library(gbm)          # Para o modelo GBM
library(dplyr)        # Para manipulação de dados (opcional)
library(ggplot2)      # Para visualização (opcional)

# 2. Carregar o conjunto de dados Iris
data(iris)  # O conjunto de dados Iris já vem carregado no R

# Visualizar as primeiras linhas dos dados
head(iris)

# 3. Dividir os dados em treino e teste
set.seed(123)  # Para reprodutibilidade
trainIndex <- createDataPartition(iris$Species, p = 0.7, list = FALSE)
trainData <- iris[trainIndex, ]
testData <- iris[-trainIndex, ]

# 4. Treinar o modelo GBM
# O modelo será treinado com a variável 'Species' como alvo e as outras variáveis como preditoras
gbm_model <- gbm(Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width, 
                 data = trainData, 
                 distribution = "multinomial",  # Para classificação múltipla
                 n.trees = 100,                # Número de árvores
                 interaction.depth = 3,        # Profundidade da árvore
                 shrinkage = 0.01,             # Taxa de aprendizado
                 cv.folds = 5,                 # Validação cruzada
                 verbose = FALSE)

# 5. Resumo do modelo
summary(gbm_model)

# 6. Fazer previsões no conjunto de teste
predictions <- predict(gbm_model, testData, n.trees = 100, type = "response")

# Como temos múltiplas classes, as previsões retornam uma matriz de probabilidades
# Precisamos pegar a classe com maior probabilidade
predicted_class <- colnames(predictions)[apply(predictions, 1, which.max)]

# 7. Avaliar o desempenho do modelo
# Gerar a matriz de confusão
confMatrix <- confusionMatrix(predicted_class, testData$Species)
print(confMatrix)

# 8. Avaliar a acurácia
accuracy <- confMatrix$overall['Accuracy']
print(paste("Acurácia do modelo:", accuracy))

# 9. Visualizar a importância das variáveis
# O GBM fornece a importância das variáveis preditoras
print(gbm_model$var.summary)
# Gráfico de importância das variáveis
varImpPlot(gbm_model)

# 10. Fim do script








