# Regressão Múltipla
# Vinicius Osterne (www.osterne.com)


# ----------------------------------------------------------------------------
# Exemplo 1
# ----------------------------------------------------------------------------

# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(car)
library(MASS)

# Carregar conjunto de dados mtcars
data(mtcars)

# Exibir primeiras linhas do dataset
head(mtcars)

# Resumo estatístico das variáveis
summary(mtcars)

# Matriz de correlação para verificar multicolinearidade
cor(mtcars)

# Visualização da relação entre as variáveis
pairs(mtcars, main = "Matriz de Dispersão")

# Ajustar um modelo de regressão múltipla
modelo <- lm(mpg ~ cyl + disp + hp + wt, data = mtcars)

# Resumo do modelo ajustado
summary(modelo)



# Diagnóstico do modelo

# 1. Análise dos resíduos
par(mfrow = c(2, 2)) # Configurar área de plotagem
plot(modelo) # Gráficos diagnósticos

# 2. Teste de normalidade dos resíduos
shapiro.test(modelo$residuals)
# h0: Os resíduos seguem uma distribuição normal.
# h1: Os resíduos não seguem uma distribuição normal.

# 3. Teste de homocedasticidade (Teste de Breusch-Pagan)
library(lmtest)
bptest(modelo)
# h0: Os resíduos apresentam homocedasticidade (variância constante).
# h1: Os resíduos apresentam heterocedasticidade (variância não constante)

# 4. Teste de multicolinearidade (VIF - Variance Inflation Factor)
vif(modelo)
#VIF < 1 → Sem correlação entre a variável e as outras.
#1 ≤ VIF < 5 → Correlação moderada, geralmente aceitável.
#VIF ≥ 5 → Forte multicolinearidade, pode ser problemático.
#VIF > 10 → Multicolinearidade severa, pode causar instabilidade nos coeficientes do modelo.

# Ajuste do modelo com seleção de variáveis (usando stepwise AIC)
modelo_step <- stepAIC(modelo, direction = "both")

# Resumo do modelo otimizado
summary(modelo_step)

# Exibir os coeficientes finais do modelo ajustado
coef(modelo_step)
















# ----------------------------------------------------------------------------
# Exemplo 2
# ----------------------------------------------------------------------------

# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(car)
library(MASS)
library(lmtest)

# Carregar conjunto de dados Boston Housing
data(Boston)

# Exibir primeiras linhas do dataset
head(Boston)

# Resumo estatístico das variáveis
summary(Boston)

# Matriz de correlação para verificar multicolinearidade
cor(Boston)

# Visualização da relação entre as variáveis
pairs(Boston, main = "Matriz de Dispersão")

# Ajustar um modelo de regressão múltipla
modelo <- lm(medv ~ crim + rm + age + tax + lstat, data = Boston)

# Resumo do modelo ajustado
summary(modelo)

# Diagnóstico do modelo

# 1. Análise dos resíduos
par(mfrow = c(2, 2)) # Configurar área de plotagem
plot(modelo) # Gráficos diagnósticos

# 2. Teste de normalidade dos resíduos
shapiro.test(modelo$residuals)

# 3. Teste de homocedasticidade (Teste de Breusch-Pagan)
bptest(modelo)

# 4. Teste de multicolinearidade (VIF - Variance Inflation Factor)
vif(modelo)

# Ajuste do modelo com seleção de variáveis (usando stepwise AIC)
modelo_step <- stepAIC(modelo, direction = "both")

# Resumo do modelo otimizado
summary(modelo_step)

# Exibir os coeficientes finais do modelo ajustado
coef(modelo_step)














# ----------------------------------------------------------------------------
# Exemplo 3
# ----------------------------------------------------------------------------



# Carregar pacotes necessários
library(ggplot2)
library(dplyr)
library(car)
library(MASS)
library(lmtest)

# Carregar conjunto de dados iris
data(iris)

# Exibir primeiras linhas do dataset
head(iris)

# Resumo estatístico das variáveis
summary(iris)

# Matriz de correlação (apenas para variáveis numéricas)
cor(iris[, 1:4])

# Visualização da relação entre as variáveis
pairs(iris[, 1:4], main = "Matriz de Dispersão - Iris")

# Ajustar um modelo de regressão múltipla para prever o comprimento da pétala
modelo <- lm(Petal.Length ~ Sepal.Length + Sepal.Width + Petal.Width, data = iris)

# Resumo do modelo ajustado
summary(modelo)

# Diagnóstico do modelo

# 1. Análise dos resíduos
par(mfrow = c(2, 2)) # Configurar área de plotagem
plot(modelo) # Gráficos diagnósticos

# 2. Teste de normalidade dos resíduos
shapiro.test(modelo$residuals)

# 3. Teste de homocedasticidade (Teste de Breusch-Pagan)
bptest(modelo)

# 4. Teste de multicolinearidade (VIF - Variance Inflation Factor)
vif(modelo)

# Ajuste do modelo com seleção de variáveis (usando stepwise AIC)
modelo_step <- stepAIC(modelo, direction = "both")

# Resumo do modelo otimizado
summary(modelo_step)

# Exibir os coeficientes finais do modelo ajustado
coef(modelo_step)









