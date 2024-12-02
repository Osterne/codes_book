#============================================================================================
# Script - Regressão Logística
# Vinícius Osterne (www.osterne.com | vinicius@osterne.com)
#============================================================================================




#https://repositorio.enap.gov.br/bitstream/1/3452/3/Aula%202%20-%20Geraldo%20Goes%20e%20Alexandre%20Ywata%20-%20Introdu%C3%A7%C3%A3o%20%C3%A0%20Regress%C3%A3o%20Log%C3%ADstica.pdf
# Distribuição bernoulli, binomial e multinomail
#A ideia básica da regressão logística é assumir que cada
#valor individual $y_i$ corresponde a uma variável aleatória
#de Bernoulli, com probabilidade de sucesso dada por
#$\mathcal{P}(y_i = 1) = p_i$.

#O pulo do gato é fazer com que $\mathcal{P}(y_i = 1) = p_i$
#  dependa das variáveis explicativas $x_{1i}, x_{2i}..., x_{ni}$.

#Uma primeira tentativa seria assumir
#$\mathcal{P}(y_i = 1) = p_i = \beta_0 + \beta_1x_{1i} + ... + \beta_kx_{ki}$
  
#  Mas o problema é que $\mathcal{P}(y_i = 1)$ precisar estar 
#estritamente no intervalo $[0,1]$ e, com a estrutura acima,
#isso não é garantido, pois o termo $\beta_0 + \beta_1x_{1i} + ... + \beta_kx_{ki}$ 
#  pode assumir valores menores do que zero e/ou maiores do que um.

#Para forçar que os valores pertençam a esse intervalo, 
#é considerada a seguinte estrutura:
#  $\mathcal{P}(y_i = 1) = p_i = e^{\beta_0 + \beta_1x_{1i} + ... + \beta_kx_{ki}}$
#  dividido por $1 + e^{\mathcal{P}(y_i = 1) = p_i = \beta_0 + \beta_1x_{1i} + ... + \beta_kx_{ki}}$

  
#============================================================================================
# Aplicação 1 - Regressão Logística Simples
#============================================================================================

# Dados
temp = c(53,56,57,63,66,67,67,67,68,69,70,70,70,70,72,73,75,75,76,76,78,79,80,81)
falha = c(1,1,1,0,0,0,0,0,0,0,0,1,1,1,0,0,0,1,0,0,0,0,0,0)



# Plot dos dados
plot(temp, falha, 
     pch = 20, 
     main = "", xlab="Temperatura (em °F)", ylab = "Falha")



# Ajustando o modelo
model_1 = glm(falha~temp,family=binomial(link="logit"))
summary(model_1)



# Log da razão de chances
exp(coef(model_1)[1]) 
exp(coef(model_1)[2]) #para cada unidade de aumento na temperatura, a chance da falha se reduz em 0.84


# Plot com a linha de ajuste
plot(temp, falha, 
     pch = 20, 
     main = "", xlab="Temperatura (em °F)", ylab = "Falha")
lines(spline(temp, model_1$fitted), col="red", lwd=3)



# Tabela de probabilidades
prob_table = cbind(temp, falha, model_1$fitted.values)
head(prob_table)



# Verificação de ajuste (envolope simulado)
library(hnp)
hnp(model_1,
    pch = 20, 
    main = "", xlab="Quantis teóricos", ylab = "Resíduos")



# Verificação de ajuste (matriz de transição)
prediction = predict(model_1, type = "response")
table(falha, prediction > 0.5)
  










#============================================================================================
# Aplicação 2 - Regressão Logística Múltiplo
#============================================================================================


# Dados
library(aplore3)
data(lowbwt)
df_client = lowbwt
head(df_client)

# Detalhes dos atributos
# low:	Indicador de peso baixo ao nascer (1: > = 2500g, 2: < 2500g)
# age:	Idade da mãe em anos
# lwt:	Peso da mãe no último período menstrual em libras.
# smoke:	Indicador de fumo durante a gravidez (1: Não, 2: Sim)

# Filtrando e renomeando os atributos de interesse
attr = c("low","age", "lwt", "smoke")
df_client = df_client[,attr]
colnames(df_client) = c("peso_bebe","idade_mae", "peso_mae", "fumante_mae")
head(df_client)

# Estatísticas descritivas
summary(df_client)

#Existe uma quantidade maior de bebês com peso acima de 2500 (69%) e de mães que não são fumantes.
# Além disso, a idade da mãe tem média de 23 anos (valor próximo da mediana, o que indica uma distribuição)
# simétrica para essa variável. O pesso da mãe tem média de 129 kg.


# Tabela 2x2 para variáveis categóricas
table(df_client$peso_bebe, df_client$fumante_mae)


# Por essa breve análise descritiva, já podemos notar que mães que não fumaram durante a gravidez
# têm filhos com peso maior que 2500g. Podemos validar essa afirmação por meio de um teste qui-quadrado.
# A hipótese nula nesse caso será "não existe associação entre peso do bebê e indicador de fumo da mãe 
# durante a gravidez", ou seja, as duas variáveis são independentes.

# Teste qui-quadrado
teste_qui = table(df_client$peso_bebe, df_client$fumante_mae)
chisq.test(teste_qui)
# Rejeitamos a hipótese nula: existe associação entre fumo da mãe e peso da criança



# Boxplot para os atributos numéricos
par(mfrow=c(1,2))
boxplot(idade_mae ~ peso_bebe, data = df_client, 
        xlab = "Peso do bebê (em gramas)", ylab = "Idade da mãe (em anos)")
boxplot(peso_mae ~ peso_bebe, data = df_client, 
        xlab = "Peso do bebê (em gramas)", ylab = "Peso da mãe (em libras)")

#A idade da mãe apresenta maior variabilidade para bebes
# com menos de 2500g.


# Tranformando as variáveis em dummies e convertendo em zeros e uns
as.factor(df_client$peso_bebe)
as.factor(df_client$fumante_mae)
levels(df_client$peso_bebe) = c(0, 1)
levels(df_client$fumante_mae) = c(0, 1)



# Neste tutorial será mostrado como estimar a probabilidade de um bebê ter um peso menor que 2500 g ao nascer, 
# a partir da idade da mãe, do peso dela no último período menstrual e se ela fumou durante a gravidez. 
# O comando geral para a regressão logística é a partir da função glm usando o argumento family = binomial, 
# que por default usa a função de ligação logito. O primeiro passo será calcular apenas a probabilidade de 
# um bebê nascer com peso baixo a partir da idade da mãe.

# Ajustando o modelo de regressão
model_1 = glm(peso_bebe ~ idade_mae, family = binomial, data = df_client)
summary(model_1)

# Não é significativa, vale usar teste de Wald para avaliar



# Ajustando o modelo de regressão
model_2 = glm(peso_bebe ~ peso_mae, family = binomial, data = df_client)
summary(model_2)



# Ajustando o modelo de regressão
model_3 = glm(peso_bebe ~ fumante_mae, family = binomial, data = df_client)
summary(model_3)


# Ajustando o modelo de regressão
model_4 = glm(peso_bebe ~ peso_mae + fumante_mae, family = binomial, data = df_client)
summary(model_4)

# A partir dessa saída, pode-se concluir que as probabilidades ajustadas de ter um filho com menor peso terão:
# intercepto nulo (p=0.4345)
# serão decrescentes conforme o peso da mãe aumenta (p=0.0287)
# aumentam se a mãe é fumante (p=0.0372)
# A seguir, será utilizada a tabela ANOVA com testes da máxima verossimilhança para verificar a significância do modelo completo.

anova(model_4, test = "Chisq")

# IC
cbind(Estimativa = coef(model_4), confint(model_4))


# Análise de resíduos (Pearson)
res_model_4_pearson = residuals(model_4, type = "pearson")
indices = sample(1:nrow(df_client)) #selecionando as observações aleatoriamente 
plot(indices, res_model_4_pearson, 
     xlab = "Unidade amostral", ylab = "Resíduos de Pearson", 
     ylim = c(-3,3), pch = 20, 
     col=ifelse(res_model_4_pearson > 2, 'red', 'black'))

# Os resíduos parecem bem comportados



# Teste de ajuste
# O p-valor da estatística qui-quadrado de Pearson para o ajuste do modelo pode ser calculada pelo comando
#pchisq(sum(res_model_4_pearson^2), df = model_4$df.residual, lower.tail = F)



# Análise de resíduos (Deviance)
res_model_4_deviance = residuals(model_4, type = "deviance")
indices = sample(1:nrow(df_client)) #selecionando as observações aleatoriamente 
plot(indices, res_model_4_deviance, 
     xlab = "Unidade amostral", ylab = "Resíduos deviance", 
     ylim = c(-3,3), pch = 20, 
     col=ifelse(res_model_4_deviance > 2, 'red', 'black'))

# Os resíduos parecem bem comportados (valor-p do teste)
pchisq(sum(res_model_4_deviance^2), df = model_4$df.residual, lower.tail = F)
#Nesse caso, considerando um nível de significância de 5%, a hipótese de adequação do 
#modelo é rejeitada.



# Predição
# critério de separação é de 0.3
pred = predict(model_4, type = "response")
cm = table(df_client$peso_bebe, pred > 0.3)
cm
sens = round(cm[2,2]/(cm[2,1] + cm[2,2]), 2)
sens


# Curva ROC
library(pROC)
curva_roc = roc(df_client$peso_bebe, fitted(model_4))
plot(curva_roc, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1, 0.2),
     max.auc.polygon=TRUE, auc.polygon.col="gray70",
     xlab="Especificidade", ylab="Sensibilidade")


# Interpretação dos Resultados


# Intercepto igual a zero:
#O logito do intercepto pode ser entendido como a probabilidade de ocorrência do evento quando todas as covariáveis assumem o valor 0
#, mesmo que seja uma variável que o valor zero não faça sentido. No caso do modelo ajustado neste tutorial, seria um caso em que a mãe teria peso 0
#(impossível) e não fumou durante a gestão. Como o intercepto teve valor 0
#, portanto, a probabilidade de uma mãe com peso hipotético de 0
#libra e que não fuma seria de

exp(coef(model_4))


#Precisão: https://smolski.github.io/livroavancado/reglog.html
#https://stats.oarc.ucla.edu/r/dae/multinomial-logistic-regression/
#https://bookdown.org/chua/ber642_advanced_regression/multinomial-logistic-regression.html
#https://rstudio-pubs-static.s3.amazonaws.com/533804_c2fc5294e6eb407facf66d81a4cadafa.html
#http://lea.estatistica.ccet.ufrn.br/tutoriais/regressao-logistica.html

library(VGAM)

# Load the iris dataset 
data(iris) 
df_client = iris
head(df_client)



