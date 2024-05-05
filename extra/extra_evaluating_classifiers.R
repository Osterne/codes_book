#=================================================================================================
#Code: Classification methods 
#Author: Vinícius Osterne (vinicius@osterne -- www.osterne.com)
#=================================================================================================


#=================================================================================================
# Clearing the memory
#=================================================================================================
rm(list=ls())




#=================================================================================================
# Package
#=================================================================================================
library(tidyverse)
library(MASS)
library(caret)
library(class)
library(caret)
library(randomForest)



#=================================================================================================
# Dataset
#=================================================================================================

#Loading dataset

#data = read.csv(choose.files(), header = T, sep = ",", dec = ".") # txt
data = read.csv(choose.files(), header = T, sep = ";", dec = ",") # csv1
#data = read.csv(choose.files(), header = T, sep = ",", dec = ".") # csv2
head(data)
dim(data)



col.class = 23 #column with the class
n = length(data[,1])
id = round(0.70*n,0)
sel.id = sample(1:n, id)
train = data[sel.id,]
test = data[-sel.id,]
y.train = train[,col.class]
x.train = train[,-col.class]
y.test = test[,col.class]
x.test = test[,-col.class]
head(data)





#=================================================================================================
# Data organization
#=================================================================================================

#Removing variables
data = data[,-c(1, 2, 10)]
#head(data)
factor(data$tipo_pagamento)

#Removing NAs
data = na.omit(data)


#Type of data
levels(data$resp_saiu_do_plano) = c("0", "1")
data = as.data.frame(cbind(data$resp_saiu_do_plano,
                        data$data_primeiro_procedimento, data$data_ultimo_procedimento, data$tempo_ultima_consulta, 
                        data$data_de_inclusao, data$idade, factor(data$sexo), factor(data$estado_civil), 
                        factor(data$estado), factor(data$opcional_odonto), factor(data$tipo_pagamento), data$num_dependentes, 
                        data$custo_total_beneficiario, data$num_consultas, data$num_exames_cirugias, data$num_outros_procedimentos, 
                        data$num_internacoes, data$num_atendimento_todos))
#head(data)




#=================================================================================================
#Descriptive analysis
#=================================================================================================
#prop.table(y)









#=================================================================================================
#Loop for evaluate the classifiers
#=================================================================================================

accuracy = 0
precision = 0 
recall = 0
f1score = 0


for(i in 1:100){


  
#Randomly selecting individuals--------------------------------------------------------------------
  
n = length(data[,1])
id = round(0.80*n,0)
sel.id = sample(1:n, id)







#Original data--------------------------------------------------------------------------------------

original.data.train = data[sel.id,]
original.data.test = data[-sel.id,]

col.class = 23
original.data.x.train = data.matrix(original.data.train[,-col.class])
original.data.x.test = data.frame(original.data.test[,-col.class])

original.data.y.train = original.data.train[,col.class]
original.data.y.test = original.data.test[,col.class]






#Box-cox data transformation-------------------------------------------------------------------------

preprocessParams = preProcess(data[,2:18], method=c("BoxCox"))
boxcox.data = predict(preprocessParams, data[,2:18])

boxcox.data.train = boxcox.data[sel.id,]
boxcox.data.test = boxcox.data[-sel.id,]

boxcox.data.x.train = data.matrix(boxcox.data.train)
boxcox.data.x.test = data.frame(boxcox.data.test)

boxcox.data.y.train = original.data.train[,1]
boxcox.data.y.test = original.data.test[,1]




#PCA data transformation-------------------------------------------------------------------------------

pca.model = prcomp(data[,2:18], center = TRUE,scale. = TRUE)
#summary(pca.model)
comp = 7

pca.data = pca.model$x[,seq(1,comp)]

pca.data.train = pca.data[sel.id,]
pca.data.test = pca.data[-sel.id,]

pca.data.x.train = data.matrix(pca.data.train)
pca.data.x.test = data.frame(pca.data.test)

pca.data.y.train = original.data.train[,1]
pca.data.y.test = original.data.test[,1]








#LDA data transformation-----------------------------------------------------------------------------------

col.class = 23
X = data[,-col.class]
y = data[,col.class]
X = scale(X, center = TRUE, scale = FALSE) #Centering the data on the mean
data = data.frame(cbind(y,X)) #Centering the data on the mean

#Vetor de médias para cada classe classe e geral
mu_k = data %>% group_by(y) %>% summarise_all(mean)
mu_k = mu_k[,-1] #retirando a coluna referente à classe
mu = data %>% summarise_all(mean) %>% as_tibble()
mu = mu[,-col.class] #retirando a coluna referente à classe

# Calculando as matrizes de covariancia de cada classe
data.c1 = data %>% filter(y==0)
cov.c1 = cov(data.c1[,-col.class])
data.c2 = data %>% filter(y==1)
cov.c2 = cov(data.c2[,-col.class])

# Matriz de dispersão intraclasses (Sw)
Sw = dim(data.c1)[1]*cov.c1 + dim(data.c2)[1]*cov.c2 
Sw = Sw + 0.001*diag(c(rep(1, dim(Sw)[1])))

# Matriz de dispersão interclasses
Sb1 = as.matrix(mu_k[1,] - mu)
Sb1 = t(Sb1) %*% Sb1
Sb2 = as.matrix(mu_k[2,] - mu)
Sb2 = t(Sb2) %*% Sb2
Sb = Sb1 + Sb2

# Calculando os autovalores e autovetores
M = solve(Sw)%*%Sb
eigen.value = eigen(M)$values #calculating eigenvalues
eigen.value = Re(eigen(M)$values) #real part
eigen.vector = eigen(M)$vectors #calculating eigenvectors
eigen.vector = Re(eigen(M)$vectors) #real part

# Matriz com os autovetores correspondentes a maior autovalor
acum.variance = cumsum(eigen.value/sum(eigen.value))
Q = eigen.vector[,c(1)]

#Transformação nos dados
lda.X = as.matrix(X)%*%Q
lda.data = data.frame(lda.X, class = y)

lda.data.train = lda.data[sel.id,]
lda.data.test = lda.data[-sel.id,]

lda.data.x.train = data.matrix(lda.data.train)
lda.data.x.test = data.frame(lda.data.test)

lda.data.y.train = original.data.train[,col.class]
lda.data.y.test = original.data.test[,col.class]









#Loop for evaluate-----------------------------------------------------------------------------------

# Selecting the data
x.train = original.data.x.train
y.train = original.data.y.train
x.test  = original.data.x.test
y.test  = original.data.y.test

#x.train = boxcox.data.x.train
#y.train = boxcox.data.y.train
#x.test  = boxcox.data.x.test
#y.test  = boxcox.data.y.test

#x.train = pca.data.x.train
#y.train = pca.data.y.train
#x.test  = pca.data.x.test
#y.test  = pca.data.y.test

#x.train = lda.data.x.train
#y.train = lda.data.y.train
#x.test  = lda.data.x.test
#y.test  = lda.data.y.test


train = as.data.frame(cbind(y.train, x.train))
test = as.data.frame(cbind(y.test, x.test))


# KNN classifier (OK)
#library(class)
#library(caret)
#model = knn(train = x.train, test = x.test, cl = y.train, k = 5)
#y.pred = model
#output = confusionMatrix(factor(y.pred), factor(y.test), positive = "1", mode = "prec_recall")
#output$overall["Accuracy"]




# Decision tree classifier (OK)
#library(caret)
#library(rpart)
pruneControl = rpart.control(minsplit = 15, minbucket = 5)
model = rpart(y.train ~ ., data = train, control = pruneControl)
y.pred = predict(model, x.test)
y.pred = ifelse(y.pred >= 0.5, 1, 0)
output = confusionMatrix(factor(y.pred), factor(y.test), positive = "1", mode = "prec_recall") 
output$overall["Accuracy"]



# Logistic regression classifier (ok)
#model = glm(as.factor(y.train) ~ ., data = train, family = binomial("logit"))
#y.pred = predict(model, x.test, type = "response")
#y.pred = ifelse(y.pred > 0.5, 1, 0)
#output = confusionMatrix(factor(y.pred), factor(y.test), positive = "1", mode = "prec_recall") 
#output$overall["Accuracy"]

















# Random forest classifier (ok)
#library(caret)
#library(randomForest)
#model = randomForest(as.factor(y.train) ~ ., data = train, ntree = 101)
#y.pred = predict(model, x.test)
#output = confusionMatrix(factor(y.pred), factor(y.test), positive = "1", mode = "prec_recall") 
#output$overall["Accuracy"]





# XGBoost classifier (ok)
#library(xgboost)
#library(caret)
#model = xgboost(data = x.train, label = y.train, max.depth = 10, eta = 0.1, nthread = 4, nrounds = 100, objective = "binary:logistic")
#y.pred = predict(model, data.matrix(x.test))
#y.pred = ifelse(y.pred > 0.5, 1, 0)
#output = confusionMatrix(factor(y.pred), factor(y.test), positive = "1", mode = "prec_recall") 
#output$overall["Accuracy"]







# Naive Bayes classifier
#library(e1071)
#model = naiveBayes(y.train ~ ., data = train)
#y.pred = predict(model, x.test)
#output = confusionMatrix(factor(y.pred), factor(y.test), positive = "1", mode = "prec_recall") 
#output2$overall["Accuracy"]




# Least square classifier
#model = 
#output = confusionMatrix(factor(y.pred), factor(y.test), positive = "1", mode = "prec_recall") 
#output$overall["Accuracy"]






# Adaboost classifier
#library(adabag)
#model = boosting(y.train ~ ., data = train)
#y.pred = predict.boosting(model, dados_teste[, -1])
#output = confusionMatrix(factor(y.pred), factor(y.test), positive = "1", mode = "prec_recall") 
#output$overall["Accuracy"]






# Gradient boosting classifier
#library(caret)
#library(gbm)
#model = gbm(y.train ~ ., data = train, n.trees = 1000, n.minobsinnode = 30, distribution = "bernoulli")
#y.pred = predict.gbm(model, x.test, type = "response")
#y.pred = ifelse(y.pred >= 0.5, 1, 0)
#output = confusionMatrix(factor(y.pred), factor(y.test), positive = "1", mode = "prec_recall") 
#output$overall["Accuracy"]





# Neural network classifier
#library(caTools)
#library(h2o)
#library(caret)
#model = h2o.deeplearning(y = "classe", training_frame = as.h2o(dados_treinamento), activation = "Rectifier", hidden = c(22, 22), epochs = 100)
#t.pred = h2o.predict(mod_rn, newdata = as.h2o(dados_teste[, -1]))
#output = confusionMatrix(factor(y.pred), factor(y.test), positive = "1", mode = "prec_recall") 
#output$overall["Accuracy"]





# Metrics for classifier
accuracy[i] = output$overall["Accuracy"]
precision[i] = output$byClass["Precision"]
recall[i] = output$byClass["Recall"]
f1score[i] = output$byClass["F1"]

}

metrics = 100*c(mean(accuracy), mean(precision), mean(recall), mean(f1score))
round(metrics, 4)

































#=================================================================================================
# Interesting links
#=================================================================================================
#https://rpubs.com/Godinho/714375










