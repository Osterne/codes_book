#=================================================================================================
#Code: Least squares classifier
#Author: Vinicius Osterne (vinicius@osterne -- www.osterne.com)
#=================================================================================================




#=================================================================================================
#Clearing the memory
#=================================================================================================

rm(list=ls())

#data = read.table(choose.files(), header = T, sep = ";", dec = ",")
#head(data)
#y = data[,22]
#X = data[,-c(6,7,22,23)]
#train.perc = 0.8
#cov.matrix = "naive-bayes"








#=================================================================================================
# Least squares classifier - Function
#=================================================================================================
#train.perc = 0.8

lsClassifier = function(y, X, train.perc){

  
  
#-------------------------------------------------------------------------------------------------
# Step 1: Preparing the data
#-------------------------------------------------------------------------------------------------

natt = dim(X)[2] # number of attributes
n = dim(X)[1] # number of observations

# Modifications for class:
class = factor(y) #class for classification
class = as.numeric(class)
nclass = max(class)

# Data final:
data = data.frame(class, X) # data for classification
col.x = seq(2,length(data[1,]),1) # number of columms for X
col.y = c(1) #number of columns for y

#Selecting individuals for train
n = length(y)
ind.train = round(train.perc*n,0)
ind.sample = sample(1:n, ind.train)

#Train data
train.data = data[ind.sample,]
x.train = train.data[,col.x]
y.train = train.data[,col.y]

#Test data
test.data = data[-ind.sample,]
x.test = test.data[,col.x]
y.test = test.data[,col.y]


#Changing y
aux = as.numeric(y)
aux = replace(aux, aux == 1, "100")
aux = replace(aux, aux == 2, "010")
aux = replace(aux, aux == 3, "001")
library(stringr)
aux1 = aux %>% str_sub(1,1)
aux2 = aux %>% str_sub(2,2)
aux3 = aux %>% str_sub(3,3)

aux1 = as.numeric(aux1)
aux2 = as.numeric(aux2)
aux3 = as.numeric(aux3)

Y = data.matrix(cbind(aux1, aux2, aux3))



#------------------------------------------------------------------------------------------------
# Step 2: Estimating W
#-------------------------------------------------------------------------------------------------

  
#X = matrix(cbind(as.numeric(X[,1]), as.numeric(X[,2]), as.numeric(X[,3]), as.numeric(X[,4]), as.numeric(X[,5]), 
#                 as.numeric(X[,6]), as.numeric(X[,7]), as.numeric(X[,8]), as.numeric(X[,9]), as.numeric(X[,10]),
#                 as.numeric(X[,11]), as.numeric(X[,12]), as.numeric(X[,13]), as.numeric(X[,14]), as.numeric(X[,15]), 
#                 as.numeric(X[,16]), as.numeric(X[,17]), as.numeric(X[,18]), as.numeric(X[,19]), as.numeric(X[,20]), as.numeric(X[,21])), 
#           nrow = dim(X)[1], ncol = dim(X)[2])

#Y = matrix(cbind(as.numeric(X[,1]), as.numeric(X[,2]), as.numeric(X[,3])),
#           nrow = dim(Y)[1], ncol = dim(Y)[2])


Y = matrix(cbind(as.numeric(Y[,1]), as.numeric(Y[,2]), as.numeric(Y[,3])), nrow = dim(Y)[1], ncol = dim(Y)[2])
Y = t(Y)

X = t(X)

lambda = 0.01
const = lambda*diag(rep(1), dim(X)[1])
W = Y%*%t(X) %*% solve(X%*%t(X) + const) 

#dim(Y)
#dim(X)




#-------------------------------------------------------------------------------------------------
#Step 3: Function for evaluate the classifier
#-------------------------------------------------------------------------------------------------
#obs=1
out1 = 0
y.class = 0
for(obs in 1:length(test.data[,col.y])){
  
  #Test data
  x.new = as.numeric(test.data[obs,col.x])
  y.class = test.data[obs,1]
  
  #Classifier
  out = W%*%x.new 

  #Comparing values
  aux.QL = c(out)
  names(aux.QL) = seq(1,nclass,1)
  aux.comp = names(aux.QL)[which.max(aux.QL)]
  
  #Comparing predicted class with true class
  #out1[obs] = ifelse(y.class == aux.comp, 1, 0)
  out1[obs] = names(aux.QL)[which.max(aux.QL)] #para a matriz de confusão
  
}

verdadeiro = test.data[,1]
estimado = as.numeric(out1)
confusion.matrix = table(verdadeiro, estimado)

ac = sum(diag(confusion.matrix))/sum(confusion.matrix)*100
pr = diag(confusion.matrix)/colSums(confusion.matrix)*100
re = diag(confusion.matrix)/rowSums(confusion.matrix)*100
f1 = 2*(pr*re)/(pr + re)

ac1 = sum(confusion.matrix[1,1])/sum(confusion.matrix[,1])*100
ac2 = sum(confusion.matrix[2,2])/sum(confusion.matrix[,2])*100

#Output
mylist = list("ac" = ac, "pr" = pr, "re" = re, "f1" = f1, "ac1" = ac1, "ac2" = ac2)
return(mylist)

}





#-------------------------------------------------------------------------------------------------
# Saving the function
#-------------------------------------------------------------------------------------------------
#save("ls", file="ls.Rdata")
























#=================================================================================================
# Dimensionality Reduction - Principal Components Analysis (PCA)
#=================================================================================================


#Data for reduction ------------------------------------------------------------------------------
data = read.csv(choose.files(), header = T, sep = ";", dec = ",")
head(data)
col.class = 23
y = data[,23]
X = data[,-c(22,23)]
head(X)
dim(X)

#Ste-by-step for PCA in X-------------------------------------------------------------------------
X = scale(X, scale = FALSE) #centering the data on the mean
cov.X = cov(X) #covariance matrix of X
eigen.value = eigen(cov.X)$values #calculating eigenvalues
eigen.vector = eigen(cov.X)$vectors #calculating eigenvectors
acum.variance = cumsum(eigen.value/sum(eigen.value)) #accumulated variance of each eigenvalue
V = eigen.vector[,c(1,2,3,4)] #choosing eigenvalues with greater variability
pca.X = as.matrix(X)%*%V #applying the transformation to the original data
pca.data = data.frame(pca.X, class = factor(y)) #data with PCA
head(pca.data)













#=================================================================================================
# Dimensionality Reduction - Linear Discriminant Analysis (LDA)
#=================================================================================================


#Data for reduction ------------------------------------------------------------------------------
data = read.csv(choose.files(), header = T, sep = ";", dec = ",")
head(data)
data = data[,-22]
head(data)
col.class = 22
y = data[,col.class]
X = data[,-col.class]
head(X)


#Ste-by-step for LDA ------------------------------------------------------------------------------
library(tidyverse) #necessary package
library(MASS) #necessary package 

#Centering the data on the mean
X = scale(X, center = TRUE, scale = FALSE)
data = data.frame(X, y)

#Vetor de médias para cada classe classe e geral
mu_k = data %>% group_by(y) %>% summarise_all(mean)
mu_k = mu_k[,-1] #retirando a coluna referente à classe
mu = data %>% summarise_all(mean) %>% as_tibble()
mu = mu[,-col.class] #retirando a coluna referente à classe

# Calculando as matrizes de covariancia de cada classe
data.c1 = data %>% filter(y==1)
cov.c1 = cov(data.c1[,-22])
data.c2 = data %>% filter(y==2)
cov.c2 = cov(data.c2[,-22])
data.c3 = data %>% filter(y==3)
cov.c3 = cov(data.c3[,-22])

# Matriz de dispersão intraclasses (Sw)
Sw = dim(data.c1)[1]*cov.c1 + dim(data.c2)[1]*cov.c2 + dim(data.c3)[1]*cov.c3 
Sw = Sw + 0.001*diag(c(rep(1, dim(Sw)[1])))

# Matriz de dispersão interclasses
Sb1 = as.matrix(mu_k[1,] - mu)
Sb1 = t(Sb1) %*% Sb1
Sb2 = as.matrix(mu_k[2,] - mu)
Sb2 = t(Sb2) %*% Sb2
Sb3 = as.matrix(mu_k[3,] - mu)
Sb3 = t(Sb3) %*% Sb3
Sb = Sb1 + Sb2 + Sb3

# Calculando os autovalores e autovetores
M = solve(Sw)%*%Sb
eigen.value = eigen(M)$values #calculating eigenvalues
eigen.value = Re(eigen(M)$values) #real part
eigen.vector = eigen(M)$vectors #calculating eigenvectors
eigen.vector = Re(eigen(M)$vectors) #real part

# Matriz com os autovetores correspondentes a maior autovalor
acum.variance = cumsum(eigen.value/sum(eigen.value))
Q = eigen.vector[,c(1,2)]

#Transformação nos dados
lda.X = as.matrix(X)%*%Q
lda.data = data.frame(lda.X, class = factor(y))
head(lda.data)









#=================================================================================================
# Box-Cox transformation
#=================================================================================================
data = read.csv(choose.files(), header = T, sep = ";", dec = ",")
data = data[,-22]
col.class = 22
y = data[,col.class]
X = data[,-col.class]

# Original data
head(data)
y = data[,22]
X = data[,-22]

#Transformation
library(caret)
preprocessParams = preProcess(X[,1:21], method=c("BoxCox"))
boxcox.data = predict(preprocessParams, X[,1:21])
head(boxcox.data)
head(X)
















#=================================================================================================
# Evaluating the classifier - Accuracy
#=================================================================================================
data = read.csv(choose.files(), header = T, sep = ";", dec = ",")
head(data)
data = data[,-22]
head(data)
col.class = 22
y = data[,col.class]
X = data[,-col.class]
head(X)


#Dados originais
head(data)
y = data[,22]
X = data[,-22]



#Para fazer o PCA
head(pca.data)
y = as.numeric(pca.data$class)
X = matrix(cbind(as.numeric(pca.data[,1]), as.numeric(pca.data[,2]), as.numeric(pca.data[,3]), as.numeric(pca.data[,4])), nrow = 2126, ncol = 4)



#Para fazer a LDA
head(lda.data)
y = as.numeric(lda.data$class)
X = matrix(cbind(as.numeric(lda.data[,1]), as.numeric(lda.data[,2])), nrow = 2126, ncol = 2)



# Data with Box-Cox
head(boxcox.data)
y = data[,22]
X = boxcox.data







# Reading data set
data = read.table(choose.files(), header = F, sep = ",", dec = ".")
head(data)
data = na.omit(data)

# Change the attribute V2
data$V2[data$V2 == "Female"] = 1
data$V2[data$V2 == "Male"] = 2
data$V2 = as.numeric(data$V2)

# Change the attribute V2
data$V11[data$V11 == "1"] = 1
data$V11[data$V11 =="2"] = 2
data$V11 = as.numeric(data$V11)



# Train and test data
head(data)
X = data[,-11]
y = data[,11]












#Comparando os modelos
out1 = 0
out2 = 0
for(i in 1:100){
  out1[i] = lsClassifier(y = y, X = X, train.perc = 0.8)$ac1
  out2[i] = lsClassifier(y = y, X = X, train.perc = 0.8)$ac2
  #out[i] = gaussian.quadratic.classifier(y = y, X = X, train.perc = 0.8, cov.matrix = "naive-bayes")
  #out[i] = gaussian.quadratic.classifier(y = y, X = X, train.perc = 0.8, cov.matrix = "white-noise")
  #out[i] = gaussian.quadratic.classifier(y = y, X = X, train.perc = 0.8, cov.matrix = "pooled")
  #out[i] = gaussian.quadratic.classifier(y = y, X = X, train.perc = 0.8, cov.matrix = "friedman")
  #out[i] = gaussian.quadratic.classifier(y = y, X = X, train.perc = 0.8, cov.matrix = "tikhonov")
}

out1 = c(round(min(out), 2), round(mean(out), 2), round(max(out), 2), round(sd(out), 2))
out1







#=================================================================================================
# Evaluating the classifier - Precision, recall and f1-score
#=================================================================================================

out1 = c()
out2 = c()
out3 = c()
out4 = c()
out5 = c()
out6 = c()
out7 = c()
out8 = c()
out9 = c()
out10 = c()
for(i in 1:100){
  out1[i] = lsClassifier(y = y, X = X, train.perc = 0.8)$ac[1]
  out2[i] = lsClassifier(y = y, X = X, train.perc = 0.8)$pr[1]
  out3[i] = lsClassifier(y = y, X = X, train.perc = 0.8)$pr[2]
  out4[i] = lsClassifier(y = y, X = X, train.perc = 0.8)$pr[3]
  out5[i] = lsClassifier(y = y, X = X, train.perc = 0.8)$re[1]
  out6[i] = lsClassifier(y = y, X = X, train.perc = 0.8)$re[2]
  out7[i] = lsClassifier(y = y, X = X, train.perc = 0.8)$re[3]
  out8[i] = lsClassifier(y = y, X = X, train.perc = 0.8)$f1[1]
  out9[i] = lsClassifier(y = y, X = X, train.perc = 0.8)$f1[2]
  out10[i] = lsClassifier(y = y, X = X, train.perc = 0.8)$f1[3]
}

out = c(round(mean(out1), 2), round(mean(out2), 2), round(mean(out3), 2), round(mean(out4), 2), round(mean(out5), 2),
        round(mean(out6), 2), round(mean(out7), 2), round(mean(out8), 2), round(mean(out9), 2), round(mean(out10), 2))
out





