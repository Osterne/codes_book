#=================================================================================================
#Code: Bayes classifier based on gaussian distribution
#Author: Vinícius Osterne (vinicius@osterne -- www.osterne.com)
#=================================================================================================







#=================================================================================================
#Clearing the memory
#=================================================================================================

rm(list=ls())


#data = read.table(choose.files(), header = T, sep = ";", dec = ",")
#head(data)
#y = data[,23]
#X = data[,-c(6,7,22,23)]
#train.perc = 0.8
#cov.matrix = "white-noise"












#=================================================================================================
# Gaussian quadratic classifier function
#=================================================================================================


gaussianClassifier = function(y, X, train.perc, cov.matrix){
  


  
#-------------------------------------------------------------------------------------------------
# Step 1: Preparing the data
#-------------------------------------------------------------------------------------------------

natt = length(X[1,]) # number of attributes
n = length(X[,1]) # number of observations

# Modifications for class:
class = factor(y) #class for classification
class = as.numeric(class)
nclass = max(class)

# Data final:
data = data.frame(class, X) # data for classification
col.x = seq(2,length(data[1,]),1) # number of columms for X
col.y = c(1) #number of columns for y

  

#-------------------------------------------------------------------------------------------------
# Step 2: Separating data in Test and train
#-------------------------------------------------------------------------------------------------

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



# Separing data by class
i=0
x.train.ci = list()
for (i in 1:nclass) {
  x.train.ci[[i]] = subset(train.data, class == i)[,-1]
}




# Size of each class

ni = c()
for (i in 1:nclass) {
  ni[i] = length(x.train.ci[[i]][,1])
}




#-------------------------------------------------------------------------------------------------
#Step 3: Estimating the parameters
#-------------------------------------------------------------------------------------------------


# Prior probability

ptotal = length(train.data[,1])
pwi = c()
for (i in 1:nclass) {
  pwi[i] = length(x.train.ci[[i]][,1])/ptotal
}







#Estimating \mu
mu.ci.est = list()
for (i in 1:nclass) {
  mu.ci.est[[i]] = c(colMeans(x.train.ci[[i]]))
}






# Estimating Cov

#cov.matrix = "cov()"
if(cov.matrix == "standard"){
i=0
cov.ci = list()
for (i in 1:nclass) {
  cov.ci[[i]] = cov(x.train.ci[[i]])
}
}






#cov.matrix = "nayve-bayes"
if(cov.matrix == "naive-bayes"){
  i=0
  aux = list()
  cov.ci = list()
  for (i in 1:nclass) {
    aux[[i]] = diag(cov(x.train.ci[[i]]))
    cov.ci[[i]] = diag(aux[[i]])
  }
}





if(cov.matrix == "tikhonov"){
  i=0
  cov.ci = list()
  lambda = abs(0.01*rnorm(1,0,1))
  for (i in 1:nclass) {
    cov.ci[[i]] = matrix(cbind(cov(x.train.ci[[i]])) , nrow = natt, ncol = natt) + diag(rep(lambda, natt))
  }
}




#cov.matrix = "pooled"
if(cov.matrix == "pooled"){
  i=0
  aux1 = list()
  aux2 = list()
  cov.ci = list()
  for (i in 1:nclass) {
    aux1[[i]] = ni[i]*cov(x.train.ci[[i]])
    aux2 = (1/sum(ni))*Reduce("+", aux1)
  }
  for (i in 1:nclass) {
    cov.ci[[i]] = aux2
  }
}





Spool.aux = list()
S = list()
if(cov.matrix == "friedman"){

  cov.ci = list()
  lambda = abs(0.01*rnorm(1,0,1))
  
  for (i in 1:nclass) {
    Spool.aux[[i]] = ni[i]*cov(x.train.ci[[i]])
    S[[i]] = ni[i]*cov(x.train.ci[[i]])
  }
  Spool = (1/sum(ni))*Reduce("+", Spool.aux)
  
  for (i in 1:nclass) {
    cov.ci[[i]] = ((1-lambda)*S[[i]] + lambda*Spool)/((1-lambda)*ni[i] + lambda*sum(ni))
  }
}



















#-------------------------------------------------------------------------------------------------
#Step 4: Calculating the quantities
#-------------------------------------------------------------------------------------------------


# Logaritmo do deteminante
i=0
log.det.cov.ci = c()
for (i in 1:nclass) {
  log.det.cov.ci[i] = log(det(cov.ci[[i]]))
}
#log.det.cov.ci





# Calculating inv.cov(Omega)
i=0
solve.cov.ci = list()
for (i in 1:nclass) {
  solve.cov.ci[[i]] = solve(cov.ci[[i]])
}










#-------------------------------------------------------------------------------------------------
#Step 5: Function for evaluate the classifier
#-------------------------------------------------------------------------------------------------

out1 = 0
for(obs in 1:length(test.data[,col.y])){
  
  #Test data
  y.value = as.numeric(test.data[obs,col.x])
  y.class = test.data[obs,1]
  
  md.ci.QL = c()
  for (i in 1:nclass) {
    md.ci.QL[i] = -(1/2)*log.det.cov.ci[i] - (1/2)*(y.value - mu.ci.est[[i]])%*%solve.cov.ci[[i]]%*%(y.value - mu.ci.est[[i]]) + log(pwi[i])
  }
  
  
  #Comparing "QL" values
  aux.QL = c(md.ci.QL)
  names(aux.QL) = seq(1,nclass,1)
  aux.comp = names(aux.QL)[which.max(aux.QL)]

  #Comparing predicted class with true class
  out1[obs] = names(aux.QL)[which.max(aux.QL)] #para a matriz de confusão
  
}

verdadeiro = test.data[,1]
estimado = as.numeric(out1)
confusion.matrix = table(verdadeiro, estimado)

ac = sum(diag(confusion.matrix))/sum(confusion.matrix)*100
pr = diag(confusion.matrix)/colSums(confusion.matrix)*100
re = diag(confusion.matrix)/rowSums(confusion.matrix)*100
f1 = 2*(pr*re)/(pr + re)


#Output
mylist = list("ac" = ac, "pr" = pr, "re" = re, "f1" = f1)
return(mylist)

}








#-------------------------------------------------------------------------------------------------
#Step 6: Saving the function
#-------------------------------------------------------------------------------------------------
#save("gaussianClassifier", file="gaussianClassifier.Rdata")







#=================================================================================================
# Evaluating the classifier
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



# Evalueting the classifier based on accuracy
out = 0
for(i in 1:100){
  #out[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "standard")$ac
  #out[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "naive-bayes")$ac
  out[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "tikhonov")$ac
  #out[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "pooled")$ac
  #out[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "friedman")$ac
}

out1 = c(round(min(out), 2), round(median(out), 2), round(max(out), 2), round(mean(out), 2), round(sd(out), 2), round(sd(out)/mean(out), 2))
out1



























#=================================================================================================
# Dimensionality Reduction - Principal Components Analysis (PCA)
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
head(data)
data = data[,-22]
head(data)
col.class = 22
y = data[,col.class]
X = data[,-col.class]
head(X)

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

# Original data
head(data)
y = data[,22]
X = data[,-22]


# Data with PCA
head(pca.data)
y = pca.data[,5]
X = pca.data[,-5]

# Data with LDA
head(lda.data)
y = lda.data[,3]
X = lda.data[,-3]


# Data with Box-Cox
head(boxcox.data)
y = data[,22]
X = boxcox.data


# Evalueting the classifier
out = 0
for(i in 1:100){
  out[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "standard")$ac[1]
  #out[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "naive-bayes")$ac[1]
  #out[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "white-noise")$ac[1]
  #out[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "pooled")$ac[1]
  #out[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "friedman")$ac[1]
  #out[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "tikhonov")$ac[1]
}

out1 = c(round(min(out), 2), round(median(out), 2), round(max(out), 2), round(mean(out), 2), round(sd(out), 2), round(sd(out)/mean(out), 2))
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
  out1[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "standard")$ac[1]
  out2[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "standard")$pr[1]
  out3[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "standard")$pr[2]
  out4[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "standard")$pr[3]
  out5[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "standard")$re[1]
  out6[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "standard")$re[2]
  out7[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "standard")$re[3]
  out8[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "standard")$f1[1]
  out9[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "standard")$f1[2]
  out10[i] = gaussianClassifier(y = y, X = X, train.perc = 0.8, cov.matrix = "standard")$f1[3]
}

out = c(round(mean(out1), 2), round(mean(out2), 2), round(mean(out3), 2), round(mean(out4), 2), round(mean(out5), 2),
         round(mean(out6), 2), round(mean(out7), 2), round(mean(out8), 2), round(mean(out9), 2), round(mean(out10), 2))
out







