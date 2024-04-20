#=====================================================================
#Code: Clustering via K-means algorithm
#Author: Vinicius Osterne (vinicius@osterne -- www.osterne.com)
#=====================================================================



#---------------------------------------------------------------------
# Clearing memory
#---------------------------------------------------------------------
rm(list=ls())






#---------------------------------------------------------------------
# Package necessary
#---------------------------------------------------------------------
library(philentropy)













#---------------------------------------------------------------------
# Clustering via K-means algorithm - Function construction
#---------------------------------------------------------------------

KmeansCluster = function(k, data){
  

  
# Step 1: Initial values for prototypes (random selection)
ind =  seq(1, dim(data)[1], 1)
select.ind = sample(ind, size = k, replace = F)

prot = list()
for (i in 1:k) {
  prot[[i]] = data[select.ind[i],]
}



dif = 1
while (dif > 0.01) {
  
# Step 2: Calculate distance between obs i and prototype k
N = dim(data)[1]
euclidian.distance = c()
m = list()

for (j in 1:k) {
  for (i in 1:N) {
    euclidian.distance[i] = distance(rbind(prot[[j]], data[i,]), method = "euclidean")
  }
  m[[j]] = euclidian.distance
}



# Step 3: Classify based on the shortest distance between obs i and prototype k
m.dist = Reduce(cbind, m)
m.dist = data.frame(m.dist)
names(m.dist) = seq(1,k,1)

out = c()
for (i in 1:N) {
  out[i] = names(m.dist)[which.min(m.dist[i,])]
}

new.data = data.frame(cbind(data,out))
new.data = lapply(new.data,as.numeric)
new.data = data.frame(new.data)



# Step 4: Update the prototype with the mean
prot.actual = list()
aux.k = dim(data)[2] + 1
for (i in 1:k) {
  prot.actual[[i]] = apply(subset(new.data, out == i)[-aux.k], 2, mean)
}

#prot.actual = Reduce(rbind, aux)
#prot.actual = data.frame(prot.actual)[-aux.k]





# Conditional for while
normk = c()
for (i in 1:k) {
  normk[i] = norm(as.matrix(prot[[i]] - prot.actual[[i]]))
}
dif = sum(normk)
prot = prot.actual




}#end while





# Calculating SSD measure
ssd.data = data.frame(cbind(m.dist,out))
SSDk = c()
for (i in 1:k) {
  SSDk[i] = sum(subset(ssd.data, out == i)[,i])
}
SSD = sum(SSDk)


# Calculating Dunn index:
library(clValid)
forIndex = as.numeric(out)
dunnIndex = dunn(clusters = forIndex, Data=data)



# Calculating Davies-Bouldin index:
library(clusterSim)
dbIndex = index.DB(data, forIndex, centrotypes="centroids")$DB



# Calculating Calinski-Harabasz index:
library(fpc)
chIndex = calinhara(data, forIndex)




# Outpusts for function
mylist = list("ssd" = SSD, "prot.values" = prot, "dunnIndex" = dunnIndex,  "dbIndex" = dbIndex, "chIndex" = chIndex)
return(mylist)

}















#---------------------------------------------------------------------
# Application in real data
#---------------------------------------------------------------------

# Real data
#rm(list = ls())
data = read.table(choose.files(), header = F, sep = "", dec = ".")
head(data)


# Normalized data
library(caret)
normData = preProcess(data, method=c("range"))
normData =  predict(normData, data)
data = normData
head(data)




#Evaluating the cluster by index
k = 6
out = KmeansCluster(k, data)
outIndex  = c(out$dunnIndex, out$dbIndex, out$chIndex)
outIndex = round(outIndex, 4)
outIndex 






#Evaluating the cluster by coordenates of prototypes
prot.values1 = list()
prot.values2 = list()

for (i in 1:10) {
  
  out = KmeansCluster(k = 2, data)
  prot.values1[[i]] = out$prot.values[[1]]
  prot.values2[[i]] = out$prot.values[[2]]
}

prot.values11 = Reduce(rbind, prot.values1)
prot.values22 = Reduce(rbind, prot.values2)


round(apply(prot.values22, 2, sd), 4)





#---------------------------------------------------------------------
# Using the function in R
#---------------------------------------------------------------------


k=5
kmeans.app = kmeans(data, k)

dunn(clusters = kmeans.app$cluster, Data=data)
index.DB(data, kmeans.app$cluster, centrotypes="centroids")$DB
calinhara(data, kmeans.app$cluster)







