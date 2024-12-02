# Subject: Multivariate Statistic
# Lecture: Exploratory analysis
# Author: Vinicius Osterne (www.osterne.com)



# Organização dos dados
x1 = c(23, 67, 30, 40, 38)
x2 = c(1, 3, 4, 4, 3)
X = matrix(cbind(x1, x2), nrow = 5, ncol = 2)
X


# Vetor de médias
means = colMeans(X)
means


# Matriz de covariância
covs = cov(X)
covs


#Matriz de correlação
cors = cor(X)
cors







#Distância Euclidiana
x = c(2, 2, 6, 9, 3, 5, 6, 1)
y = c(16, 21, 12, 10, 10, 6, 8, 14)
d_e = sqrt(sum((x-y)^2))
d_e



# Distância de Manhattan
x = c(2, 2, 6, 9, 3, 5, 6, 1)
y = c(16, 21, 12, 10, 10, 6, 8, 14)
d_mh = sqrt(sum(abs(x-y)))
d_mh


# Distância de Mahalanobis
x1 = c(91, 93, 72, 87, 86, 73, 68, 87, 78, 99, 95, 76, 84, 96, 
       76, 80, 83, 84, 73, 74)
x2 = c(16, 6, 3, 1, 2, 3, 2, 5, 2, 5, 2, 3, 4, 3, 3, 3, 4, 3, 4, 4)
x3 = c(3, 4, 0, 3, 4, 0, 1, 2, 1, 2, 3, 3, 3, 2, 2, 2, 3, 3, 2, 2)
x4 = c(70, 88, 80, 83, 88, 84, 78, 94, 90, 93, 89, 82, 95, 94, 
       81, 93, 93, 90, 89, 89)
df_xy = data.frame(cbind(x1, x2, x3, x4))
mu_xy = colMeans(df_xy)
cov_xy = cov(df_xy)
cov_xy = cor(df_xy)

d_mb = c()
for (i in 1:length(x1)) {
  d_mb[i] = (t(t(df_xy[i,] - mu_xy)) %*% solve(cov_xy) %*% 
               t(df_xy[i,] - mu_xy))
}
d_mb

d_mb_R = mahalanobis(df_xy, mu_xy, cov_xy)
d_mb_R













