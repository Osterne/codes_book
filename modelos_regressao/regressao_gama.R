# Subject: Regression Models
# Lecture: Gamma model
# Author: Vinicius Osterne (www.osterne.com)



# ------------------------------------------------------------------------------
# Application 1
# ------------------------------------------------------------------------------

# generating the data
n<-50
x<-runif(n,0,1)
eta<-2 + 3*x 
mu<-exp(eta)
var<- 2
alpha <- mu^2/var
sigma <- var/mu
y1 <- rgamma(n, shape = alpha, scale = sigma) 
df1<-data.frame(y1,x)


# ajuste do modelo
mod1<-glm(y1~x,data = df1,Gamma(link = "log"))
summary(mod1)




