# Subject: Multivariate Statistic
# Lecture: Introduction
# Author: Vinicius Osterne (www.osterne.com)




# Distribuição Normal

library(faux)
X = rnorm_multi(100, 3, c(0, .5, 1),
                r = c(0.2, -0.5, 0.5), 
                varnames=c("X1", "X2", "X3"))
cor(X)



# Student-t
library(faux)
library(LaplacesDemon)
x <- seq(-2,4,length=21)
y <- 2*x+10
z <- x+cos(y) 
mu <- c(1,12,2)
S <- matrix(c(1,2,0,
              2,5,0.5,
              0,0.5,3), 3, 3)
df <- 4
X <- rmvt(1000, c(0,1,2), S, 5)
head(X)
cor(X)

