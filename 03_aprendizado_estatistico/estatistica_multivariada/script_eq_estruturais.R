# Subject: Multivariate Statistic
# Lecture: Structural Equation Modeling
# Author: Vinicius Osterne (www.osterne.com)



# ------------------------------------------------------------------------------
# Application 1
# ------------------------------------------------------------------------------

# necessary packages
library(lavaan)

# data
dat <- read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2021/02/worland5.csv")
head(dat)

#
m1 <-   '
# regressions
read ~ 1 + motiv
# variance (optional)
motiv ~~ motiv
'

# sem
fit1 <- sem(m1, data=dat)
summary(fit1, fit.measures=TRUE)






# ------------------------------------------------------------------------------
# Application 2
# ------------------------------------------------------------------------------

library(lavaan)

#the observed variables x1 and x2to the latent variable y
model <- "y =~ x1 + x2" 


#x1 and x2 are observed variables influenced by y
#y is also predicted by two outcome variables x3 and x4
#x3 and x5 are covariates
model <- "y =~ x1 + x2 #
          y ~ x3 + x4      
          x3 ~~ x5"


fit <- sem(model, data = mydata)
summary(fit, standardized = TRUE)

#https://medium.com/geekculture/how-to-perform-structural-equation-modeling-in-r-with-the-lavaan-package-d224a65668b5

#Model Specification
model <- '
  # measurement model
    ind60 =~ x1 + x2 + x3
    dem60 =~ y1 + y2 + y3 + y4
    dem65 =~ y5 + y6 + y7 + y8
  # regressions
    dem60 ~ ind60
    dem65 ~ ind60 + dem60
  # residual correlations
    y1 ~~ y5
    y2 ~~ y4 + y6
    y3 ~~ y7
    y4 ~~ y8
    y6 ~~ y8
'




