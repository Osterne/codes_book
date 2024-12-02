# Subject: Regression Models
# Lecture: Poisson model
# Author: Vinicius Osterne (www.osterne.com)



# ------------------------------------------------------------------------------
# Application 1
# ------------------------------------------------------------------------------

# download the data
library(readr)
caranguejo <- read_table2("https://goo.gl/Wvvnrf", col_names = FALSE)


# some changes
colnames(caranguejo)=c("Obs","C","S","W","Wt","Sa")
attach(caranguejo)
head(caranguejo)


# descriptive analysis
summary(caranguejo)
hist(caranguejo$Sa)
plot(caranguejo$W,caranguejo$Sa)


# ajustando modelo
regpoisson=glm(Sa~W, family="poisson", data=caranguejo)
summary(regpoisson)


# teste de dispersão
require(AER)
dispersiontest(regpoisson)

# anova
anova(regpoisson, test="Chisq")

# predição
print=data.frame(caranguejo, pred=(regpoisson$fitted.values))
head(print)
plot(caranguejo$W,caranguejo$Sa)
points(regpoisson$fitted.values,col='red', type = "l")



