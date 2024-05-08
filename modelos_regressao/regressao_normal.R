# Subject: Regression Models
# Lecture: Normal model
# Author: Vinicius Osterne (www.osterne.com)



# ------------------------------------------------------------------------------
# Application 1
# ------------------------------------------------------------------------------

# download the data
x = c(6.1, 3.7, 6.5, 5.8, 5.1, 4.6, 9.1, 6.5, 7.5, 5.5)
y = c(23.7, 17.8, 22.4, 18.9, 17.4, 19.2, 29.2, 18.2, 24.7, 21.3)
cbind(x,y)  

# ajustando modelo 1
ajuste=lm(x~y)
ajuste


# ajustando modelo 2
m = lm(y~x)
s = summary(lm(y~x))
s

# anova
anova(m)
