#============================================================================================
# Script - Variable selection methods
# By Vinícius Osterne (www.osterne.com | vinicius@osterne.com)
#============================================================================================






#--------------------------------------------------------------------------------------------
# Forward, Backward, and Stepwise methods
#--------------------------------------------------------------------------------------------

# Cleaning the memory
rm(list=ls())


#---------------------------------------------------------------------
# Previous commands
#---------------------------------------------------------------------

# Necessary packages
library(MASS)
library(faraway) #for data set

# Data set
data = swiss
head(data)

# Fitting with all variable
model = lm(Fertility ~., data = swiss)
summary(model)




#---------------------------------------------------------------------
# Method: forward
#---------------------------------------------------------------------

# Step 1
summary(lm(Fertility ~ Agriculture, data = swiss))
summary(lm(Fertility ~ Examination, data = swiss))
summary(lm(Fertility ~ Education, data = swiss))
summary(lm(Fertility ~ Catholic, data = swiss))
summary(lm(Fertility ~ Infant.Mortality, data = swiss))

# Step 2: add Education
summary(lm(Fertility ~ Education + Agriculture, data = swiss))
summary(lm(Fertility ~ Education + Examination, data = swiss))
summary(lm(Fertility ~ Education + Catholic, data = swiss))
summary(lm(Fertility ~ Education + Infant.Mortality, data = swiss))

# Step 3: add Education + Catholic
summary(lm(Fertility ~ Education + Catholic + Agriculture, data = swiss))
summary(lm(Fertility ~ Education + Catholic + Examination, data = swiss))
summary(lm(Fertility ~ Education + Catholic + Infant.Mortality, data = swiss))

# Step 4: add Education + Catholic + Infant.Mortality
summary(lm(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture, data = swiss))
summary(lm(Fertility ~ Education + Catholic + Infant.Mortality + Examination, data = swiss))

# Step 5: add Education + Catholic + Infant.Mortality + Agriculture
summary(lm(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture + Examination, data = swiss))

# Final model:
summary(lm(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture + Examination, data = swiss))






#--------------------------------------------------------------------------------------------
# Method: Backward
#--------------------------------------------------------------------------------------------

#Homework :)


# Step 1:
summary(lm(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture + Examination, data = swiss))

# Step 2: models without Examination
summary(lm(Fertility ~ Catholic + Infant.Mortality + Agriculture, data = swiss))
summary(lm(Fertility ~ Education + Infant.Mortality + Agriculture, data = swiss))
summary(lm(Fertility ~ Education + Catholic + Agriculture, data = swiss))
summary(lm(Fertility ~ Education + Catholic + Infant.Mortality, data = swiss))
summary(lm(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture, data = swiss))

# Final model:
summary(lm(Fertility ~ Education + Catholic + Infant.Mortality + Agriculture + Examination, data = swiss))








#--------------------------------------------------------------------------------------------
# Method: Stepwise
#--------------------------------------------------------------------------------------------

# Homework :)












#--------------------------------------------------------------------------------------------
# Forward, Backward, and Stepwise methods
#--------------------------------------------------------------------------------------------

# Cleaning the memory
rm(list=ls())

# Necessary packages
library(MASS)


# Dataset
data = swiss
head(data)


# Fitting with all variable
model = lm(Fertility ~., data = swiss)


# Method: forward
forward.model = stepAIC(model, direction = "forward", trace = FALSE) #ver o que é o "trace"
summary(forward.model)


# Method: backward
backward.model = stepAIC(model, direction = "backward", trace = FALSE) #ver o que é o "trace"
summary(backward.model)


# Method: stepwise
stepwise.model = stepAIC(model, direction = "both", trace = FALSE) #ver o que é o "trace"
summary(stepwise.model)






























#--------------------------------------------------------------------------------------------
# AIC and BIC criterian
#--------------------------------------------------------------------------------------------

# Cleaning the memory
rm(list=ls())


# Necessary packages
library(modelr)


# Dataset
data = swiss
head(data)


# Models
model.1 = lm(Fertility ~., data = swiss)
summary(model.1)
model.2 = lm(Fertility ~. -Examination, data = swiss)
summary(model.2)

# AIC metric by command
AIC(model.1)
AIC(model.2)

# BIC metric by command
BIC(model.1)
BIC(model.2)










#--------------------------------------------------------------------------------------------
# Homework
#--------------------------------------------------------------------------------------------

# 1. Selecionar as variáveis (à mão) pelo método Stepwise

# 2. Selecionar as variáveis (à mão) pelo método Forward, Backward, and Stepwise usando as medidas AIC e BIC



