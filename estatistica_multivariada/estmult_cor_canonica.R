# Subject: Multivariate Statistic
# Lecture: Canonical Correlation Analysis
# Author: Vinicius Osterne (www.osterne.com)




# ------------------------------------------------------------------------------
# Application 1
# ------------------------------------------------------------------------------
library(ggplot2)
library(GGally)
library(CCA)
library(psych)

#Descrição dos dados
# 600 observações sobre oito variáveis
# As variáveis psicológicas são locus_of_control, autoconceito e motivação.
# As variáveis acadêmicas são testes padronizados de leitura (leitura), redação (escrita), matemática (matemática) e ciências (ciências). 
# Além disso, a variável feminino é uma variável indicadora zero-um, sendo que aquela indica uma estudante do sexo feminino.


# dowloading data
mm = read.csv("https://stats.idre.ucla.edu/stat/data/mmreg.csv")

# changing col names
colnames(mm) <- c("Control", "Concept", "Motivation", "Read", "Write", "Math", "Science", "Sex")
head(mm)

# exploratory
summary(mm)

# counting "sex"
xtabs(~Sex, data = mm)

# spliting the data
psych = mm[, 1:3]
acad = mm[, 4:8]

# correlation plot
ggpairs(psych)
ggpairs(acad)

# cross correlation
matcor(psych, acad)

# canonical correlation method
cc1 = cc(psych, acad)
cc1$cor
cc1[3:4]

# compute canonical loadings
cc2 = comput(psych, acad, cc1)
cc2

# display canonical loadings
cc2[3:6]






# ------------------------------------------------------------------------------
# Application 2
# ------------------------------------------------------------------------------

#https://medium.com/@josef.waples/canonical-correlation-analysis-cca-in-r-rstudio-using-mtcars-1669b1c56731

library(tidyverse)
data(mtcars)
head(mtcars)

# correlation matrix
corr_matrix = cor(mtcars)

# spliting
car_char = mtcars %>%
  dplyr::select(-mpg, -qsec)

car_perf <- mtcars %>%
  dplyr::select(mpg, qsec)

# canonical correlation method
cc <- cancor(car_char, car_perf)
cc

