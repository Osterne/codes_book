#=================================================================================================
#Code: Bayes classifier based on gaussian distribution
#Author: Vin?cius Osterne (vinicius@osterne -- www.osterne.com)
#=================================================================================================







#=================================================================================================
#Clearing the memory
#=================================================================================================

rm(list=ls())


# Time series
births <- scan("http://robjhyndman.com/tsdldata/data/nybirths.dat")
birthstimeseries <- ts(births, frequency=12, start=c(2000,1))
plot.ts(birthstimeseries, xlab = "Time (in years)", ylab = "Rainfall (in millimeters)")

# Longtudinal data
tolerance <- read.csv("https://stats.idre.ucla.edu/stat/r/examples/alda/data/tolerance1_pp.txt")
tolerance <- within(tolerance, {
  id <- factor(id)
  male <- factor(male, levels = 0:1, labels = c("female", "male"))
})

head(tolerance)
plot(tolerance)



