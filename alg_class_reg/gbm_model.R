#============================================================================================
# Script - GBM model
# By Vinícius Osterne (www.osterne.com | vinicius@osterne.com)
#============================================================================================




library(h2o)
h2o.init()

# import the airlines dataset:
airlines <- h2o.importFile("https://s3.amazonaws.com/h2o-public-test-data/smalldata/testng/airlines_train.csv")
airlines


# build and train the model:
model <- h2o.gbm(x = c("Origin", "Distance"),
                 y = "IsDepDelayed",
                 training_frame = airlines,
                 ntrees = 1,
                 gainslift_bins = 5)

# print the Gains/Lift table for the model:
print(h2o.gainsLift(model))
kolmogorov_smirnov <- h2o.kolmogorov_smirnov(model)
kolmogorov_smirnov



model <- h2o.gbm(x = c("Origin", "Distance"),
                 y = "IsDepDelayed",
                 training_frame = train,
                 ntrees = 1,
                 gainslift_bins = 5)

print(h2o.gainsLift(model))



library("writexl")
write_xlsx(h2o.gainsLift(model),"file_name.xlsx")

# retrieve the ks metric:
kolmogorov_smirnov <- h2o.kolmogorov_smirnov(model)
kolmogorov_smirnov







