### Sandbox/tutorial script!
### Script to test the implementation of the solutions on the various
### functions
packages <- c("BeeGUTS", "odeGUTS", "dplyr","methods", "Rcpp",
              "RcppParallel", "rstan", "rstantools", "data.table",
              "tidyr", "ggplot2", "cowplot", "magrittr",
              "utils", "gridExtra", "plotly")
sapply(packages, function(X) do.call("require", list(X)))

# Testing files
f1 <- "./data-raw/betacyfluthrin_chronic_ug.txt"
f2 <- "./data-raw/metribuzin_chronic_ug.txt"
f3 <- "./data-raw/fenoxaprop_chronic_ug.txt"

t1 <- "Chronic_Oral"
t2 <- "Chronic_Oral"
t3 <- "Chronic_Oral"
## WARNING, if we change the test_type to a non constant concentration,
## the fitting will take long time
## because there is a large number of concentration intervals to be accounted

# FOR TESTING ONLY, mixing files with different chemicals and different units
file_location <- f1
test_type <- t1

calibdata <- dataGUTS(file_location = file_location, test_type = test_type, bee_species = "Honey_Bee")
plot(calibdata)

#very small value of iterations to speedup the fit for testing purposes
fit<-fitBeeGUTS(calibdata, modelType = "SD", nIter = 1000)
plot(fit)

valdata <- dataGUTS(f3, "Chronic_Oral")
valid<-validate(fit, valdata)
plot(valid)

# Prepare data for forwards prediction
dataPredict <- data.frame(time = c(1:5, 1:15),
                          conc = c(rep(5, 5),
                                   rep(15, 15)),
                          replicate = c(rep("rep1", 5),
                                        rep("rep3", 15)))
# Perform forwards prediction.
# At the moment, no concentration recalculation is performed in the forwards
# prediction. The concentrations are taken as in a chronic test
prediction <- predict(fit, dataPredict)
plot(prediction)
