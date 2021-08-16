### Sandbox script!
### testing script to test the implementation of the solutions on the various
### functions
packages <- c("BeeGUTS", "odeGUTS", "dplyr","methods", "Rcpp",
              "RcppParallel", "rstan", "rstantools", "data.table",
              "tidyr", "ggplot2", "cowplot", "magrittr",
              "utils", "gridExtra", "plotly")
sapply(packages, function(X) do.call("require", list(X)))

f1 <- "C:/Users/roc/Documents/R/win-library/4.0/BeeGUTS/extdata/betacyfluthrin_chronic_ug.txt"
f2 <- "C:/Users/roc/Documents/BeeGUTS/BeeGUTS/data-raw/metribuzin_chronic_ug.txt"
f3 <- "C:/Users/roc/Documents/BeeGUTS/BeeGUTS/data-raw/fenoxaprop_chronic_ug.txt"

t1 <- "Chronic_Oral"
t2 <- "Chronic_Oral"
t3 <- "Chronic_Oral"
## Warning, if we change the test_type to a non constant concentration,
## the fitting takes ages beacuse there is a large number of concentration intervals

file_location <- c(f1,f2)
test_type <- c(t1,t2)

output <- dataGUTS(file_location = file_location, test_type = test_type, bee_species = "Honey_Bee")
plot(output)

#very small value to speedup the fit for testing purposes
fit<-fitBeeGUTS(output, modelType = "SD", nIter = 200)

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
