
# BeeGUTS - General Unified Threshold model of Survival for Bees using Bayesian inference

## Description
### Build status for development version

<!-- badges: start -->
[![R build status](https://github.com/bgoussen/BeeGUTS/workflows/R-CMD-check/badge.svg)](https://github.com/bgoussen/BeeGUTS/actions)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

### Issue tracking

Please report any issues using the [issue tracker](https://github.com/bgoussen/BeeGUTS/issues)

### Aim of the package
The goal of BeeGUTS is to analyse the survival toxicity tests performed for
bee species. It can be used to fit a Toxicokinetic-Toxicodynamic (TKTD) model
adapted for bee standard studies (acute oral, acute contact, and chronic oral studies).
The TKTD model used is the General Unified Threshold model of Survival (GUTS).

## Installation

You can install the released version of BeeGUTS from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("BeeGUTS")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(BeeGUTS)
file_location <- system.file("extdata", "betacyfluthrin_chronic_ug.txt", package = "BeeGUTS") # Load the path to one of the example file
lsData <- dataGUTS(file_location = file_location, test_type = 'Chronic_Oral') # Read the example file
plot(lsData) # Plot the data
fit <- fitBeeGUTS(lsData, modelType = "SD", nIter = 2000) # Fit a SD model. This can take some time...
traceplot(fit) # Produce a diagnostic plot of the fit
plot(fit) # Plot the fit results
summary(fit) # Gives a summary of the results
validation <- validate(fit, lsData) # produce a validation of the fit (here it uses the same dataset as calibration as an example, so not relevant…)
plot(validation) # plot the validation results
dataPredict <- data.frame(time = c(1:5, 1:15), conc = c(rep(5, 5), rep(15, 15)),  replicate = c(rep("rep1", 5), rep("rep3", 15))) # Prepare data for forwards prediction
prediction <- predict(fit, dataPredict) # Perform forwards prediction. At the moment, no concentration recalculation is performed in the forwards prediction. The concentrations are taken as in a chronic test
plot(prediction) # Plot of the prediction results
```

