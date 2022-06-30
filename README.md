
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BeeGUTS

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/BeeGUTS)](https://CRAN.R-project.org/package=BeeGUTS)
[![R-CMD-check](https://github.com/bgoussen/BeeGUTS/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bgoussen/BeeGUTS/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of BeeGUTS is to analyse the survival toxicity tests performed
for bee species. It can be used to fit a Toxicokinetic-Toxicodynamic
(TKTD) model adapted for bee standard studies (acute oral, acute
contact, and chronic oral studies). The TKTD model used is the General
Unified Threshold model of Survival (GUTS).

## Installation

You can install the released version of BeeGUTS from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("BeeGUTS")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("bgoussen/BeeGUTS")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(BeeGUTS)
#> BeeGUTS (Version 1.0.0, packaged on the: )
#> - For execution on a local, multicore CPU with excess RAM we recommend calling
#>       options(mc.cores = parallel::detectCores()-1)
#> - In addition to the functions provided by 'BeeGUTS', we recommend using the packages:
#>    - 'bayesplot' for posterior analysis, model checking, and MCMC diagnostics.
#>    - 'loo' for leave-one-out cross-validation (LOO) using Pareto smoothed
#>        importance sampling (PSIS), comparison of predictive errors between models, and
#>        widely applicable information criterion (WAIC).
file_location <- system.file("extdata", "betacyfluthrin_chronic_ug.txt", package = "BeeGUTS") # Load the path to one of the example file
lsData <- dataGUTS(file_location = file_location, test_type = 'Chronic_Oral') # Read the example file
plot(lsData) # Plot the data
#> [[1]]
```

<img src="man/figures/README-example-1.png" width="100%" />

``` r
fit <- fitBeeGUTS(lsData, modelType = "SD", nIter = 2000) # Fit a SD model. This can take some time...
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
traceplot(fit) # Produce a diagnostic plot of the fit
```

<img src="man/figures/README-example-2.png" width="100%" />

``` r
plot(fit) # Plot the fit results
#> [[1]]
```

<img src="man/figures/README-example-3.png" width="100%" />

``` r
summary(fit) # Gives a summary of the results
#> Warning in summary.beeSurvFit(fit): Computing summary can take some time. Please
#> be patient...
#> Summary: 
#> 
#> Bayesian Inference performed with Stan.
#>  Model type: SD 
#>  Bee species: Honey_Bee 
#> 
#>  MCMC sampling setup (select with '$setupMCMC')
#>  Iterations: 2000 
#>  Warmup iterations: 1000 
#>  Thinning interval: 1 
#>  Number of chains: 3
#> 
#> Priors of the parameters (quantiles) (select with '$Qpriors'):
#> 
#>  parameters      median        Q2.5       Q97.5
#>          hb 8.32763e-03 1.09309e-04 6.34432e-01
#>          kd 2.62826e-03 1.17073e-06 5.90041e+00
#>          zw 1.30384e-03 1.15441e-06 1.47261e+00
#>          bw 7.36245e-02 6.78843e-05 7.98500e+01
#> 
#> Posteriors of the parameters (quantiles) (select with '$Qposteriors'):
#> 
#>  parameters      median        Q2.5       Q97.5
#>       hb[1] 6.88616e-03 3.33647e-03 1.02423e-02
#>  parameters      median        Q2.5       Q97.5
#>          kd 9.96502e-01 7.21185e-01 2.00235e+00
#>          zw 2.35931e-01 1.43897e-01 2.70688e-01
#>          bw 3.57882e-01 2.56775e-01 4.29128e-01
#> 
#> 
#>  Maximum Rhat computed (na.rm = TRUE): 1.017425 
#>  Minimum Bulk_ESS: 261 
#>  Minimum Tail_ESS: 139 
#>  Bulk_ESS and Tail_ESS are crude measures of effecting sampling size for
#>       bulk and tail quantities respectively. An ESS > 100 per chain can be
#>       considered as a good indicator. Rhat is an indicator of chains convergence.
#>       A Rhat <= 1.05 is a good indicator of convergence. For detail results,
#>       one can call 'rstan::monitor(YOUR_beeSurvFit_OBJECT$stanFit)
#> 
#>  EFSA Criteria (PPC, NRMSE, and SPPE) can be accessed via 'x$EFSA'
validation <- validate(fit, lsData) # produce a validation of the fit (here it uses the same dataset as calibration as an example, so not relevant…)
#> Note that computing can be quite long (several minutes).
#>   Tips: To reduce that time you can reduce Number of MCMC chains (default mcmc_size is set to 1000).
#> Warning in summary.beeSurvFit(object): Computing summary can take some time.
#> Please be patient...
plot(validation) # plot the validation results
```

<img src="man/figures/README-example-4.png" width="100%" />

``` r
dataPredict <- data.frame(time = c(1:5, 1:15), conc = c(rep(5, 5), rep(15, 15)),  replicate = c(rep("rep1", 5), rep("rep3", 15))) # Prepare data for forwards prediction
prediction <- predict(fit, dataPredict) # Perform forwards prediction. At the moment, no concentration recalculation is performed in the forwards prediction. The concentrations are taken as in a chronic test
#> Warning in summary.beeSurvFit(object): Computing summary can take some time.
#> Please be patient...
plot(prediction) # Plot of the prediction results
```

<img src="man/figures/README-example-5.png" width="100%" />
