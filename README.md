
<!-- README.md is generated from README.Rmd. Please edit that file -->

# BeeGUTS

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
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
#> BeeGUTS (Version 1.0.1, packaged on the: )
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
#> Warning: The largest R-hat is NA, indicating chains have not mixed.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#r-hat
#> Warning: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#bulk-ess
#> Warning: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
#> Running the chains for more iterations may help. See
#> https://mc-stan.org/misc/warnings.html#tail-ess
#> Warning: 
#>     *** Markov chains did not converge! Do not analyze results! ***.
#>     Plot MCMC chains and try the following options:
#>     (1) if one or more chain are a simple stable line, increase 'adapt_delta' (default is 0.95).
#>     (2) if the variabbility between chain is great, you can increase the number of iteration (default is 2000 iteration).
#>     (3) if 'Conditional_Psurv_hat' is greater than 1, the ODE integration is wrong. So you can reduce the tolerance of the ODE integrator.
#>                  sigma[1]                  sigma[2]                  sigma[3] 
#>                  1.078931                  1.120471                  1.092462 
#>                  sigma[4]                  kd_log10                  zw_log10 
#>                  1.076083                  1.078931                  1.120471 
#>                  bw_log10               hb_log10[1]                  param[1] 
#>                  1.092462                  1.076083                  1.065235 
#>                  param[2]                  param[3]                  param[4] 
#>                  1.096105                  1.081536                  1.062778 
#>                y_hat[1,1]                y_hat[1,2]                y_hat[2,1] 
#>                       NaN                  1.062778                       NaN 
#>                y_hat[2,2]                y_hat[3,1]                y_hat[3,2] 
#>                  1.062778                       NaN                  1.062778 
#>                y_hat[4,1]                y_hat[4,2]                y_hat[5,1] 
#>                       NaN                  1.062778                       NaN 
#>                y_hat[5,2]                y_hat[6,1]                y_hat[6,2] 
#>                  1.062778                       NaN                  1.062778 
#>                y_hat[7,1]                y_hat[7,2]                y_hat[8,1] 
#>                       NaN                  1.062778                       NaN 
#>                y_hat[8,2]                y_hat[9,1]                y_hat[9,2] 
#>                  1.062778                       NaN                  1.062778 
#>               y_hat[10,1]               y_hat[10,2]               y_hat[11,1] 
#>                       NaN                  1.062778                       NaN 
#>               y_hat[11,2]               y_hat[12,1]               y_hat[12,2] 
#>                  1.062778                  1.065235                  1.062778 
#>               y_hat[13,1]               y_hat[13,2]               y_hat[14,1] 
#>                  1.074586                  1.062778                  1.052825 
#>               y_hat[14,2]               y_hat[15,1]               y_hat[15,2] 
#>                  1.062778                  1.037037                  1.062778 
#>               y_hat[16,1]               y_hat[16,2]               y_hat[17,1] 
#>                  1.026602                  1.062778                  1.019542 
#>               y_hat[17,2]               y_hat[18,1]               y_hat[18,2] 
#>                  1.062778                  1.014592                  1.062778 
#>               y_hat[19,1]               y_hat[19,2]               y_hat[20,1] 
#>                  1.011015                  1.062778                  1.008371 
#>               y_hat[20,2]               y_hat[21,1]               y_hat[21,2] 
#>                  1.062778                  1.006388                  1.062778 
#>               y_hat[22,1]               y_hat[22,2]               y_hat[23,1] 
#>                  1.004884                  1.062778                  1.065235 
#>               y_hat[23,2]               y_hat[24,1]               y_hat[24,2] 
#>                  1.062778                  1.074586                  1.041267 
#>               y_hat[25,1]               y_hat[25,2]               y_hat[26,1] 
#>                  1.052825                  1.009350                  1.037037 
#>               y_hat[26,2]               y_hat[27,1]               y_hat[27,2] 
#>                  1.005081                  1.026602                  1.008525 
#>               y_hat[28,1]               y_hat[28,2]               y_hat[29,1] 
#>                  1.019542                  1.012369                  1.014592 
#>               y_hat[29,2]               y_hat[30,1]               y_hat[30,2] 
#>                  1.015497                  1.011015                  1.017933 
#>               y_hat[31,1]               y_hat[31,2]               y_hat[32,1] 
#>                  1.008371                  1.019841                  1.006388 
#>               y_hat[32,2]               y_hat[33,1]               y_hat[33,2] 
#>                  1.021361                  1.004885                  1.022594 
#>               y_hat[34,1]               y_hat[34,2]               y_hat[35,1] 
#>                  1.065235                  1.062778                  1.074586 
#>               y_hat[35,2]               y_hat[36,1]               y_hat[36,2] 
#>                  1.079230                  1.052825                  1.123469 
#>               y_hat[37,1]               y_hat[37,2]               y_hat[38,1] 
#>                  1.037037                  1.125160                  1.026602 
#>               y_hat[38,2]               y_hat[39,1]               y_hat[39,2] 
#>                  1.120135                  1.019542                  1.113342 
#>               y_hat[40,1]               y_hat[40,2]               y_hat[41,1] 
#>                  1.014592                  1.106541                  1.011015 
#>               y_hat[41,2]               y_hat[42,1]               y_hat[42,2] 
#>                  1.100518                  1.008371                  1.095451 
#>               y_hat[43,1]               y_hat[43,2]               y_hat[44,1] 
#>                  1.006388                  1.091263                  1.004885 
#>               y_hat[44,2]               y_hat[45,1]               y_hat[45,2] 
#>                  1.087806                  1.065235                  1.062778 
#>               y_hat[46,1]               y_hat[46,2]               y_hat[47,1] 
#>                  1.074586                  1.092430                  1.052825 
#>               y_hat[47,2]               y_hat[48,1]               y_hat[48,2] 
#>                  1.051078                  1.037037                  1.011515 
#>               y_hat[49,1]               y_hat[49,2]               y_hat[50,1] 
#>                  1.026602                  1.002047                  1.019542 
#>               y_hat[50,2]               y_hat[51,1]               y_hat[51,2] 
#>                  1.010798                  1.014592                  1.020522 
#>               y_hat[52,1]               y_hat[52,2]               y_hat[53,1] 
#>                  1.011015                  1.027618                  1.008371 
#>               y_hat[53,2]               y_hat[54,1]               y_hat[54,2] 
#>                  1.032523                  1.006388                  1.035978 
#>               y_hat[55,1]               y_hat[55,2]               y_hat[56,1] 
#>                  1.004885                  1.038494                  1.065235 
#>               y_hat[56,2]               y_hat[57,1]               y_hat[57,2] 
#>                  1.062778                  1.074586                  1.063421 
#>               y_hat[58,1]               y_hat[58,2]               y_hat[59,1] 
#>                  1.052825                  1.011569                  1.037037 
#>               y_hat[59,2]               y_hat[60,1]               y_hat[60,2] 
#>                  1.003148                  1.026602                  1.021545 
#>               y_hat[61,1]               y_hat[61,2]               y_hat[62,1] 
#>                  1.019542                  1.037335                  1.014592 
#>               y_hat[62,2]               y_hat[63,1]               y_hat[63,2] 
#>                  1.047046                  1.011015                  1.052920 
#>               y_hat[64,1]               y_hat[64,2]               y_hat[65,1] 
#>                  1.008371                  1.056645                  1.006388 
#>               y_hat[65,2]               y_hat[66,1]               y_hat[66,2] 
#>                  1.059142                  1.004885                  1.060902 
#>              Psurv_hat[1]              Psurv_hat[2]              Psurv_hat[3] 
#>                  1.062784                  1.062878                  1.062978 
#>              Psurv_hat[4]              Psurv_hat[5]              Psurv_hat[6] 
#>                  1.063077                  1.063176                  1.063276 
#>              Psurv_hat[7]              Psurv_hat[8]              Psurv_hat[9] 
#>                  1.063375                  1.063474                  1.063573 
#>             Psurv_hat[10]             Psurv_hat[11]             Psurv_hat[12] 
#>                  1.063672                  1.063771                  1.062784 
#>             Psurv_hat[13]             Psurv_hat[14]             Psurv_hat[15] 
#>                  1.062878                  1.062978                  1.063077 
#>             Psurv_hat[16]             Psurv_hat[17]             Psurv_hat[18] 
#>                  1.063176                  1.063276                  1.063375 
#>             Psurv_hat[19]             Psurv_hat[20]             Psurv_hat[21] 
#>                  1.063474                  1.063573                  1.063672 
#>             Psurv_hat[22]             Psurv_hat[23]             Psurv_hat[24] 
#>                  1.063771                  1.062784                  1.041316 
#>             Psurv_hat[25]             Psurv_hat[26]             Psurv_hat[27] 
#>                  1.009350                  1.005056                  1.008458 
#>             Psurv_hat[28]             Psurv_hat[29]             Psurv_hat[30] 
#>                  1.012245                  1.015309                  1.017678 
#>             Psurv_hat[31]             Psurv_hat[32]             Psurv_hat[33] 
#>                  1.019518                  1.020969                  1.022132 
#>             Psurv_hat[34]             Psurv_hat[35]             Psurv_hat[36] 
#>                  1.062784                  1.079227                  1.123248 
#>             Psurv_hat[37]             Psurv_hat[38]             Psurv_hat[39] 
#>                  1.124343                  1.118661                  1.111236 
#>             Psurv_hat[40]             Psurv_hat[41]             Psurv_hat[42] 
#>                  1.103875                  1.097377                  1.091913 
#>             Psurv_hat[43]             Psurv_hat[44]             Psurv_hat[45] 
#>                  1.087387                  1.083634                  1.062784 
#>             Psurv_hat[46]             Psurv_hat[47]             Psurv_hat[48] 
#>                  1.091849                  1.049721                  1.011346 
#>             Psurv_hat[49]             Psurv_hat[50]             Psurv_hat[51] 
#>                  1.002186                  1.011312                  1.022039 
#>             Psurv_hat[52]             Psurv_hat[53]             Psurv_hat[54] 
#>                  1.030448                  1.036774                  1.041675 
#>             Psurv_hat[55]             Psurv_hat[56]             Psurv_hat[57] 
#>                  1.045624                  1.062784                  1.062065 
#>             Psurv_hat[58]             Psurv_hat[59]             Psurv_hat[60] 
#>                  1.010909                  1.002960                  1.023032 
#>             Psurv_hat[61]             Psurv_hat[62]             Psurv_hat[63] 
#>                  1.042370                  1.055854                  1.065213 
#>             Psurv_hat[64]             Psurv_hat[65]             Psurv_hat[66] 
#>                  1.072029                  1.077194                  1.081178 
#>  Conditional_Psurv_hat[1]  Conditional_Psurv_hat[2]  Conditional_Psurv_hat[3] 
#>                  1.062784                  1.062878                  1.062878 
#>  Conditional_Psurv_hat[4]  Conditional_Psurv_hat[5]  Conditional_Psurv_hat[6] 
#>                  1.062878                  1.062878                  1.062878 
#>  Conditional_Psurv_hat[7]  Conditional_Psurv_hat[8]  Conditional_Psurv_hat[9] 
#>                  1.062878                  1.062878                  1.062878 
#> Conditional_Psurv_hat[10] Conditional_Psurv_hat[11] Conditional_Psurv_hat[12] 
#>                  1.062878                  1.062878                  1.062784 
#> Conditional_Psurv_hat[13] Conditional_Psurv_hat[14] Conditional_Psurv_hat[15] 
#>                  1.062878                  1.062878                  1.062878 
#> Conditional_Psurv_hat[16] Conditional_Psurv_hat[17] Conditional_Psurv_hat[18] 
#>                  1.062878                  1.062878                  1.062878 
#> Conditional_Psurv_hat[19] Conditional_Psurv_hat[20] Conditional_Psurv_hat[21] 
#>                  1.062878                  1.062878                  1.062878 
#> Conditional_Psurv_hat[22] Conditional_Psurv_hat[23] Conditional_Psurv_hat[24] 
#>                  1.062878                  1.062784                  1.041316 
#> Conditional_Psurv_hat[25] Conditional_Psurv_hat[26] Conditional_Psurv_hat[27] 
#>                  1.007528                  1.025890                  1.031795 
#> Conditional_Psurv_hat[28] Conditional_Psurv_hat[29] Conditional_Psurv_hat[30] 
#>                  1.033227                  1.033654                  1.033779 
#> Conditional_Psurv_hat[31] Conditional_Psurv_hat[32] Conditional_Psurv_hat[33] 
#>                  1.033815                  1.033825                  1.033820 
#> Conditional_Psurv_hat[34] Conditional_Psurv_hat[35] Conditional_Psurv_hat[36] 
#>                  1.062784                  1.079227                  1.128453 
#> Conditional_Psurv_hat[37] Conditional_Psurv_hat[38] Conditional_Psurv_hat[39] 
#>                  1.102779                  1.083857                  1.070328 
#> Conditional_Psurv_hat[40] Conditional_Psurv_hat[41] Conditional_Psurv_hat[42] 
#>                  1.063295                  1.060008                  1.058503 
#> Conditional_Psurv_hat[43] Conditional_Psurv_hat[44] Conditional_Psurv_hat[45] 
#>                  1.057815                  1.057499                  1.062784 
#> Conditional_Psurv_hat[46] Conditional_Psurv_hat[47] Conditional_Psurv_hat[48] 
#>                  1.091849                  1.006589                  1.030782 
#> Conditional_Psurv_hat[49] Conditional_Psurv_hat[50] Conditional_Psurv_hat[51] 
#>                  1.050622                  1.054482                  1.055012 
#> Conditional_Psurv_hat[52] Conditional_Psurv_hat[53] Conditional_Psurv_hat[54] 
#>                  1.054924                  1.054778                  1.054674 
#> Conditional_Psurv_hat[55] Conditional_Psurv_hat[56] Conditional_Psurv_hat[57] 
#>                  1.054612                  1.062784                  1.062065 
#> Conditional_Psurv_hat[58] Conditional_Psurv_hat[59] Conditional_Psurv_hat[60] 
#>                  1.005703                  1.057542                  1.072263 
#> Conditional_Psurv_hat[61] Conditional_Psurv_hat[62] Conditional_Psurv_hat[63] 
#>                  1.074668                  1.074756                  1.074514 
#> Conditional_Psurv_hat[64] Conditional_Psurv_hat[65] Conditional_Psurv_hat[66] 
#>                  1.074310                  1.074184                  1.074113 
#>              Nsurv_ppc[1]              Nsurv_ppc[2]              Nsurv_ppc[3] 
#>                       NaN                  1.001872                  1.003964 
#>              Nsurv_ppc[4]              Nsurv_ppc[5]              Nsurv_ppc[6] 
#>                  1.005837                  1.001191                  1.002077 
#>              Nsurv_ppc[7]              Nsurv_ppc[8]              Nsurv_ppc[9] 
#>                  1.000959                  1.002754                  1.004865 
#>             Nsurv_ppc[10]             Nsurv_ppc[11]             Nsurv_ppc[12] 
#>                  1.006861                  1.006653                       NaN 
#>             Nsurv_ppc[13]             Nsurv_ppc[14]             Nsurv_ppc[15] 
#>                  1.002614                  1.004466                  1.005323 
#>             Nsurv_ppc[16]             Nsurv_ppc[17]             Nsurv_ppc[18] 
#>                  1.003302                  1.005912                  1.003969 
#>             Nsurv_ppc[19]             Nsurv_ppc[20]             Nsurv_ppc[21] 
#>                  1.001520                  1.001952                  1.002581 
#>             Nsurv_ppc[22]             Nsurv_ppc[23]             Nsurv_ppc[24] 
#>                  1.004401                       NaN                  1.001020 
#>             Nsurv_ppc[25]             Nsurv_ppc[26]             Nsurv_ppc[27] 
#>                  1.000885                  1.000967                  1.000408 
#>             Nsurv_ppc[28]             Nsurv_ppc[29]             Nsurv_ppc[30] 
#>                  1.002445                  1.004802                  1.002710 
#>             Nsurv_ppc[31]             Nsurv_ppc[32]             Nsurv_ppc[33] 
#>                  1.001439                  1.001241                  1.002291 
#>             Nsurv_ppc[34]             Nsurv_ppc[35]             Nsurv_ppc[36] 
#>                       NaN                  1.012954                  1.047290 
#>             Nsurv_ppc[37]             Nsurv_ppc[38]             Nsurv_ppc[39] 
#>                  1.024746                  1.016344                  1.011839 
#>             Nsurv_ppc[40]             Nsurv_ppc[41]             Nsurv_ppc[42] 
#>                  1.006208                  1.007603                  1.007623 
#>             Nsurv_ppc[43]             Nsurv_ppc[44]             Nsurv_ppc[45] 
#>                  1.006670                  1.008946                       NaN 
#>             Nsurv_ppc[46]             Nsurv_ppc[47]             Nsurv_ppc[48] 
#>                  1.038581                  1.001416                  1.002107 
#>             Nsurv_ppc[49]             Nsurv_ppc[50]             Nsurv_ppc[51] 
#>                  1.003576                  1.003693                  1.002940 
#>             Nsurv_ppc[52]             Nsurv_ppc[53]             Nsurv_ppc[54] 
#>                  1.007897                  1.003950                  1.002459 
#>             Nsurv_ppc[55]             Nsurv_ppc[56]             Nsurv_ppc[57] 
#>                  1.001500                       NaN                  1.026557 
#>             Nsurv_ppc[58]             Nsurv_ppc[59]             Nsurv_ppc[60] 
#>                  1.001331                  1.013003                  1.008662 
#>             Nsurv_ppc[61]             Nsurv_ppc[62]             Nsurv_ppc[63] 
#>                  1.003385                  1.000058                       NaN 
#>             Nsurv_ppc[64]             Nsurv_ppc[65]             Nsurv_ppc[66] 
#>                       NaN                       NaN                       NaN 
#>              Nsurv_sim[1]              Nsurv_sim[2]              Nsurv_sim[3] 
#>                       NaN                  1.000918                  1.004520 
#>              Nsurv_sim[4]              Nsurv_sim[5]              Nsurv_sim[6] 
#>                  1.006353                  1.007771                  1.010788 
#>              Nsurv_sim[7]              Nsurv_sim[8]              Nsurv_sim[9] 
#>                  1.014296                  1.016217                  1.016699 
#>             Nsurv_sim[10]             Nsurv_sim[11]             Nsurv_sim[12] 
#>                  1.017993                  1.017435                       NaN 
#>             Nsurv_sim[13]             Nsurv_sim[14]             Nsurv_sim[15] 
#>                  1.003981                  1.005199                  1.007598 
#>             Nsurv_sim[16]             Nsurv_sim[17]             Nsurv_sim[18] 
#>                  1.009242                  1.010808                  1.011297 
#>             Nsurv_sim[19]             Nsurv_sim[20]             Nsurv_sim[21] 
#>                  1.012994                  1.014755                  1.013925 
#>             Nsurv_sim[22]             Nsurv_sim[23]             Nsurv_sim[24] 
#>                  1.015611                       NaN                  1.000825 
#>             Nsurv_sim[25]             Nsurv_sim[26]             Nsurv_sim[27] 
#>                  1.000246                  1.001257                  1.001237 
#>             Nsurv_sim[28]             Nsurv_sim[29]             Nsurv_sim[30] 
#>                  1.002183                  1.002568                  1.004157 
#>             Nsurv_sim[31]             Nsurv_sim[32]             Nsurv_sim[33] 
#>                  1.004873                  1.005741                  1.006140 
#>             Nsurv_sim[34]             Nsurv_sim[35]             Nsurv_sim[36] 
#>                       NaN                  1.011941                  1.051625 
#>             Nsurv_sim[37]             Nsurv_sim[38]             Nsurv_sim[39] 
#>                  1.068686                  1.064409                  1.064170 
#>             Nsurv_sim[40]             Nsurv_sim[41]             Nsurv_sim[42] 
#>                  1.066747                  1.062590                  1.061496 
#>             Nsurv_sim[43]             Nsurv_sim[44]             Nsurv_sim[45] 
#>                  1.058314                  1.055893                       NaN 
#>             Nsurv_sim[46]             Nsurv_sim[47]             Nsurv_sim[48] 
#>                  1.028298                  1.013875                  1.002116 
#>             Nsurv_sim[49]             Nsurv_sim[50]             Nsurv_sim[51] 
#>                  1.000609                  1.002349                  1.006445 
#>             Nsurv_sim[52]             Nsurv_sim[53]             Nsurv_sim[54] 
#>                  1.012040                  1.014586                  1.015966 
#>             Nsurv_sim[55]             Nsurv_sim[56]             Nsurv_sim[57] 
#>                  1.017121                       NaN                  1.025253 
#>             Nsurv_sim[58]             Nsurv_sim[59]             Nsurv_sim[60] 
#>                  1.002648                  1.001452                  1.012467 
#>             Nsurv_sim[61]             Nsurv_sim[62]             Nsurv_sim[63] 
#>                  1.022581                  1.026979                  1.027857 
#>             Nsurv_sim[64]             Nsurv_sim[65]             Nsurv_sim[66] 
#>                  1.029670                  1.029484                  1.024523 
#>         Nsurv_sim_prec[1]         Nsurv_sim_prec[2]         Nsurv_sim_prec[3] 
#>                       NaN                       NaN                  1.000918 
#>         Nsurv_sim_prec[4]         Nsurv_sim_prec[5]         Nsurv_sim_prec[6] 
#>                  1.004520                  1.006353                  1.007771 
#>         Nsurv_sim_prec[7]         Nsurv_sim_prec[8]         Nsurv_sim_prec[9] 
#>                  1.010788                  1.014296                  1.016217 
#>        Nsurv_sim_prec[10]        Nsurv_sim_prec[11]        Nsurv_sim_prec[12] 
#>                  1.016699                  1.017993                       NaN 
#>        Nsurv_sim_prec[13]        Nsurv_sim_prec[14]        Nsurv_sim_prec[15] 
#>                       NaN                  1.003981                  1.005199 
#>        Nsurv_sim_prec[16]        Nsurv_sim_prec[17]        Nsurv_sim_prec[18] 
#>                  1.007598                  1.009242                  1.010808 
#>        Nsurv_sim_prec[19]        Nsurv_sim_prec[20]        Nsurv_sim_prec[21] 
#>                  1.011297                  1.012994                  1.014755 
#>        Nsurv_sim_prec[22]        Nsurv_sim_prec[23]        Nsurv_sim_prec[24] 
#>                  1.013925                       NaN                       NaN 
#>        Nsurv_sim_prec[25]        Nsurv_sim_prec[26]        Nsurv_sim_prec[27] 
#>                  1.000825                  1.000246                  1.001257 
#>        Nsurv_sim_prec[28]        Nsurv_sim_prec[29]        Nsurv_sim_prec[30] 
#>                  1.001237                  1.002183                  1.002568 
#>        Nsurv_sim_prec[31]        Nsurv_sim_prec[32]        Nsurv_sim_prec[33] 
#>                  1.004157                  1.004873                  1.005741 
#>        Nsurv_sim_prec[34]        Nsurv_sim_prec[35]        Nsurv_sim_prec[36] 
#>                       NaN                       NaN                  1.011941 
#>        Nsurv_sim_prec[37]        Nsurv_sim_prec[38]        Nsurv_sim_prec[39] 
#>                  1.051625                  1.068686                  1.064409 
#>        Nsurv_sim_prec[40]        Nsurv_sim_prec[41]        Nsurv_sim_prec[42] 
#>                  1.064170                  1.066747                  1.062590 
#>        Nsurv_sim_prec[43]        Nsurv_sim_prec[44]        Nsurv_sim_prec[45] 
#>                  1.061496                  1.058314                       NaN 
#>        Nsurv_sim_prec[46]        Nsurv_sim_prec[47]        Nsurv_sim_prec[48] 
#>                       NaN                  1.028298                  1.013875 
#>        Nsurv_sim_prec[49]        Nsurv_sim_prec[50]        Nsurv_sim_prec[51] 
#>                  1.002116                  1.000609                  1.002349 
#>        Nsurv_sim_prec[52]        Nsurv_sim_prec[53]        Nsurv_sim_prec[54] 
#>                  1.006445                  1.012040                  1.014586 
#>        Nsurv_sim_prec[55]        Nsurv_sim_prec[56]        Nsurv_sim_prec[57] 
#>                  1.015966                       NaN                       NaN 
#>        Nsurv_sim_prec[58]        Nsurv_sim_prec[59]        Nsurv_sim_prec[60] 
#>                  1.025253                  1.002648                  1.001452 
#>        Nsurv_sim_prec[61]        Nsurv_sim_prec[62]        Nsurv_sim_prec[63] 
#>                  1.012467                  1.022581                  1.026979 
#>        Nsurv_sim_prec[64]        Nsurv_sim_prec[65]        Nsurv_sim_prec[66] 
#>                  1.027857                  1.029670                  1.029484 
#>                log_lik[1]                log_lik[2]                log_lik[3] 
#>                  1.045152                  1.045152                  1.045152 
#>                log_lik[4]                log_lik[5]                log_lik[6] 
#>                  1.045152                  1.045152                  1.045152 
#>                log_lik[7]                log_lik[8]                log_lik[9] 
#>                  1.045152                  1.045152                  1.045152 
#>               log_lik[10]               log_lik[11]               log_lik[12] 
#>                  1.045152                  1.045152                  1.053689 
#>               log_lik[13]               log_lik[14]               log_lik[15] 
#>                  1.053689                  1.053689                  1.053689 
#>               log_lik[16]               log_lik[17]               log_lik[18] 
#>                  1.053689                  1.053689                  1.053689 
#>               log_lik[19]               log_lik[20]               log_lik[21] 
#>                  1.053689                  1.053689                  1.053689 
#>               log_lik[22]               log_lik[23]               log_lik[24] 
#>                  1.053689                  1.019158                  1.019158 
#>               log_lik[25]               log_lik[26]               log_lik[27] 
#>                  1.019158                  1.019158                  1.019158 
#>               log_lik[28]               log_lik[29]               log_lik[30] 
#>                  1.019158                  1.019158                  1.019158 
#>               log_lik[31]               log_lik[32]               log_lik[33] 
#>                  1.019158                  1.019158                  1.019158 
#>               log_lik[34]               log_lik[35]               log_lik[36] 
#>                  1.009710                  1.009710                  1.009710 
#>               log_lik[37]               log_lik[38]               log_lik[39] 
#>                  1.009710                  1.009710                  1.009710 
#>               log_lik[40]               log_lik[41]               log_lik[42] 
#>                  1.009710                  1.009710                  1.009710 
#>               log_lik[43]               log_lik[44]               log_lik[45] 
#>                  1.009710                  1.009710                  1.008365 
#>               log_lik[46]               log_lik[47]               log_lik[48] 
#>                  1.008365                  1.008365                  1.008365 
#>               log_lik[49]               log_lik[50]               log_lik[51] 
#>                  1.008365                  1.008365                  1.008365 
#>               log_lik[52]               log_lik[53]               log_lik[54] 
#>                  1.008365                  1.008365                  1.008365 
#>               log_lik[55]               log_lik[56]               log_lik[57] 
#>                  1.008365                  1.078049                  1.078049 
#>               log_lik[58]               log_lik[59]               log_lik[60] 
#>                  1.078049                  1.078049                  1.078049 
#>               log_lik[61]               log_lik[62]               log_lik[63] 
#>                  1.078049                  1.078049                  1.078049 
#>               log_lik[64]               log_lik[65]               log_lik[66] 
#>                  1.078049                  1.078049                  1.078049 
#>                      lp__ 
#>                  1.058420
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
#> Warning in summary.beeSurvFit(fit): Computing summary can take some time.
#> Please be patient...
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
#>       hb[1] 6.85208e-03 2.32906e-03 1.03117e-02
#>  parameters      median        Q2.5       Q97.5
#>          kd 1.01644e+00 7.30765e-01 2.90986e+00
#>          zw 2.36320e-01 1.34592e-01 2.74867e-01
#>          bw 3.54844e-01 2.42880e-01 4.37801e-01
#> 
#> 
#>  Maximum Rhat computed (na.rm = TRUE): 1.128453 
#>  Minimum Bulk_ESS: 48 
#>  Minimum Tail_ESS: 55 
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
