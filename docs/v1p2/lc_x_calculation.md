LCx calculations
================
2023-10-13

Back to [home](./home.md)

## LCx calculations

Once the model is [calibrated](./calibration.md) and
[validated](./validation.md), it is possible to compute all the standard
quantities that are required in Risk Assessment.

Being BeeGUTS a GUTS model, it is possible to compute an LCx value for
any survival fraction and for any time duration (this is one of the
great advantages of TKTD models).

The computation of an LCx value is done with the function `LCx`. This
will return an object with the computed LCx value, including the
confidence intervals.

``` r
lc50 <- LCx(object = fitSD,                # the model object
            X = 50,                        # survival %
            testType = "Chronic_Oral",     # test type (Chronic Oral is default)
            timeLCx = 10)                  # duration of the test (in days)

summary(lc50)
#> Summary: 
#> 
#> LC 50  calculation. 
#>  Time for which the LCx is calculated: 10 
#>  Bee species: 
#>  Test type: Chronic_Oral 
#>  LCx: 
#>         quantile      LCx
#> 1         median 18.84009
#> 2  quantile 2.5% 17.39067
#> 3 quantile 97.5% 20.76588
```
By default, the function calculates the LCx value scanning the concentration in
a range between 0 and the maximum concentration of the experiment, dividing the
interval in 100 steps.

This behaviour can be modified using the optional arguments `concRange` 
and `nPoints` e.g.

``` r
lc50 <- LCx(object = fitSD,                # the model object
            X = 50,                        # survival %
            testType = "Chronic_Oral",     # test type (Chronic Oral is default)
            timeLCx = 10,                  # duration of the test (in days)
            concRange=c(0,100),            # new range for LCx function
            nPoints=200)                   # number of bins for concRange
```            

[Back to top](#lcx-calculations)

Back to [home](./home.md)