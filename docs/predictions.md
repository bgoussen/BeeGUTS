Model predictions
================
2023-10-13

Back to [home](./home.md)

## Model predictions

It is possible also to predict the survival probability for an arbitrary
exposure profile. In the example below, we can create an exposure
profile with a time variable concentration and predict the effect on the
survival probability.

The prediction is returned with the 95% confidence intervals.

``` r
dataPredict <- data.frame(time = c(1:15),
                          conc = c(rep(15,5), rep(0,5), rep(15,5)),
                          replicate = c(rep("pred1", 15)))


pred_res <- predict(fitSD, dataPredict)

plot(pred_res)
```

<img src="figures/DOCS-showpred-1.png" width="75%" />
