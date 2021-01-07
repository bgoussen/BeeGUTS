#' Summary of \code{beeSurvFit} objects
#'
#' @description This is the generic \code{summary} S3 method for the \code{beeSurvFit} class.
#' It shows the quantiles of priors and posteriors on parameters.
#'
#' @param object An object of class \code{beeSurvFit}
#' @param ... Additional arguments to be parsed to the generic \code{summary} method (not used)
#'
#' @return
#' @export
#'
#' @examples
#' data(fitBetacyfluthrin_Chronic)
#' summary(fitBetacyfluthrin_Chronic)
summary.beeSurvFit <- function(object, ...) {

  # Prepare prior
  lsData_fit <- object$dataFit

  ## Common parameters
  hb <- 10^qnorm(p = c(0.5, 0.025, 0.975),
                 mean = lsData_fit$hbMean_log10,
                 sd = lsData_fit$hbSD_log10)

  kd <- 10^qnorm(p = c(0.5, 0.025, 0.975),
                 mean = lsData_fit$kdMean_log10,
                 sd = lsData_fit$kdSD_log10)

  ## Model specific parameters
 if (object$modelType == "SD") {

   zw <- 10^qnorm(p = c(0.5, 0.025, 0.975),
                  mean = lsData_fit$zwMean_log10,
                  sd = lsData_fit$zwSD_log10)

   kk <- 10^qnorm(p = c(0.5, 0.025, 0.975),
                  mean = lsData_fit$kkMean_log10,
                  sd = lsData_fit$kkSD_log10)

   outPrior <- data.frame(parameters = c("hb", "kd", "zw", "kk"),
                     median = c(hb[1], kd[1], zw[1], kk[1]),
                     Q2.5 = c(hb[2], kd[2], zw[2], kk[2]),
                     Q97.5 = c(hb[3], kd[3], zw[3], kk[3]))

 } else if (object$modelType == "IT") {

   mw <- 10^qnorm(p = c(0.5, 0.025, 0.975),
                  mean = lsData_fit$mwMean_log10,
                  sd = lsData_fit$mwSD_log10)

   beta <- 10^qnorm(p = c(0.5, 0.025, 0.975),
                  mean = lsData_fit$betaMean_log10,
                  sd = lsData_fit$betaSD_log10)

   outPrior <- data.frame(parameters = c("hb", "kd", "mw", "beta"),
                          median = c(hb[1], kd[1], mw[1], beta[1]),
                          Q2.5 = c(hb[2], kd[2], mw[2], beta[2]),
                          Q97.5 = c(hb[3], kd[3], mw[3], beta[3]))
 }

  # Prepare posteriors

  tmpRes <- rstan::monitor(object$stanFit, print = FALSE)

  ## Common parameters
  hb_med <- 10^tmpRes[["hb_log10", "50%"]]
  hb_inf95 <- 10^tmpRes[["hb_log10", "2.5%"]]
  hb_sup95 <- 10^tmpRes[["hb_log10", "97.5%"]]

  kd_med <- 10^tmpRes[["kd_log10", "50%"]]
  kd_inf95 <- 10^tmpRes[["kd_log10", "2.5%"]]
  kd_sup95 <- 10^tmpRes[["kd_log10", "97.5%"]]

  ## Model specific parameters
  if (object$modelType == "SD") {

    zw_med <- 10^tmpRes[["zw_log10", "50%"]]
    zw_inf95 <- 10^tmpRes[["zw_log10", "2.5%"]]
    zw_sup95 <- 10^tmpRes[["zw_log10", "97.5%"]]

    kk_med <- 10^tmpRes[["kk_log10", "50%"]]
    kk_inf95 <- 10^tmpRes[["kk_log10", "2.5%"]]
    kk_sup95 <- 10^tmpRes[["kk_log10", "97.5%"]]

    outPost <- data.frame(parameters = c("hb", "kd", "zw", "kk"),
                      median = c(hb_med, kd_med, zw_med, kk_med),
                      Q2.5 = c(hb_inf95, kd_inf95, zw_inf95, kk_inf95),
                      Q97.5 = c(hb_sup95, kd_sup95, zw_sup95, kk_sup95))


  } else if (object$modelType == "IT") {

    mw_med <- 10^tmpRes[["mw_log10", "50%"]]
    mw_inf95 <- 10^tmpRes[["mw_log10", "2.5%"]]
    mw_sup95 <- 10^tmpRes[["mw_log10", "97.5%"]]

    beta_med <- 10^tmpRes[["beta_log10", "50%"]]
    beta_inf95 <- 10^tmpRes[["beta_log10", "2.5%"]]
    beta_sup95 <- 10^tmpRes[["beta_log10", "97.5%"]]

    outPost <- data.frame(parameters = c("hb", "kd", "mw", "beta"),
                          median = c(hb_med, kd_med, mw_med, beta_med),
                          Q2.5 = c(hb_inf95, kd_inf95, mw_inf95, beta_inf95),
                          Q97.5 = c(hb_sup95, kd_sup95, mw_sup95, beta_sup95))
  }

  # Format and output
  outPrior <- format(data.frame(outPrior), scientific = TRUE, digit = 6)
  outPost <- format(data.frame(outPost), scientific = TRUE, digit = 6)
  maxRhat <- max(rstan::summary(object$stanFit)$summary[,"Rhat"], na.rm= TRUE)
  minBulk_ESS <- min(tmpRes$Bulk_ESS)
  minTail_ESS <- min(tmpRes$Tail_ESS)

  cat("Summary: \n\n")
  cat("Bayesian Inference performed with Stan.\n",
      "Model type:", object$modelType, "\n",
      "Bee species:", object$data$beeSpecies, "\n\n",
      "MCMC sampling setup (select with '$setupMCMC')\n",
      "Iterations:", object$setupMCMC$nIter, "\n",
      "Warmup iterations:", object$setupMCMC$nWarmup, "\n",
      "Thinning interval:", object$setupMCMC$thinInterval, "\n",
      "Number of chains:", object$setupMCMC$nChains)
  cat("\n\nPriors of the parameters (quantiles) (select with '$Qpriors'):\n\n")
  print(outPrior, row.names = FALSE)
  cat("\nPosteriors of the parameters (quantiles) (select with '$Qposteriors'):\n\n")
  print(outPost, row.names = FALSE)
  cat("\n\n Maximum Rhat computed (na.rm = TRUE):", maxRhat, "\n",
      "Minimum Bulk_ESS:", minBulk_ESS, "\n",
      "Minimum Tail_ESS:", minTail_ESS, "\n",
      "Bulk_ESS and Tail_ESS are crude measures of effecting sampling size for
      bulk and tail quantities respectively. An ESS > 100 per chain can be
      considered as a good indicator. Rhat is an indicator of chains convergence.
      A Rhat <= 1.05 is a good indicator of convergence. For detail results,
      one can call 'rstan::monitor(YOUR_beeSurvFit_OBJECT$stanFit)")

  invisible(list(
      setupMCMC = object$setupMCMC,
      Qpriors = outPrior,
      Qposteriors = outPost))
}



