#' @title Validation method for \code{beeSurvFit } objects
#'
#' @description This is a \code{validation} method for the
#' \code{beeSurvFit} object. It perform forwards predictions for a specific concentration
#' profile and compare these prediction to the respective experimental data.
#'
#' @param object An object of class \code{beeSurvFit}
#' @param dataValidate Data to validate in the format of the experimental data used for fit (dataGUTS)
#' @param fithb Logical argument. If \code{TRUE}, control data of the validation experiment are
#'  fitted to estimate the value of the background mortality rate. If \code{FALSE},
#'  background mortality can be fixed with the optional argument \code{hb_valueFORCED}
#' @param ... Additional arguments to be parsed to the  \code{predict.survFit} method from \code{odeGUTS} (e.g.
#'  \code{mcmc_size = 1000} is to be used to reduce the number of mcmc samples in order to speed up
#'  the computation. \code{mcmc_size} is the number of selected iterations for one chain. Default
#'  is 1000. If all MCMC is wanted, set argument to \code{NULL}.,
#'  \code{hb_valueFORCED  = 0} hb_valueFORCED If \code{fithb} is \code{FALSE}, it fix \code{hb}. The default is \code{0}
#'
#' @return An object of class \code{beeSurvValidation}.
#'
#' @export
validate <- function(object,
                     dataValidate,
                     fithb = FALSE,
                     ...){
  UseMethod("validate")
}



#' Validate method for \code{beeSurvFit} objects
#'
#' @description This is the generic \code{validate} S3 method for the \code{beeSurvFit}
#' class. It predict the survival over time for the concentration profiles entered by the user.
#'
#' @param object An object of class \code{beeSurvFit}
#' @param dataValidate Data to validate in the format of the experimental data used for fit (dataGUTS)
#' @param fithb Logical argument. If \code{TRUE}, control data of the validation experiment are
#'  fitted to estimate the value of the background mortality rate. If \code{FALSE},
#'  background mortality can be fixed with the optional argument \code{hb_valueFORCED}
#' @param ... Additional arguments to be parsed to the  \code{predict.survFit} method from \code{odeGUTS} (e.g.
#'  \code{mcmc_size = 1000} is to be used to reduce the number of mcmc samples in order to speed up
#'  the computation. \code{mcmc_size} is the number of selected iterations for one chain. Default
#'  is 1000. If all MCMC is wanted, set argument to \code{NULL}.,
#'  \code{hb_valueFORCED  = 0} hb_valueFORCED If \code{fithb} is \code{FALSE}, it fix \code{hb}. The default is \code{0}
#'
#' @return A \code{beeSurvValidation} object with the results of the validation
#' @export
#'
#' @examples
#' \donttest{
#' data(betacyfluthrinChronic)
#' data(fitBetacyfluthrin_Chronic)
#' validation <- validate(fitBetacyfluthrin_Chronic, betacyfluthrinChronic)
#' }

validate.beeSurvFit <- function(object,
                                dataValidate,
                                fithb = FALSE,
                                ...) {

  # Check for correct class
  if (!is(object,"beeSurvFit")) {
    stop("predict.beeSurvFit: an object of class 'beeSurvFit' is expected")
  }
  # Ugly fix to keep the validate function the same
  # Validation data is always a single file, so collapse the list
  if (length(dataValidate$nDatasets)==1){
    for (name in names(dataValidate)) {dataValidate[name]<-dataValidate[name][[1]]}
  }

   ### prepare experimental dataset for
   dfData <- dplyr::full_join(dplyr::select(dataValidate$survData_long,!Dataset),
                            dplyr::select(dataValidate$concModel_long, !Dataset),
                            by =c("SurvivalTime", "Treatment"))
  colnames(dfData) <- c("time", "replicate", "Nsurv", "conc")

  dfData <- dfData[with(dfData, order(replicate, time)),]

  ## run prediction with odeGUTS::predict_Nsurv_ode function
  if(object$modelType == "SD"){
     morseObject <- list(mcmc = rstan::As.mcmc.list(object$stanFit, pars = c("kd_log10", "zw_log10", "bw_log10")),
                       model_type = object$modelType)
     class(morseObject) <- "survFit"

     for(i in 1:object$setupMCMC$nChains) {
       colnames(morseObject$mcmc[[i]]) <- c("kd_log10", "z_log10", "kk_log10")
     }
   } else if(object$modelType == "IT") {
     morseObject <- list(mcmc = rstan::As.mcmc.list(object$stanFit, pars = c("kd_log10", "mw_log10", "beta_log10")),
                         model_type = object$modelType)
     class(morseObject) <- "survFit"

     for(i in 1:object$setupMCMC$nChains) {
       colnames(morseObject$mcmc[[i]]) <- c("kd_log10", "alpha_log10", "beta_log10")
     }
   } else {
     stop("Wrong model type. Model type should be 'SD' or 'IT'")
   }

  if (fithb){
    hbfit = refit_hb(object, dataValidate)

    morseObj_hb = list(mcmc = rstan::As.mcmc.list(hbfit, pars = c("hb_log10")))
    for (i in 1:length(morseObject$mcmc)){
        morseObject$mcmc[[i]] = cbind(morseObject$mcmc[[i]],morseObj_hb$mcmc[[i]])
    }
    # Perform predictions using the odeGUTS package
    outMorse <- odeGUTS::predict_Nsurv_ode(morseObject, dfData, hb_value = TRUE,...)
  } else {
    hbfit = NULL
    # Perform predictions using the odeGUTS package
    outMorse <- odeGUTS::predict_Nsurv_ode(morseObject, dfData, hb_value = FALSE,...)
  }

  # Calculate EFSA criteria using the odeGUTS package
  EFSA_Criteria <- odeGUTS::predict_Nsurv_check(outMorse, ...)

  # Calculate summary to embed mean posteriors values with outputs
  invisible(utils::capture.output(outSummary <- summary(object)))

  if(object$data$beeSpecies != dataValidate$beeSpecies){
    warning("BeeGUTS was calibrated on a ", object$data$beeSpecies, " and is validated on a ", dataValidate$beeSpecies)
  }

  # Return
  lsOut <- list(parsPost = outSummary$Qposteriors,
                modelType = object$modelType,
                unitData = object$data$unitData,
                beeSpecies = object$data$beeSpecies,
                beeSpeciesVal = dataValidate$beeSpecies,
                typeData = dataValidate$typeData,
                setupMCMC = object$setupMCMC,
                sim = outMorse$df_quantile,
                EFSA = EFSA_Criteria,
                data = dataValidate$concData_long,
                dataModel = dataValidate$concModel_long,
                hbfit = hbfit
  )


  class(lsOut) <- "beeSurvValidation"

  return(lsOut)
}

refit_hb = function(object, dataValidate){
  # Need here to perform a fit of the hb value on the validation dataset
  cat("Fitting the background mortality parameter on the control data of the
        validation dataset.","\n")
  data_control = NULL
  data_control$survData_long[[1]] = dataValidate$survData_long %>%
    dplyr::filter(Treatment == "Control")
  data_control$concModel_long[[1]] = dataValidate$concModel_long %>%
    dplyr::filter(Treatment == "Control")
  priorlist = c(hbMean_log10 =  object$dataFit$hbMean_log10,
                hbSD_log10 = object$dataFit$hbSD_log10)
  data_control$nDatasets = 1
  lsStanData <- dataFitStan(data_control, "SD", NULL, priorlist)
  lsStanData$nGroups = 1
  lsStanData$groupDataset = 1

  modelObject <- stanmodels$GUTS_hb_only
  chcr <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(chcr) && chcr == "TRUE") {
    # this is needed in order to pass CRAN checks
    # use 2 cores in CRAN/Travis/AppVeyor
    nCores <- 2L
  } else {
    # use all cores in devtools::test()
    nCores = parallel::detectCores(logical = FALSE)-1L
  }
  op <- options()
  options(mc.cores = as.integer(nCores))
  on.exit(options(op))

  hbfit <- rstan::sampling(
    object = modelObject,
    data = lsStanData,
    chains = object$setupMCMC$nChains,
    iter = object$setupMCMC$nIter,
    warmup = object$setupMCMC$nWarmup,
    thin = object$setupMCMC$thinInterval,
    control = list(adapt_delta = 0.95))

  tmpRes <- rstan::monitor(hbfit, print = FALSE)
  maxRhat <- max(rstan::summary(hbfit)$summary[,"Rhat"], na.rm= TRUE)
  minBulk_ESS <- min(tmpRes$Bulk_ESS)
  minTail_ESS <- min(tmpRes$Tail_ESS)

  hb_med   <- 10^tmpRes[["hb_log10", "50%"]]
  hb_inf95 <- 10^tmpRes[["hb_log10", "2.5%"]]
  hb_sup95 <- 10^tmpRes[["hb_log10", "97.5%"]]
  outPost_hb <- data.frame(parameters = "hb",
                           median = hb_med,
                           Q2.5 = hb_inf95,
                           Q97.5 = hb_sup95)

  cat("Bayesian Inference performed with Stan.\n",
      "MCMC sampling setup (select with '$setupMCMC')\n",
      "Iterations:", object$setupMCMC$nIter, "\n",
      "Warmup iterations:", object$setupMCMC$nWarmup, "\n",
      "Thinning interval:", object$setupMCMC$thinInterval, "\n",
      "Number of chains:", object$setupMCMC$nChains,"\n")
  cat("\nMaximum Rhat computed (na.rm = TRUE):", maxRhat, "\n",
      "Minimum Bulk_ESS:", minBulk_ESS, "\n",
      "Minimum Tail_ESS:", minTail_ESS, "\n",
      "Bulk_ESS and Tail_ESS are crude measures of effecting sampling size for
      bulk and tail quantities respectively. An ESS > 100 per chain can be
      considered as a good indicator. Rhat is an indicator of chains convergence.
      A Rhat <= 1.05 is a good indicator of convergence. For detail results,
      one can call 'rstan::monitor(beeSurvValidation$hbfit)","\n\n")

  cat("Results for hb:","\n")
  print(outPost_hb, row.names = FALSE)

  return(hbfit)
}
