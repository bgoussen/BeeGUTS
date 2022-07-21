#' @title Validation method for \code{beeSurvFit } objects
#'
#' @description This is a \code{validation} method for the
#' \code{beeSurvFit} object. It perform forwards predictions for a specific concentration
#' profile and compare these prediction to the respective experimental data.
#'
#' @param object An object of class \code{beeSurvFit}
#' @param dataValidate Data to validate in the format of the experimental data used for fit (dataGUTS)
#' @param ... Additional arguments to be parsed to the  \code{predict.survFit} method from \code{odeGUTS} (e.g.
#'  \code{mcmc_size = 1000} is to be used to reduce the number of mcmc samples in order to speed up
#'  the computation. \code{mcmc_size} is the number of selected iterations for one chain. Default
#'  is 1000. If all MCMC is wanted, set argument to \code{NULL}.,
#'  \code{hb_value  = FALSE} the background mortality \code{hb} is taken into account from the posterior.
#' If \code{FALSE}, parameter \code{hb} is set to a fixed value. The default is \code{FALSE}.
#'  \code{hb_valueFORCED  = 0} hb_valueFORCED If \code{hb_value} is \code{FALSE}, it fix \code{hb}. The default is \code{0}
#'
#' @return An object of class \code{beeSurvValidation}.
#'
#' @export
validate <- function(object,
                     dataValidate,
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
#' @param ... Additional arguments to be parsed to the  \code{predict.survFit} method from \code{odeGUTS} (e.g.
#'  \code{mcmc_size = 1000} is to be used to reduce the number of mcmc samples in order to speed up
#'  the computation. \code{mcmc_size} is the number of selected iterations for one chain. Default
#'  is 1000. If all MCMC is wanted, set argument to \code{NULL}.,
#'  \code{hb_value  = FALSE} the background mortality \code{hb} is taken into account from the posterior.
#' If \code{FALSE}, parameter \code{hb} is set to a fixed value. The default is \code{FALSE}.
#'  \code{hb_valueFORCED  = 0} hb_valueFORCED If \code{hb_value} is \code{FALSE}, it fix \code{hb}. The default is \code{0}
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

  # Perform predictions using the odeGUTS package
  outMorse <- odeGUTS::predict_Nsurv_ode(morseObject, dfData, ...)


  # Calculate EFSA criteria using the odeGUTS package
  EFSA_Criteria <- odeGUTS::predict_Nsurv_check(outMorse, ...)

  # Calculate summary to embed mean posteriors values with outputs
  invisible(utils::capture.output(outSummary <- summary(object)))

  # Return
  lsOut <- list(parsPost = outSummary$Qposteriors,
                modelType = object$modelType,
                unitData = object$data$unitData,
                beeSpecies = object$data$beeSpecies,
                typeData = dataValidate$typeData,
                setupMCMC = object$setupMCMC,
                sim = outMorse$df_quantile,
                EFSA = EFSA_Criteria,
                data = dataValidate$concData_long,
                dataModel = dataValidate$concModel_long
  )


  class(lsOut) <- "beeSurvValidation"

  return(lsOut)
}

