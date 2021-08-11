#' @title Validation method for \code{beeSurvFit } objects
#'
#' @description This is a \code{validation} method for the
#' \code{beeSurvFit} object. It perform forwards predictions for a specific concentration
#' profile and compare these prediction to the respective experimental data.
#'
#' @param object An object of class \code{beeSurvFit}
#' @param dataValidate Data to validate in the format of the experimental data used for fit (dataGUTS)
#' @param ... Additional arguments to be parsed to the  \code{predict.survFit} method from \code{morse} (e.g.
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
#' \dontrun{
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

  ### prepare experimental dataset for
  data <- dplyr::full_join(dataValidate$survData_long, dataValidate$concModel_long, by =c("SurvivalTime", "Treatment"))
  colnames(data) <- c("time", "replicate", "Nsurv", "conc")

  ## run prediction with odeGUTS::predict_Nsurv_ode function
  if(object$modelType == "SD"){
    morseObject <- list(mcmc = rstan::As.mcmc.list(object$stanFit, pars = c("hb_log10", "kd_log10", "zw_log10", "bw_log10")),
                        model_type = object$modelType)
    class(morseObject) <- "survFit"

    for(i in 1:object$setupMCMC$nChains) {
      colnames(morseObject$mcmc[[i]]) <- c("hb_log10", "kd_log10", "z_log10", "kk_log10")
    }
  } else if(object$modelType == "IT") {
    morseObject <- list(mcmc = rstan::As.mcmc.list(object$stanFit, pars = c("hb_log10", "kd_log10", "mw_log10", "beta_log10")),
                        model_type = object$modelType)
    class(morseObject) <- "survFit"

    for(i in 1:object$setupMCMC$nChains) {
      colnames(morseObject$mcmc[[i]]) <- c("hb_log10", "kd_log10", "alpha_log10", "beta_log10")
    }
  } else {
    stop("Wrong model type. Model type should be 'SD' or 'IT'")
  }

  # Perform predictions using the odeGUTS package
  outMorse <- odeGUTS::predict_Nsurv_ode(morseObject, data, hb_value = FALSE, hb_valueFORCED  = 0, ...)


  # Calculate EFSA criteria using the odeGUTS package
  EFSA_Criteria <- odeGUTS::predict_Nsurv_check(outMorse, ...)

  # Calculate summary to embed mean posteriors values with outputs
  invisible(utils::capture.output(outSummary <- summary(object)))

  # Return
  lsOut <- list(parsPost = outSummary$Qposteriors,
                modelType = object$modelType,
                unitData = object$data$unitData,
                beeSpecies = object$data$beeSpecies,
                typeData = object$data$typeData,
                setupMCMC = object$setupMCMC,
                sim = outMorse$df_quantile,
                EFSA = EFSA_Criteria,
                data = dataValidate$concData_long
  )


  class(lsOut) <- "beeSurvValidation"

  return(lsOut)
}

