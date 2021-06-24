#' Predict method for \code{beeSurvFit} objects
#'
#' @description This is the generic \code{predict} S3 method for the \code{beeSurvFit}
#' class. It predict the survival over time for the concentration profiles entered by the user.
#' No concentration reconstructions are performed here.
#'
#' @param object An object of class \code{beeSurvFit}
#' @param dataPredict Data to predict in the format XXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#' @param ... Additional arguments to be parsed to the  \code{predict.survFit} method from \code{morse} (e.g.
#'  \code{mcmc_size = 1000} is to be used to reduce the number of mcmc samples in order to speed up
#'  the computation. \code{mcmc_size} is the number of selected iterations for one chain. Default
#'  is 1000. If all MCMC is wanted, set argument to \code{NULL}.,
#'  \code{hb_value  = FALSE} the background mortality \code{hb} is taken into account from the posterior.
#' If \code{FALSE}, parameter \code{hb} is set to a fixed value. The default is \code{FALSE}.
#'  \code{hb_valueFORCED  = 0} hb_valueFORCED If \code{hb_value} is \code{FALSE}, it fix \code{hb}. The default is \code{0}
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' dataPredict <- data.frame(time = c(1:10, 1:10, 1:10), conc = c(rep(5, 10), rep(10, 10), rep(15, 10)), replicate = c(rep("rep1", 10), rep("rep2", 10), rep("rep3", 10)))
#' data(fitBetacyfluthrin_Chronic)
#' prediction <- predict(fitBetacyfluthrin_Chronic, dataPredict)
#' }
predict.beeSurvFit <- function(object,
                           dataPredict,
                           ...) {

  # Check for correct class
  if (!is(object,"beeSurvFit")) {
    stop("predict.beeSurvFit: an object of class 'beeSurvFit' is expected")
  }


  # Transform

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

  # Perform predictions using the morse package
  outMorse <- morse::predict_ode(morseObject, dataPredict, hb_value = FALSE, hb_valueFORCED  = 0, ...)

  # Calculate summary to embed mean posteriors values with outputs
  invisible(utils::capture.output(outSummary <- summary(object)))

  # Return
  lsOut <- list(parsPost = outSummary$Qposteriors,
                modelType = object$modelType,
                unitData = object$data$unitData,
                beeSpecies = object$data$beeSpecies,
                setupMCMC = object$setupMCMC,
                sim = outMorse$df_quantile
                )
  class(lsOut) <- "beeSurvPred"

  return(lsOut)
}

