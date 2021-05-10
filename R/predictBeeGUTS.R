#' Predict method for \code{beeSurvFit} objects
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
predict.beeSurvFit <- function(object,
                           dataPredict,
                           ...) {

  # Check for correct class
  if (!is(object,"beeSurvFit")) {
    stop("predict.beeSurvFit: an object of class 'beeSurvFit' is expected")
  }


  # Transform
  morseObject <- list(mcmc = rstan::As.mcmc.list(object$stanFit, pars = c("hb_log10", "kd_log10", "zw_log10", "bw_log10")),
                      model_type = object$modelType)
  class(morseObject) <- "survFit"

  for(i in 1:object$setupMCMC$nChains) {
    colnames(morseObject$mcmc[[i]]) <- c("hb_log10", "kd_log10", "z_log10", "kk_log10")
  }

  # Perform predictions using the morse package
  outMorse <- morse::predict_ode(morseObject, dataPredict, hb_value = FALSE, hb_valueFORCED  = 0, ...)

  # Calculate summary to embed mean posteriors values with outputs
  invisible(utils::capture.output(outSummary <- summary(object)))

  # Return
  lsOut <- list(parsPost = outSummary$Qposteriors,
                modelType = object$modelType,
                setupMCMC = object$setupMCMC,
                sim = outMorse$df_quantile
                )
  class(lsOut) <- "beeSurvPred"

  return(lsOut)
}
