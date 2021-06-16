#' Validate method for \code{beeSurvFit} objects
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
#' @return
#' @export
#'
#' @examples
#' #' \dontrun{
#' dataValidate <- betacyfluthrinChronic
#' object <- fitBetacyfluthrin_Chronic
#' validate <- validate.beeSurvFit(object, dataValidate)
#' }
validate.beeSurvFit <- function(object,
                               dataValidate,
                               ...) {

  # Check for correct class
  if (!is(object,"beeSurvFit")) {
    stop("predict.beeSurvFit: an object of class 'beeSurvFit' is expected")
  }

  ### we prepare experimental dataset for
  time <- dataValidate$survData_long$SurvivalTime
  conc <- dataValidate$survData_long$Treatment
  NSurv <- dataValidate$survData_long$NSurv
  data <- data.frame(time, conc, replicate=NA, NSurv)
  data$replicate <- c(rep("rep1", nrow(dataValidate)))  ## do we have replicates at all in our standard experimental data?

  ## we run the predict.beeSurvFit function based on the experimental data and the fit object
  lsOut <- predict.beeSurvFit(fitBetacyfluthrin_Chronic, data)


  class(lsOut) <- "beeSurvValid"

  return(lsOut)
}
