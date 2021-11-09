#' Computes PPC and NRMSE as defined in EFSA 2018
#'
#' @param x an object of class \code{beeSurvFit} or \code{beeSurvPred}
#'
#' @return The function returns a list with three items:
#' \item{PPC}{The criterion, in percent, compares the predicted median number of survivors associated
#' to their uncertainty limits with the observed numbers of survivors.
#' Based on experience, PPC resulting in more than \eqn{50\%} of the
#' observations within the uncertainty limits indicate good model performance (EFSA 2018). A fit of
#' \eqn{100\%} may hide too large uncertainties of prediction (so covering all data).}
#' \item{PPC_global}{percentage of PPC for the whole data set by gathering data types.}
#' \item{NRMSE}{The criterion, in percent, is based on the classical root-mean-square error (RMSE),
#'  used to aggregate the magnitudes of the errors in predictions for various time-points
#'  into a single measure of predictive power. In order to provide a criterion expressed
#'   as a percentage, NRMSE is the normalised RMSE by the mean of the observations.
#'   EFSA (2018) recognised that a NRMSE of less than 50% indicates good model performance}
#'
#'
#'  @references
#'  EFSA PPR Scientific Opinion (2018)
#' \emph{Scientific Opinion on the state of the art of Toxicokinetic/Toxicodynamic (TKTD) effect models for regulatory risk assessment of pesticides for aquatic organisms}
#' \url{https://www.efsa.europa.eu/en/efsajournal/pub/5377}
#'
#' @export
#'
criteriaCheck<- function(x){

  # --- PPC
  dfGlobal<- ppc(x) %>%
    dplyr::mutate(ppcMatching_valid = ifelse(value<q_0.025|value>q_0.975, 0, 1),
                  SE_id = (value - median)^2)

  dfPPC <- dfGlobal %>%
    dplyr::select(data, ppcMatching_valid) %>%
    dplyr::group_by(data) %>%
    dplyr::summarise(PPC = mean(ppcMatching_valid)*100)

  # --- NRMSE
  dfNRMSE <- dfGlobal %>%
    dplyr::select(value, data, SE_id) %>%
    dplyr::group_by(data) %>%
    dplyr::summarise(NRMSE = sqrt(mean(SE_id,  na.rm = TRUE)) / mean(value , na.rm = TRUE) * 100)


  return(list(percentPPC = as.data.frame(dfPPC),
              percentNRMSE = as.data.frame(dfNRMSE))

  )
}


