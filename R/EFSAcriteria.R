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
#' \item{NRMSE}{The criterion, in percent, is based on the classical root-mean-square error (RMSE),
#'  used to aggregate the magnitudes of the errors in predictions for various time-points
#'  into a single measure of predictive power. In order to provide a criterion expressed
#'   as a percentage, NRMSE is the normalised RMSE by the mean of the observations.
#'   EFSA (2018) recognised that a NRMSE of less than 50% indicates good model performance}
#' \item{SPPE}{A list with the Survival Probability Prediction Error per dataset and condition.
#'   Each dataset is in a sublist.}
#'
#'
#'  @references
#'  EFSA PPR Scientific Opinion (2018)
#' \emph{Scientific Opinion on the state of the art of Toxicokinetic/Toxicodynamic (TKTD) effect models for regulatory risk assessment of pesticides for aquatic organisms}
#' \url{https://www.efsa.europa.eu/en/efsajournal/pub/5377}
#'
#'  @example
#'  data(fitBetacyfluthrin_Chronic)
#'  out <- criteriaCheck(fitBetacyfluthrin_Chronic)
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


  # ---- SPPE
  dfDataSurv_long_full <- dplyr::bind_rows(x$data$survData_long)
  # extract the indices of the various datasets from the global table
  dfDataSurv_index = dfDataSurv_long_full %>%
    dplyr::mutate(idAll = dplyr::row_number() ) %>%
    dplyr::group_by(Dataset) %>%
    dplyr::summarise(idS_lw = min(idAll),
                     idS_up = max(idAll))
  #dfModelSurv <- as.data.frame(x$survModel)
  lsNsurv_sim <- rstan::extract(x$stanFit, pars = 'Nsurv_sim')

  lsDataSurv <- list()
  for (i in 1:x$data$nDatasets) {
    # Extract data
    dfDataSurv_long <- as.data.frame(x$data$survData_long[[i]])
    #Extract the right simulated values for each dataset
    lsNsurv_sim_dataset <- lsNsurv_sim[[1]][,dfDataSurv_index$idS_lw[i]:dfDataSurv_index$idS_up[i]]
    # Compute quantiles of simulations and compile all in a dataframe
    dfDataSurv_long$simQ50 <- apply(lsNsurv_sim_dataset, 2, quantile, 0.5)
    lsDataSurv[[i]] <- dfDataSurv_long[dfDataSurv_long$SurvivalTime == max(unique(dfDataSurv_long$SurvivalTime)),]
    lsDataSurv[[i]]$NSurvInit <-  dfDataSurv_long[dfDataSurv_long$SurvivalTime == min(unique(dfDataSurv_long$SurvivalTime)),"NSurv"]
    lsDataSurv[[i]]$SPPE <- (lsDataSurv[[i]]$NSurv - lsDataSurv[[i]]$simQ50) / lsDataSurv[[i]]$NSurvInit * 100
  }

  return(list(percentPPC = as.data.frame(dfPPC),
              percentNRMSE = as.data.frame(dfNRMSE),
              SPPE = lsDataSurv)

  )
}
