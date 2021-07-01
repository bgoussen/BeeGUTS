# From morse package v3.3.1 29.06.2021
# Functions imported due to dependency issues with rjags
# morse package under GPL (>=2) https://cran.r-project.org/package=morse

#' Checking goodness-of-fit method for \code{survFitPredict} and
#'  \code{survFitPredict_Nsurv} objects
#'
#' Function from the \code{morse v 3.3.1} package.
#' It returns measures of goodness-of-fit for predictions.
#'
#' @rdname predict_check
#'
#' @param object an object used to select a method \code{predict_Nsurv_check}
#' @param \dots Further arguments to be passed to generic methods
#'
#' @export
predict_Nsurv_check <- function(object, ...){
  UseMethod("predict_Nsurv_check")
}

#' Compute criteria to check model performance
#'
#' Function from the \code{morse v 3.3.1} package.
#' Provide various criteria for assessment of the model performance:
#' (i) percentage of observation within the 95\% credible
#' interval of the Posterior Prediction Check (PPC), the Normalised Root Mean
#' Square Error (NRMSE) and the Survival Probability Prediction Error (SPPE) as
#' reccommended by the recent Scientific Opinion from EFSA (2018).
#'
#' @rdname predict_check
#'
#' @param object an object of class \code{survFitPredict_Nsurv}
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return The function return a list with three items:
#' \item{PPC}{The criterion, in percent, compares the predicted median numbers
#' of survivors associated to their uncertainty limits with the observed numbers
#' of survivors. Based on experience, PPC resulting in less than \eqn{50\%} of the
#' observations within the uncertainty limits indicate poor model performance. A fit of
#' \eqn{100\%} may hide too large uncertainties of prediction (so covering all data).}
#' \item{PPC_global}{percentage of PPC for the whole data set by gathering replicates.}
#' \item{NRMSE}{The criterion, in percent, is based on the classical root-mean-square error (RMSE),
#'  used to aggregate the magnitudes of the errors in predictions for various time-points
#'  into a single measure of predictive power. In order to provide a criterion expressed
#'   as a percentage, NRMSE is the normalised RMSE by the mean of the observations.}
#'  \item{NRMSE_global}{NRMSE for the whole data set by gathering replicates.}
#' \item{SPPE}{The SPPE indicator, in percent, is negative (between \eqn{0} and \eqn{-100\%}) for an
#' underestimation of effects, and positive (between \eqn{0} and \eqn{100}) for an
#' overestimation of effects. An SPPE value of \eqn{0} means an exact prediction
#'  of the observed survival probability at the end of the exposure profile.}
#'
#'  @references
#'  EFSA PPR Scientific Opinion (2018)
#' \emph{Scientific Opinion on the state of the art of Toxicokinetic/Toxicodynamic (TKTD) effect models for regulatory risk assessment of pesticides for aquatic organisms}
#' \url{https://www.efsa.europa.eu/en/efsajournal/pub/5377}
#'
#' @export
predict_Nsurv_check.survFitPredict_Nsurv <- function(object, ...){

  df_global <- object$df_quantile %>%
    mutate(#ppc_matching_check = ifelse(Nsurv_qinf95_check > Nsurv | Nsurv_qsup95_check < Nsurv, 0, 1),
      ppc_matching_valid = ifelse(Nsurv_qinf95_valid > Nsurv | Nsurv_qsup95_valid < Nsurv, 0, 1),
      SE_id = (Nsurv - Nsurv_q50_valid)^2)

  #percent_ppc_graphic <- sum(df_global$ppc_matching_check) / nrow(df_global) * 100

  df_ppc <- df_global %>%
    select(Nsurv, time, Nsurv_q50_valid, Nsurv_q50_check, replicate, ppc_matching_valid) %>%
    group_by(replicate) %>%
    summarise(PPC = mean(ppc_matching_valid)*100)

  percent_ppc_timeserie <- sum(df_global$ppc_matching_valid) / nrow(df_global) * 100

  # --- NRMSE
  df_nrmse <- df_global %>%
    select(Nsurv, time, Nsurv_q50_valid, Nsurv_q50_check, replicate, SE_id) %>%
    group_by(replicate) %>%
    summarise(NRMSE = sqrt(mean(SE_id)) / mean(Nsurv) * 100)

  nrmse <- sqrt(mean(df_global$SE_id)) / mean(df_global$Nsurv) * 100
  ## version with distribution
  # rmse <- (jags.data$Nsurv - t(df_sim))^2
  # y_mean <- mean(jags.data$Nsurv)
  # nrmse <- rmse / y_mean

  # --- SPPE

  df_sppe <- df_global %>%
    select(Nsurv, time, Nsurv_q50_valid, Nsurv_q50_check, replicate) %>%
    group_by(replicate) %>%
    arrange(replicate,time) %>%
    summarise(SPPE = (last(Nsurv) - last(Nsurv_q50_valid)) / first(Nsurv) * 100 )
  # summarise(sppe_check = (last(Nsurv) - last(Nsurv_q50_check)) / first(Nsurv) * 100,
  #           sppe_valid = (last(Nsurv) - last(Nsurv_q50_valid)) / first(Nsurv) * 100 )


  return( list(Percent_PPC = as.data.frame(df_ppc),
               Percent_PPC_global = percent_ppc_timeserie,
               Percent_NRMSE = as.data.frame(df_nrmse),
               Percent_NRMSE_global = nrmse,
               Percent_SPPE = as.data.frame(df_sppe))
  )

}










#' Predict method for \code{survFit} objects
#'
#' Function from the \code{morse v 3.3.1} package.
#' This is a \code{method} to replace function \code{predict} used on \code{survFit}
#' object when computing issues happen. \code{predict_ode} uses the \code{deSolve}
#' library to improve robustness. However, time to compute may be longer.
#'
#'
#' @param object an object used to select a method \code{ppc}
#' @param \dots Further arguments to be passed to generic methods
#' @export
predict_ode <- function(object, ...){
  UseMethod("predict_ode")
}

#' Predict method for \code{survFit} objects
#'
#' Function from the \code{morse v 3.3.1} package.
#' This is the generic \code{predict} S3 method for the \code{survFit} class.
#' It provides predicted survival rate for "SD" or "IT" models under constant or time-variable exposure.
#'
#' @param object An object of class \code{survFit}.
#' @param data_predict A dataframe with three columns \code{time}, \code{conc} and \code{replicate}
#'  used for prediction. If \code{NULL}, prediction is based on \code{x} object of
#'  class \code{survFit} used for fitting.
#' @param spaghetti If \code{TRUE}, return a set of survival curves using
#' parameters drawn from the posterior distribution.
#' @param mcmc_size Can be used to reduce the number of mcmc samples in order to speed up
#'  the computation. \code{mcmc_size} is the number of selected iterations for one chain. Default
#'  is 1000. If all MCMC is wanted, set argument to \code{NULL}.
#' @param hb_value If \code{TRUE}, the background mortality \code{hb} is taken into account from the posterior.
#' If \code{FALSE}, parameter \code{hb} is set to a fixed value. The default is \code{TRUE}.
#' @param interpolate_length Length of the time sequence for which output is wanted.
#' @param interpolate_method The interpolation method for concentration. See package \code{deSolve} for details.
#' Default is \code{linear}.
#' @param  hb_valueFORCED If \code{hb_value} is \code{FALSE}, it fix \code{hb}.
#' @param \dots Further arguments to be passed to generic methods
#'
#' @examples
#'
#' # (1) Load the survival data
#' data("propiconazole_pulse_exposure")
#'
#' # (2) Create an object of class "survData"
#' dataset <- survData(propiconazole_pulse_exposure)
#'
#' \dontrun{
#' # (3) Run the survFit function
#' out <- survFit(dataset , model_type = "SD")
#'
#' # (4) Create a new data table for prediction
#' data_4prediction <- data.frame(time = 1:10,
#'                                conc = c(0,5,30,30,0,0,5,30,15,0),
#'                                replicate= rep("predict", 10))
#'
#' # (5) Predict on a new data set
#' predict_out <- predict_ode(object = out, data_predict = data_4prediction,
#'                            mcmc_size = 1000, spaghetti = TRUE)
#'
#' }
#'
#' @import deSolve
#' @importFrom stats approxfun
#'
#' @export
predict_ode.survFit <- function(object,
                                data_predict = NULL,
                                spaghetti = FALSE,
                                mcmc_size = 1000,
                                hb_value = TRUE,
                                interpolate_length = 100,
                                interpolate_method = "linear",
                                hb_valueFORCED = NA,
                                ...) {
  x <- object # Renaming to satisfy CRAN checks on S3 methods
  # arguments should be named the same when declaring a
  # method and its instantiations

  # Initialisation
  mcmc <- x$mcmc
  model_type <- x$model_type

  if(is.null(data_predict)){
    x_interpolate = data.frame(
      time = x$jags.data$time,
      conc = x$jags.data$conc,
      replicate = x$jags.data$replicate)
  }
  if(!is.null(data_predict)){
    x_interpolate <- data_predict
  }

  df <- data.frame(
    time = x_interpolate$time,
    conc = x_interpolate$conc,
    replicate = x_interpolate$replicate)

  unique_replicate <- unique(df$replicate)

  ls_time <- list()
  ls_conc <- list()

  for(i in 1:length(unique_replicate)){

    ls_time[[i]] <- dplyr::filter(df, replicate == unique_replicate[i])$time
    ls_conc[[i]] <- dplyr::filter(df, replicate == unique_replicate[i])$conc

  }
  # ------- Computing

  mcmc.samples = mcmc

  if(!is.null(mcmc_size)){
    reduc_tab = lapply(mcmc.samples, "[",
                       seq(1, nrow(mcmc.samples[[1]]), length = mcmc_size),
                       1:ncol(mcmc.samples[[1]]))
    mcmc.samples = reduc_tab
  }

  mctot = do.call("rbind", mcmc.samples)
  #if(is.null(mcmc_size)){
  mcmc_size = nrow(mctot)
  #}

  kd = 10^mctot[, "kd_log10"]

  if(hb_value == TRUE){
    hb <- 10^mctot[, "hb_log10"]
  } else if(hb_value == FALSE){
    if(is.na(hb_valueFORCED)){
      if(is.na(x$hb_valueFIXED)){
        stop("Please provide value for `hb` using `hb_valueFORCED`.")
      } else{
        hb <- rep(x$hb_valueFIXED, nrow(mctot))
      }
    } else{
      hb <- rep(hb_valueFORCED, nrow(mctot))
    }
  }

  k = 1:length(unique_replicate)

  if(model_type == "SD"){
    kk <- 10^mctot[, "kk_log10"]
    z <- 10^mctot[, "z_log10"]

    dtheo = lapply(k, function(kit) { # For each replicate
      SurvSD_ode(Cw = ls_conc[[kit]],
                 time = ls_time[[kit]],
                 replicate = unique_replicate[kit],
                 kk=kk,
                 kd=kd,
                 hb=hb,
                 z=z,
                 mcmc_size = mcmc_size,
                 interpolate_length = interpolate_length,
                 interpolate_method = interpolate_method)
    })

  }
  if(model_type == "IT"){

    alpha <- 10^mctot[, "alpha_log10"]
    beta <- 10^mctot[, "beta_log10"]

    dtheo = lapply(k, function(kit) { # For each replicate
      SurvIT_ode(Cw = ls_conc[[kit]],
                 time = ls_time[[kit]],
                 replicate = unique_replicate[kit],
                 kd = kd,
                 hb = hb,
                 alpha = alpha,
                 beta = beta,
                 mcmc_size = mcmc_size,
                 interpolate_length = interpolate_length,
                 interpolate_method = interpolate_method)
    })

  }

  # Transpose
  df_theo <- do.call("rbind", dtheo)

  df_quantile = select(df_theo, time, conc, replicate, q50, qinf95, qsup95)

  if(spaghetti == TRUE){
    df_spaghetti <- df_theo
  } else df_spaghetti <- NULL

  return_object <- list(df_quantile = df_quantile,
                        df_spaghetti = df_spaghetti)

  class(return_object) <- c(class(return_object), "survFitPredict")

  return(return_object)

}

# Survival function for "IT" model with external concentration changing with time
#
# Function from the \code{morse v 3.3.1} package.
# @param Cw A scalar of external concentration
# @param time A vector of time
# @param kk a vector of parameter
# @param kd a vector of parameter
# @param z a vector of parameter
# @param hb a vector of parameter
#
#
# @return A matrix generate with coda.samples() function
#

SurvSD_ode <- function(Cw, time, replicate, kk, kd, z, hb, mcmc_size = 1000, interpolate_length = NULL, interpolate_method=c("linear","constant")) {
  interpolate_method <- match.arg(interpolate_method)

  ## external signal with several rectangle impulses
  signal <- data.frame(times=time,import=Cw)
  if(!is.null(interpolate_length)){
    times <- seq(min(time), max(time), length = interpolate_length)
  } else{
    times <- signal$times
  }

  xstart <- c(rep(c(D=0),mcmc_size),rep(c(H=0),mcmc_size))
  # ordering of parameters required by compiled function
  parms <- c(mcmc_size,kd,hb,z,kk)
  # solve model
  on.exit(.C("gutsredsd_free")) # clean up
  deSolve::ode(y=xstart,
               times=times,
               parms=parms,
               method="lsoda",
               dllname="BeeGUTS",
               initfunc="gutsredsd_init",
               func="gutsredsd_func",
               initforc="gutsredsd_forc",
               forcings=signal,
               fcontrol=list(method=interpolate_method,rule=2,ties="ordered"),
               nout=1
  ) -> out

  dtheo <- exp(-out[,grep("H",colnames(out))])

  # Manage vector case
  if(mcmc_size == 1){
    q50 = dtheo
    qinf95 = dtheo
    qsup95 = dtheo
  } else{
    qs <- apply(as.matrix(dtheo), 1, quantile, probs=c(0.5,0.025,0.975), names=FALSE, na.rm=TRUE)
    q50 = qs[1,]
    qinf95 = qs[2,]
    qsup95 = qs[3,]
  }

  dtheo <- as.data.frame(dtheo)
  names(dtheo) <- paste0("H",seq(1,mcmc_size))
  dtheo <- dtheo %>%
    dplyr::mutate(time = times,
                  conc = out[,ncol(out)],
                  replicate = c(replicate),
                  q50 = q50,
                  qinf95 = qinf95,
                  qsup95 = qsup95)

  return(dtheo)
}

# Survival function for "IT" model with external concentration changing with time
#
# Function from the \code{morse v 3.3.1} package.
# @param Cw A vector of external concentration
# @param time A vector of time
# @param replicate A scalar of char
# @param kk a vector of parameter
# @param kd a vector of parameter
# @param z a vector of parameter
# @param hb a vector of parameter
#
#
# @return A matrix generate with coda.samples() function
#

SurvIT_ode <- function(Cw, time, replicate, kd, hb, alpha, beta, mcmc_size = NULL, interpolate_length = NULL, interpolate_method=c("linear","constant")){
  interpolate_method <- match.arg(interpolate_method)

  ## external signal with several rectangle impulses
  signal <- data.frame(times=time,import=Cw)
  if(!is.null(interpolate_length)){
    times <- seq(min(time), max(time), length = interpolate_length)
  } else{
    times <- signal$times
  }

  ## The parameters
  parms  <- c(mcmc_size,kd,hb)

  ## Start values for steady state
  xstart <- c(rep(c(D=0),mcmc_size),rep(c(H=0),mcmc_size))

  ## Solve model
  #on.exit(.C("callCgutsredit_free")) # clean up
  deSolve::ode(y=xstart,
               times=times,
               parms=parms,
               method="lsoda",
               dllname="BeeGUTS",
               initfunc="gutsredit_init",
               func="gutsredit_func",
               initforc="gutsredit_forc",
               forcings=signal,
               fcontrol=list(method=interpolate_method,rule=2,ties="ordered"),
               nout=1
  ) -> out

  D <- out[,grep("D",colnames(out))]
  cumMax_D <- if(is.null(dim(D))) cummax(D) else apply(D, 2, cummax)
  thresholdIT <- t(1 / (1 + (t(cumMax_D) / alpha)^(-beta)))

  dtheo <- (1 - thresholdIT) * exp(times %*% t(-hb))

  # Manage vector case
  if(mcmc_size == 1){
    q50 = dtheo
    qinf95 = dtheo
    qsup95 = dtheo
  } else{
    qs <- apply(as.matrix(dtheo), 1, quantile, probs=c(0.5,0.025,0.975), names=FALSE, na.rm=TRUE)
    q50 = qs[1,]
    qinf95 = qs[2,]
    qsup95 = qs[3,]
  }

  dtheo <- as.data.frame(dtheo)
  names(dtheo) <- paste0("H",seq(1,mcmc_size))
  dtheo <- dtheo %>%
    dplyr::mutate(time = out[, "time"],
                  conc = out[,ncol(out)],
                  replicate = c(replicate),
                  q50 = q50,
                  qinf95 = qinf95,
                  qsup95 = qsup95)

  return(dtheo)

}










#' Predict method for \code{survFit} objects
#'
#' Function from the \code{morse v 3.3.1} package.
#' This is a \code{method} to replace function \code{predict_Nsurv} used on \code{survFit}
#' object when computing issues happen. \code{predict_nsurv_ode} uses the \code{deSolve}
#' library to improve robustness. However, time to compute may be longer.
#'
#' @rdname predict
#'
#' @param object An object of class \code{survFit}.
#' @param data_predict A dataframe with three columns \code{time}, \code{conc} and \code{replicate}
#'  used for prediction. If \code{NULL}, prediction is based on \code{x} object of
#'  class \code{survFit} used for fitting.
#' @param spaghetti If \code{TRUE}, return a set of survival curves using
#' parameters drawn from the posterior distribution.
#' @param mcmc_size Can be used to reduce the number of mcmc samples in order to speed up
#'  the computation. \code{mcmc_size} is the number of selected iterations for one chain. Default
#'  is 1000. If all MCMC is wanted, set argument to \code{NULL}.
#' @param hb_value If \code{TRUE}, the background mortality \code{hb} is taken into account from the posterior.
#' If \code{FALSE}, parameter \code{hb} is set to 0. The default is \code{TRUE}.
#' @param  hb_valueFORCED If \code{hb_value} is \code{FALSE}, it fix \code{hb}.
#' @param extend_time Length of time points interpolated with variable exposure profiles.
#' @param interpolate_length Length of the time sequence for which output is wanted.
#' @param interpolate_method The interpolation method for concentration. See package \code{deSolve} for details.
#' Default is \code{linear}.
#' @param \dots Further arguments to be passed to generic methods
#'
#' @export
predict_Nsurv_ode <- function(object,
                              data_predict,
                              spaghetti,
                              mcmc_size,
                              hb_value,
                              hb_valueFORCED,
                              extend_time,
                              interpolate_length,
                              interpolate_method,
                              ...){
  UseMethod("predict_Nsurv_ode")
}

#' @import deSolve
#' @importFrom stats approxfun
#'
#' @export
predict_Nsurv_ode.survFit <- function(object,
                                      data_predict = NULL,
                                      spaghetti = FALSE,
                                      mcmc_size = 1000,
                                      hb_value = TRUE,
                                      hb_valueFORCED = NA,
                                      extend_time = 100,
                                      interpolate_length = NULL,
                                      interpolate_method = "linear",
                                      ...) {
  x <- object # Renaming to satisfy CRAN checks on S3 methods
  # arguments should be named the same when declaring a
  # method and its instantiations

  if(!("Nsurv" %in% colnames(data_predict))){
    warning("Please provide a column 'Nsurv' in the 'data_predict' argument to have
            prediction on the Number of survivor.")
  }

  message("Note that computing can be quite long (several minutes).
  Tips: To reduce that time you can reduce Number of MCMC chains (default mcmc_size is set to 1000).")

  # Initialisation
  mcmc <- x$mcmc
  model_type <- x$model_type
  extend_time <- extend_time

  if(is.null(data_predict)){
    if("survFitVarExp" %in% class(x)){
      x_interpolate = data.frame(
        time = x$jags.data$time_long,
        conc = x$jags.data$conc_long,
        replicate = x$jags.data$replicate_long)
    } else{
      data_predict = data.frame(
        time = x$jags.data$time,
        conc = x$jags.data$conc,
        replicate = x$jags.data$replicate,
        Nsurv = x$jags.data$Nsurv)

      x_interpolate <- predict_interpolate(data_predict,  extend_time = extend_time) %>%
        dplyr::arrange(replicate, time)
    }
  }
  if(!is.null(data_predict)){
    x_interpolate <- predict_interpolate(data_predict,  extend_time = extend_time) %>%
      dplyr::arrange(replicate, time)
  }

  df <- data.frame(
    time = x_interpolate$time,
    conc = x_interpolate$conc,
    replicate = x_interpolate$replicate)

  unique_replicate <- unique(df$replicate)

  ls_time <- list()
  ls_conc <- list()

  for(i in 1:length(unique_replicate)){

    ls_time[[i]] <- dplyr::filter(df, replicate == unique_replicate[i])$time
    ls_conc[[i]] <- dplyr::filter(df, replicate == unique_replicate[i])$conc

  }

  # ------- Computing
  mcmc.samples = mcmc

  if(!is.null(mcmc_size)){
    reduc_tab = lapply(mcmc.samples, "[",
                       seq(1, nrow(mcmc.samples[[1]]), length = mcmc_size),
                       1:ncol(mcmc.samples[[1]]))
    mcmc.samples = reduc_tab
  }

  mctot = do.call("rbind", mcmc.samples)
  mcmc_size = nrow(mctot)

  kd = 10^mctot[, "kd_log10"]

  if(hb_value == TRUE){
    # "hb" is not in survFit object of morse <v3.2.0
    if("hb" %in% colnames(mctot)){
      hb <- mctot[, "hb"]
    } else{ hb <- 10^mctot[, "hb_log10"] }
  } else if(hb_value == FALSE){
    if(is.na(hb_valueFORCED)){
      if(is.na(x$hb_valueFIXED)){
        stop("Please provide value for `hb` using `hb_valueFORCED`.")
      } else{
        hb <- rep(x$hb_valueFIXED, nrow(mctot))
      }
    } else{
      hb <- rep(hb_valueFORCED, nrow(mctot))
    }
  }

  k = 1:length(unique_replicate)

  if(model_type == "SD"){
    kk <- 10^mctot[, "kk_log10"]
    z <- 10^mctot[, "z_log10"]

    dtheo = lapply(k, function(kit) { # For each replicate
      SurvSD_ode(Cw = ls_conc[[kit]],
                 time = ls_time[[kit]],
                 replicate = unique_replicate[kit],
                 kk=kk,
                 kd=kd,
                 hb=hb,
                 z=z,
                 mcmc_size = mcmc_size,
                 interpolate_length = interpolate_length,
                 interpolate_method = interpolate_method)
    })

  }
  if(model_type == "IT"){

    alpha <- 10^mctot[, "alpha_log10"]
    beta <- 10^mctot[, "beta_log10"]

    dtheo = lapply(k, function(kit) { # For each replicate
      SurvIT_ode(Cw = ls_conc[[kit]],
                 time = ls_time[[kit]],
                 replicate = unique_replicate[kit],
                 kd = kd,
                 hb = hb,
                 alpha = alpha,
                 beta = beta,
                 mcmc_size = mcmc_size,
                 interpolate_length = interpolate_length,
                 interpolate_method = interpolate_method)
    })
  }
  # Transpose
  df_theo <- do.call("rbind", dtheo)
  # dtheo <- do.call("rbind", lapply(dtheo, t))

  # Computing Nsurv
  df_mcmc <- as_tibble(do.call("rbind", x$mcmc))
  NsurvPred_valid <- select(df_mcmc, contains("Nsurv_sim"))
  NsurvPred_check <- select(df_mcmc, contains("Nsurv_ppc"))

  if(is.null(data_predict) &
     # The following condition are always true for survFit done after morse v3.2.0 !
     ncol(NsurvPred_valid) > 0 &
     ncol(NsurvPred_check) > 0){

    df_quantile <- data.frame(
      time = data_predict$time,
      conc = data_predict$conc,
      replicate = data_predict$replicate,
      Nsurv = data_predict$Nsurv,
      Nsurv_q50_check = apply(NsurvPred_check, 1, quantile, probs = 0.5, na.rm = TRUE),
      Nsurv_qinf95_check = apply(NsurvPred_check, 1, quantile, probs = 0.025, na.rm = TRUE),
      Nsurv_qsup95_check = apply(NsurvPred_check, 1, quantile, probs = 0.975, na.rm = TRUE),
      Nsurv_q50_valid = apply(NsurvPred_valid, 1, quantile, probs = 0.5, na.rm = TRUE),
      Nsurv_qinf95_valid = apply(NsurvPred_valid, 1, quantile, probs = 0.025, na.rm = TRUE),
      Nsurv_qsup95_valid = apply(NsurvPred_valid, 1, quantile, probs = 0.975, na.rm = TRUE))

  } else{
    # --------------------

    df_psurv <- as_tibble(df_theo) %>%
      select(-conc) %>%
      mutate(time = df$time,
             replicate = df$replicate)

    df_filter <- dplyr::inner_join(df_psurv, data_predict, by = c("replicate", "time")) %>%
      filter(!is.na(Nsurv)) %>%
      group_by(replicate) %>%
      arrange(replicate, time) %>%
      mutate(Nprec = ifelse(time == min(time), Nsurv, lag(Nsurv)),
             iter = row_number(),
             iter_prec = ifelse(time == min(time), iter, lag(iter))) %>%
      ungroup()

    mat_psurv <- df_filter %>%
      select(- c("time", "conc", "replicate",
                 "q50", "qinf95", "qsup95",
                 "Nsurv", "Nprec", "iter", "iter_prec")) %>%
      as.matrix()

    ncol_NsurvPred <- ncol(mat_psurv)
    nrow_NsurvPred <- nrow(mat_psurv)
    iter = df_filter$iter
    iter_prec = df_filter$iter_prec

    NsurvPred_valid <- matrix(ncol = ncol_NsurvPred, nrow = nrow(mat_psurv))

    Nprec <- cbind(df_filter$Nprec)[, rep(1,ncol_NsurvPred)]

    mat_psurv_prec = matrix(ncol = ncol_NsurvPred, nrow = nrow_NsurvPred)
    for(i in 1:nrow_NsurvPred){
      if(iter[i] == iter_prec[i]){
        mat_psurv_prec[i,] = mat_psurv[i,]
      } else{
        mat_psurv_prec[i,] = mat_psurv[i-1,]
      }
    }
    mat_pSurv_ratio = mat_psurv / mat_psurv_prec

    NsurvPred_check_vector = rbinom(ncol_NsurvPred*nrow_NsurvPred,
                                    size = Nprec,
                                    prob =  mat_pSurv_ratio)
    NsurvPred_check = matrix(NsurvPred_check_vector, byrow = FALSE, nrow = nrow_NsurvPred)


    NsurvPred_valid[1, ] = rep(Nprec[1], ncol_NsurvPred)
    for(i in 2:nrow(mat_psurv)){
      if(iter[i] == iter_prec[i]){
        NsurvPred_valid[i,] = NsurvPred_check[i,]
      } else{
        NsurvPred_valid[i,] = rbinom(ncol_NsurvPred,
                                     size = NsurvPred_valid[i-1,],
                                     prob = mat_pSurv_ratio[i,])
      }
    }


    df_quantile <- data.frame(time = df_filter$time,
                              conc = df_filter$conc,
                              replicate = df_filter$replicate,
                              Nsurv = df_filter$Nsurv,
                              Nsurv_q50_check = apply(NsurvPred_check, 1, quantile, probs = 0.5, na.rm = TRUE),
                              Nsurv_qinf95_check = apply(NsurvPred_check, 1, quantile, probs = 0.025, na.rm = TRUE),
                              Nsurv_qsup95_check = apply(NsurvPred_check, 1, quantile, probs = 0.975, na.rm = TRUE),
                              Nsurv_q50_valid = apply(NsurvPred_valid, 1, quantile, probs = 0.5, na.rm = TRUE),
                              Nsurv_qinf95_valid = apply(NsurvPred_valid, 1, quantile, probs = 0.025, na.rm = TRUE),
                              Nsurv_qsup95_valid = apply(NsurvPred_valid, 1, quantile, probs = 0.975, na.rm = TRUE))

  }

  if(spaghetti == TRUE){
    random_column <- sample(1:ncol(NsurvPred_valid), size = round(10/100 * ncol(NsurvPred_valid)))
    df_spaghetti <- as_tibble(NsurvPred_valid[, random_column]) %>%
      mutate(time = data_predict$time,
             conc = data_predict$conc,
             replicate = data_predict$replicate,
             Nsurv = data_predict$Nsurv)
  } else df_spaghetti <- NULL

  #ls_check_on_Nsurv <- check_on_Nsurv(df_quantile)

  return_object <- list(df_quantile = df_quantile,
                        df_spaghetti = df_spaghetti)

  class(return_object) <- c(class(return_object), "survFitPredict_Nsurv")

  return(return_object)

}





# Create a dataset for survival analysis when the replicate of concentration is variable
#
# Function from the \code{morse v 3.3.1} package.
# @param x An object of class \code{survData}
# @param extend_time length of time points interpolated with variable exposure profiles
#
# @return A dataframe
#
predict_interpolate <- function(x, extend_time = 100){

  ## data.frame with time

  df_MinMax <- x %>%
    dplyr::group_by(replicate) %>%
    dplyr::summarise(min_time = min(time, na.rm = TRUE),
                     max_time = max(time, na.rm = TRUE)) %>%
    dplyr::group_by(replicate) %>%
    dplyr::do(tibble(replicate = .$replicate, time = seq(.$min_time, .$max_time, length = extend_time)))

  x_interpolate <- dplyr::full_join(df_MinMax, x,
                                    by = c("replicate", "time")) %>%
    dplyr::group_by(replicate) %>%
    dplyr::arrange(replicate, time) %>% # organize in replicate and time
    dplyr::mutate(conc = zoo::na.approx(conc, time, na.rm = FALSE)) %>%
    # from package zoo : 'na.locf()' carry the last observation forward to replace your NA values.
    dplyr::mutate(conc = ifelse(is.na(conc),zoo::na.locf(conc),conc) )

  return(x_interpolate)
}



