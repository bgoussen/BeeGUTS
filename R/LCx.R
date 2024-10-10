#' Predict the Lethal Concentration method at which \eqn{x\%} of organisms die for any
#' specified time-point for a \code{beeSurvFit} object
#'
#' Predict median and 95% credible interval of the \eqn{x\%} Lethal Concentration.
#'
#' When class of \code{object} is \code{beeSurvFit}, see \link[=LCx.beeSurvFit]{LCx.beeSurvFit}.
#'
#' @rdname LCX
#'
#' @param object An object used to select a method
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return A \code{LCx} object containing the results of the lethal concentration predictions
#' @export
#'
LCx <- function(object, ...){
  UseMethod("LCx")
}



#' Predict the Lethal Concentration at which \eqn{x\%} of organisms die for any
#' specified time-point for a \code{beeSurvFit} object
#'
#' @param object An object of class \code{beeSurvFit}
#' @param X Percentage of individuals dying (e.g., \eqn{50} for the \eqn{LC_{50}})
#' @param timeLCx A scalar giving the time at which  \eqn{LC_{x}} is predicted.
#' If \code{NULL}, the latest time point of the experiment used in the calibration is used
#' @param concRange A vector of length 2 with minimal and maximal value of the
#' range of concentration. If \code{NULL}, the range is define between 0 and the
#' highest tested concentration of the calibration experiment.
#' @param nPoints Number of time point in \code{concRange} between 0 and the
#' maximal concentration. 100 by default.
#' @param ... Further arguments to be passed to generic methods
#' @param testType Test type for which the \eqn{LC_{X}} is calculated
#'  amongst "Acute_Oral", "Acute_Contact", and "Chronic_Oral". Note that for
#'  "Acute_Oral" and "Acute_Contact", the concentration will be reconstructed as
#'  in the \link[=dataGUTS]{dataGUTS} function (not recommended as this might not
#'  make sense for \eqn{LC_{X}} estimations. Default is "Chronic_Oral"
#'
#' @return A object of class \code{LCx} containing the results of the lethal concentration predictions
#' @export
#'
#' @examples
#' \donttest{
#' data(fitBetacyfluthrin_Chronic)
#' out <- LCx(fitBetacyfluthrin_Chronic)
#' }
LCx.beeSurvFit <- function(object,
                           X = 50,
                           testType = "Chronic_Oral",
                           timeLCx = NULL,
                           concRange = NULL,
                           nPoints = 100,
                           ...) {

  # library(doParallel)
  # how to do this properly?? is it enough to have imports in BeeGUTS-package.R
  # Check for correct class
  if (!is(object,"beeSurvFit")) {
    stop("LCx.beeSurvFit: an object of class 'beeSurvFit' is expected")
  }

  # Set concentration range to test
  if(is.null(concRange)){
    concRange = seq(0, max(object$dataFit$conc), length.out = nPoints)
  } else{
    if(length(concRange) != 2){
      stop("'concRange' must a vector of length 2 with minimal and maximal value of the range of concentration")
    }
    if(min(concRange) != 0){
      stop("The minimal value of 'concRange' must be 0.")
    }
    concRange = seq(concRange[1], concRange[2], length.out = nPoints)
  }

  # Set time of LCx calculation
  if(is.null(timeLCx)){
    timeLCx = max(object$dataFit$tconc)
    cat("No time of LCx calculation entered, maximum time in the calibration",
      "dataset of",timeLCx, "taken")
  }

  # calculate dose
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

  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(chk) && chk == "TRUE") {
    # use 2 cores in CRAN/Travis/AppVeyor
    ncores <- 2L
  } else {
    # use all cores in devtools::test()
    ncores = parallel::detectCores(logical = FALSE)-1L
  }
  cl <- parallel::makeCluster(mc <- getOption("cl.cores",ncores))
  doParallel::registerDoParallel(cl)

  # Perform predictions using the odeGUTS package
  #func =  function(kit){ # conc
  #  tmp <- predict_ode(morseObject, data.frame(time = c(0,timeLCx),
  #                                                      conc = concRange[kit],
  #                                                      replicate = "rep")
  #  )
  #  tmp <- tmp$df_quantile[tmp$df_quantile[,"time"] == timeLCx,]
  #  return(tmp)
  #}
  # test this compiled or go back to previous version withe the function.
  k <- 1:length(concRange)
  if(testType == "Chronic_Oral") {
    dtheo <- foreach::foreach(i=1:length(concRange),.combine="cbind") %dopar% {
      tmp = odeGUTS::predict_ode(morseObject, data.frame(time = c(0,timeLCx),
                                                conc = concRange[i],
                                                replicate = "rep"))
      val <- tmp$df_quantile[tmp$df_quantile[,"time"] == timeLCx,]
      list(val)
    }
    parallel::stopCluster(cl)
    } else if(testType == "Acute_Oral") {
    warning("Calculating LCx for 'Acute_Oral' reconstructed concentrations is
            not in line with guidelines and might not make sense. Prefer to use
            'Chronic_Oral' for the accepted way of calculating LCx")
    dtheo <- lapply(k, function(kit) { # conc
      tmpConc <- concAO(as.data.frame(concRange[kit]), expTime = timeLCx, ...)
      tmp <- odeGUTS::predict_ode(morseObject, data.frame(time = tmpConc[,1],
                                                          conc = tmpConc[,2],
                                                          replicate = rep("rep", nrow(tmpConc)))
      )
      tmp <- tmp$df_quantile[tmp$df_quantile[,"time"] == timeLCx,]
    })
  } else if(testType == "Acute_Contact") {
    warning("Calculating LCx for 'Acute_Contact' reconstructed concentrations is
            not in line with guidelines and might not make sense. Prefer to use
            'Chronic_Oral' for the accepted way of calculating LCx")
    dtheo <- lapply(k, function(kit) { # conc
      tmpConc <- concAC(as.data.frame(concRange[kit]), expTime = timeLCx, ...)
      tmp <- odeGUTS::predict_ode(morseObject, data.frame(time = tmpConc[,1],
                                                          conc = tmpConc[,2],
                                                          replicate = rep("rep", nrow(tmpConc)))
      )
      tmp <- tmp$df_quantile[tmp$df_quantile[,"time"] == timeLCx,]
    })
  } else {
    stop("You need to specifiy a correct data 'test_type' amongst 'Acute_Oral', 'Acute_Contact', or 'Chronic_Oral'.")
  }

  dtheo <- do.call(rbind.data.frame, dtheo)
  colnames(dtheo) <- c("time","concentration","replicate","q50","qinf95","qsup95")

  # Calculate LCx
  X_prop = ((100 - X)/100)
  dfLCx <- pointsLCx(dtheo, X_prop)

  out <- list(X_prop = X_prop,
              timeLCx = timeLCx,
              testType = testType,
              modelType = object$modelType,
              beeType = object$data$beeSpecies,
              dfLCx = dfLCx,
              dfDose = dtheo)
  class(out) <- c("LCx", class(out))
  return(out)
}



# points for LCx. From morse
#
pointsLCx <- function(df_dose, X_prop){

  if(min(df_dose$q50) < X_prop & X_prop < max(df_dose$q50)){
    LCX_q50 = approx( df_dose$q50, df_dose$concentration, xout = X_prop, ties = mean)$y
  } else {
    LCX_q50 = NA
    warning(paste("No median for survival probability of", X_prop,
                  " in the range of concentrations under consideration: [",
                  min(df_dose$concentration), ";", max(df_dose$concentration), "]"))
  }
  if(min(df_dose$qinf95) < X_prop & X_prop < max(df_dose$qinf95)){
    LCX_qinf95 = approx( df_dose$qinf95, df_dose$concentration, xout = X_prop, ties = mean)$y
  } else{
    LCX_qinf95 = NA
    warning(paste("No 95%inf for survival probability of", X_prop ,
                  " in the range of concentrations under consideration: [",
                  min(df_dose$concentration), ";", max(df_dose$concentration), "]"))
  }
  if(min(df_dose$qsup95) < X_prop & X_prop < max(df_dose$qsup95)){
    LCX_qsup95 = approx( df_dose$qsup95, df_dose$concentration, xout = X_prop, ties = mean)$y
  } else{
    LCX_qsup95 = NA
    warning(paste("No 95%sup for survival probability of", X_prop,
                  " in the range of concentrations under consideration: [",
                  min(df_dose$concentration), ";", max(df_dose$concentration), "]"))
  }

  df_LCx <- data.frame(quantile = c("median", "quantile 2.5%", "quantile 97.5%"),
                       LCx = as.numeric(c(LCX_q50, LCX_qinf95, LCX_qsup95)))
  # as.numeric is needed here because if all values are NA, LCx has type logical
  return(df_LCx)
}

