#' Plotting method for \code{beeSurvData} objects
#'
#' @description This is the generic \code{plot} S3 method for the \code{beeSurvData}
#' class. It plots the number of survivors as a function of time as well as the reconstructed
#' concentrations for \code{"Acute_Oral"} and \code{"Acute_Contact"} test types.
#'
#' @param x An object of class \code{beeSurvData}
#' @param xlab A character string for the label of the x-axis
#' @param ylab1 A character string for the label of the y-axis of the survivor plots
#' @param ylab2 A character string for the label of the y-axis of the concentration plots
#' @param main A character string for the title label plot
#' @param ... Additional parameters to generic plot function (not used)
#'
#' @return
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
#' data(betacyfluthrinChronic)
#' plot(betacyfluthrinChronic)
plot.beeSurvData <- function(x,
                             ...,
                             xlab = "Time [d]",
                             ylab1 = "Number of survivors",
                             ylab2 = "Concentration",
                             main = paste("Data from a", x$typeData, "test on",
                                          x$beeSpecies)) {
  # Check for correct class
    if (!is(x,"beeSurvData")) {
    stop("plot.beeSurvData: an object of class 'beeSurvData' is expected")
  }

  # Extract data
  dfDataSurv_long <- as.data.frame(x$survData_long)
  dfDataConc_long <- as.data.frame(x$concData_long)
  dfModelConc_long <- as.data.frame(x$concModel_long)

  ggSurv <- ggplot(data = dfDataSurv_long, aes(x=SurvivalTime, y = NSurv)) +
    geom_point() +
    xlab(xlab) +
    ylab(ylab1) +
    facet_grid(~Treatment) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  ggConc <- ggplot(data = dfModelConc_long, aes(x=SurvivalTime, y = Conc)) +
    geom_line() +
    geom_point(data = dfDataConc_long) +
    xlab(xlab) +
    ylab(paste0(ylab2,"\n", x$unitData)) +
    ggtitle(main) +
    facet_grid(~Treatment) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank())

  ggOut <- cowplot::plot_grid(ggConc, ggSurv, align = "v", nrow = 2)
  return(ggOut)
}


#' Plotting method for \code{beeSurvFit} objects
#'
#' @description This is the generic \code{plot} S3 method for the \code{beeSurvFit}
#' class. It plots the number of survivors as a function of time as well as the reconstructed
#' concentrations for \code{"Acute_Oral"} and \code{"Acute_Contact"} test types.
#'
#' @param x An object of class \code{beeSurvFit}
#' @param xlab A character string for the label of the x-axis
#' @param ylab1 A character string for the label of the y-axis of the survivor plots
#' @param ylab2 A character string for the label of the y-axis of the concentration plots
#' @param main A character string for the title label plot
#' @param ... Additional parameters to generic plot functions (not used)
#'
#' @return
#'
#' @import ggplot2
#' @importFrom stats quantile
#'
#' @export
#'
#' @examples
#' data(fitBetacyfluthrin_Chronic)
#' plot(fitBetacyfluthrin_Chronic)
plot.beeSurvFit <- function(x,
                            ...,
                            xlab = "Time [d]",
                            ylab1 = "Number of survivors",
                            ylab2 = "Concentration",
                            main = paste("Calibration results for a", x$data$typeData, "test on",
                                         x$data$beeSpecies) ) {
  # Check for correct class
  if (!is(x,"beeSurvFit")) {
    stop("plot.beeSurvFit: an object of class 'beeSurvFit' is expected")
  }

  # Extract data
  dfDataSurv_long <- as.data.frame(x$data$survData_long)
  #dfModelSurv <- as.data.frame(x$survModel)
  dfDataConc_long <- as.data.frame(x$data$concData_long)
  dfModelConc_long <- as.data.frame(x$data$concModel_long)
  lsNsurv_sim <- rstan::extract(x$stanFit, pars = 'Nsurv_sim')

  # Compute quantiles of simulations and compile all in a dataframe
  dfDataSurv_long$simQ50 <- apply(lsNsurv_sim[[1]], 2, quantile, 0.5)
  dfDataSurv_long$simQinf95 <- apply(lsNsurv_sim[[1]], 2, quantile, 0.025)
  dfDataSurv_long$simQsup95 <- apply(lsNsurv_sim[[1]], 2, quantile, 0.975)
  yLimits <- c(0, max(dfDataSurv_long$NSurv, dfDataSurv_long$simQsup95))

  ggSurv <- ggplot(data = dfDataSurv_long, aes(x=SurvivalTime, y = NSurv)) +
    geom_point() +
    # geom_pointrange( aes(x = SurvivalTime, y = q50, ymin = qinf95, ymax = qsup95, group = Treatment), color = "blue", size = 0.2) +
    geom_line( aes(x = SurvivalTime, y = simQ50,  group = Treatment), color = "blue") +
    geom_ribbon( aes(x= SurvivalTime, ymin = simQinf95, ymax = simQsup95, group = Treatment), fill = "blue", alpha = 0.2)+
    scale_y_continuous(limits = yLimits) +
    xlab(xlab) +
    ylab(ylab1) +
    facet_grid(~Treatment) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  ggConc <- ggplot(data = dfModelConc_long, aes(x=SurvivalTime, y = Conc)) +
    geom_line() +
    geom_point(data = dfDataConc_long) +
    xlab(xlab) +
    ylab(paste0(ylab2,"\n", x$data$unitData)) +
    ggtitle(main) +
    facet_grid(~Treatment) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank())

  ggOut <- cowplot::plot_grid(ggConc, ggSurv, align = "v", nrow = 2)
  return(ggOut)
}



#' Plotting method for \code{beeSurvValidation} objects
#'
#' @description This is the generic \code{plot} S3 method for the \code{beeSurvValid}
#' class. It plots the number of survivors as a function of time as well as the reconstructed
#' concentrations for \code{"Acute_Oral"} and \code{"Acute_Contact"} test types.
#'
#' @param x An x of class \code{beeSurvValid}
#' @param xlab A character string for the label of the x-axis
#' @param ylab1 A character string for the label of the y-axis of the survivor plots
#' @param ylab2 A character string for the label of the y-axis of the concentration plots
#' @param main A character string for the title label plot
#' @param ... Additional parameters to generic plot functions (not used)
#'
#' @return
#'
#' @import ggplot2
#' @importFrom stats quantile
#'
#' @export
#'
#' @examples
#' data(betacyfluthrinChronic) # Load dataset for validation
#' data(fitBetacyfluthrin_Chronic)
#' validation <- validate(fitBetacyfluthrin_Chronic, betacyfluthrinChronic)
#' plot(validation)
plot.beeSurvValidation <- function(x,
                                   ...,
                                   xlab = "Time [d]",
                                   ylab1 = "Number of survivors",
                                   ylab2 = "Concentration",
                                   main = paste("Validation results for a BeeGUTS", x$typeData, "calibrated for",
                                                x$beeSpecies) ) {
  # Check for correct class
  if (!is(x,"beeSurvValidation")) {
    stop("plot.beeSurvValidation: an object of class 'beeSurvValidation' is expected")
  }

  yLimits <- c(0, max(x$sim$NSurv, x$sim$Nsurv_qsup95_check))


  EFSA_criteria <- x$EFSA$Percent_PPC
  EFSA_criteria$PPC_global <- ""
  EFSA_criteria$PPC_global[1] <- x$EFSA$Percent_PPC_global
  EFSA_criteria$NRMSE <- x$EFSA$Percent_NRMSE$NRMSE
  EFSA_criteria$NRMSE_global <- ""
  EFSA_criteria$NRMSE_global[1] <- x$EFSA$Percent_NRMSE_global
  EFSA_criteria$SPPE <-  x$EFSA$Percent_SPPE$SPPE
  ###############################################

  ggSurv <- ggplot(data = x$sim, aes(x = time, y = Nsurv_q50_valid,  group = replicate)) +
    geom_line(color = "blue") +
    geom_point(data = x$sim, aes(x=time, y=Nsurv,  group = replicate)) +
    geom_ribbon( aes(x= time, ymin = Nsurv_qinf95_valid, ymax = Nsurv_qsup95_valid, group = replicate), fill = "blue", alpha = 0.2)+
    scale_y_continuous(limits = yLimits) +
    xlab(xlab) +
    ylab(ylab1) +
    facet_grid(~replicate) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  ggConc <- ggplot(data = x$data, aes(x=SurvivalTime, y = Conc)) +
    geom_line() +
    geom_point() +
    xlab(xlab) +
    ylab(paste0(ylab2,"\n", x$data$unitData)) +
    ggtitle(main) +
    facet_grid(~Treatment) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank())

  table <- gridExtra::tableGrob(EFSA_criteria, rows=NULL)

  ggOut1 <- cowplot::plot_grid(ggConc, ggSurv, align = "v",nrow = 2)
  ggOut <- cowplot::plot_grid(ggOut1, table, nrow = 2)



  #############################################################################
  return(ggOut)
}





#' Plotting method for \code{beeSurvPred} objects
#'
#' @description This is the generic \code{plot} S3 method for the \code{beeSurvPred}
#' class. It plots the predicted number of survivors for the exposure concentration entered by the user.
#'
#' @param x An object of class \code{beeSurvPred}
#' @param xlab A character string for the label of the x-axis
#' @param ylab1 A character string for the label of the y-axis of the survivor plots
#' @param ylab2 A character string for the label of the y-axis of the concentration plots
#' @param main A character string for the title label plot
#' @param ... Additional parameters to generic plot functions (not used)
#'
#' @return
#'
#' @import ggplot2
#' @importFrom stats quantile
#'
#' @export
#'
#' @examples
#' dataPredict <- data.frame(time = c(1:10, 1:10, 1:10), conc = c(rep(5, 10), rep(10, 10), rep(15, 10)), replicate = c(rep("rep1", 10), rep("rep2", 10), rep("rep3", 10)), NSurv = c(rep(5, 10), rep(10, 10), rep(15, 10)))
#' data(fitBetacyfluthrin_Chronic)
#' prediction <- predict(fitBetacyfluthrin_Chronic, dataPredict)
#' plot(prediction)
plot.beeSurvPred <- function(x,
                            ...,
                            xlab = "Time [d]",
                            ylab1 = "Survival probability",
                            ylab2 = "Concentration",
                            main = paste("Predictions results for a BeeGUTS", x$modelType,"calibrated for",
                                         x$beeSpecies) ) {
  # Check for correct class
  if (!is(x,"beeSurvPred")) {
    stop("plot.beeSurvPred: an object of class 'beeSurvPred' is expected")
  }

  ggSurv <- ggplot(data = x$sim, aes(x = time, y = q50,  group = replicate)) +
    geom_line(color = "blue") +
    geom_ribbon( aes(x= time, ymin = qinf95, ymax = qsup95, group = replicate), fill = "blue", alpha = 0.2)+
    scale_y_continuous(limits = c(0,1)) +
    xlab(xlab) +
    ylab(ylab1) +
    facet_grid(~replicate) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  ggConc <- ggplot(data = x$sim, aes(x = time, y = conc)) +
    geom_line() +
    xlab(xlab) +
    ylab(paste0(ylab2,"\n", x$unitData)) +
    ggtitle(main) +
    facet_grid(~replicate) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank())

  ggOut <- cowplot::plot_grid(ggConc, ggSurv, align = "v", nrow = 2)
  return(ggOut)
}



#' Plotting method for traces and densities for \code{beeSurvFit} objects
#'
#' @description This is the generic \code{traceplot} S3 method for the \code{beeSurvFit}
#' class. It plots the traces with as well as the densities for the parameters of
#' the GUTS IT or GUTS SD. The traceplot includes by default the warmup iterations,
#' the density plot does not include them
#'
#' @param object An object of class \code{beeSurvFit} to be plotted
#' @param ... Additional parameters to be parsed to generic \code{rstan} plot functions
#' @param incWarmup_trace A logical indicating whether the warmup iterations should be plotted
#' in the traceplot (default TRUE)
#' @param incWarmup_dens A logical indicating whether the warmup iterations should be plotted
#' in the density plot (default FALSE)
#'
#' @return
#' @export
#'
#' @examples
#' data(fitBetacyfluthrin_Chronic)
#' traceplot(fitBetacyfluthrin_Chronic)
traceplot <- function(object, ..., incWarmup_trace = TRUE, incWarmup_dens = FALSE){
  UseMethod("traceplot")
}



#' @rdname traceplot
#' @export
traceplot.beeSurvFit <- function(object, ..., incWarmup_trace = TRUE, incWarmup_dens = FALSE) {
  if (object$modelType == "SD") {
    ggTrace <- rstan::stan_trace(object$stanFit,
                                 pars = c("hb_log10", "kd_log10", "zw_log10", "bw_log10"),
                                 inc_warmup = incWarmup_trace,
                                 nrow = 4,
                                 ...) +
      ggplot2::ggtitle("Traces")
      ggplot2::theme(legend.position = "none")
    ggDens <- rstan::stan_dens(object$stanFit,
                               pars = c("hb_log10", "kd_log10", "zw_log10", "bw_log10"),
                               inc_warmup = incWarmup_dens,
                               nrow = 4,
                               separate_chains = TRUE,
                               ...) +
      ggplot2::ggtitle("Densities")
    ggOut <- cowplot::plot_grid(ggTrace, ggDens, ncol = 2)
  }
  if (object$modelType == "IT") {
    ggTrace <- rstan::stan_trace(object$stanFit,
                                 pars = c("hb_log10", "kd_log10", "mw_log10", "beta_log10"),
                                 inc_warmup = incWarmup_trace,
                                 nrow = 4,
                                 ...) +
      ggplot2::ggtitle("Traces")
    ggplot2::theme(legend.position = "none")
    ggDens <- rstan::stan_dens(object$stanFit,
                               pars = c("hb_log10", "kd_log10", "mw_log10", "beta_log10"),
                               inc_warmup = incWarmup_dens,
                               nrow = 4,
                               separate_chains = TRUE,
                               ...) +
      ggplot2::ggtitle("Densities")
    ggOut <- cowplot::plot_grid(ggTrace, ggDens, ncol = 2)
  }
  return(ggOut)
}

