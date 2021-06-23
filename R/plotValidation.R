#' Plotting method for \code{beeSurvValidation} objects
#'
#' @description This is the generic \code{plot} S3 method for the \code{beeSurvValidation}
#' class. It plots the number of survivors as a function of time as well as the reconstructed
#' concentrations for \code{"Acute_Oral"} and \code{"Acute_Contact"} test types.
#'
#' @param x An object of class \code{beeSurvValidation}
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
#' dataValidate <- betacyfluthrinChronic
#' data(fitBetacyfluthrin_Chronic)
#' validate <- validate(fitBetacyfluthrin_Chronic, dataValidate)
#' object <- validate
#'  plot(validate, dataValidate)
plot.beeSurvValidation <- function(object, dataValidate,
                            ...,
                            xlab = "Time [d]",
                            ylab1 = "Number of survivors",
                            ylab2 = "Concentration",
                            main = paste("Validation results for a BeeGUTS", dataValidate$typeData, "calibrated for",
                                         object$beeSpecies) ) {
  # Check for correct class
  if (!is(object,"beeSurvValid")) {
    stop("plot.beeSurvValidation: an object of class 'beeSurvValid' is expected")
  }
  if (!is(dataValidate,"beeSurvData")) {
    stop("plot.beeSurvData: an object of class 'beeSurvData' is expected")
  }

### Correct the extraction from the respective parts of the object
  # Extract data
  ### from the experimental data
  dfDataSurv_long <- as.data.frame(dataValidate$survData_long)
  dfDataConc_long <- as.data.frame(dataValidate$concData_long)

  ### simulated data from the beeSurvValid object
  dfModelConc_long <- as.data.frame(object$sim$conc) ## extract simulated data from the object
  ### !!!Transfer from Probability of survival to Number of survivors
  dfDataSurv_long$simQ50 <- object$sim$q50
  dfDataSurv_long$simQinf95 <- object$sim$qinf95
  dfDataSurv_long$simQsup95 <- object$sim$qsup95
  yLimits <- c(0, max(dfDataSurv_long$NSurv, dfDataSurv_long$simQsup95))

  # https://github.com/cran/morse/blob/master/R/JAGS_models.R
  for (i in 1:object$sim){
    Nsurv[i] ~ dbin(psurv[i], Nprec[i]) # psurv <- q50, Nprec??
  }


  ### Compare with the prediction plot (but keep in mind the lower y-axis is Number of survivors

'  ggSurv <- ggplot(data = dfDataSurv_long, aes(x=SurvivalTime, y = NSurv)) +
    geom_point() +
    geom_line( aes(x = SurvivalTime, y = simQ50,  group = Treatment), color = "blue") +
    geom_ribbon( aes(x= SurvivalTime, ymin = simQinf95, ymax = simQsup95, group = Treatment), fill = "blue", alpha = 0.2)+
    scale_y_continuous(limits = yLimits) +
   # xlab(xlab) +
   # ylab(ylab1) +
    facet_grid(~Treatment) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )'


  ggSurv <- ggplot(data = object$sim, aes(x = time, y = q50,  group = replicate)) + ## should be transfer in NSurv, group = Treatment
    geom_line(color = "blue") +
    geom_point(data = dfDataSurv_long, aes(x=SurvivalTime, y = NSurv)) +       # data from the experiment
    geom_ribbon( aes(x= time, ymin = qinf95, ymax = qsup95, group = replicate), fill = "blue", alpha = 0.2)+  ## should be transfer in NSurv
    #scale_y_continuous(limits = c(0,1)) +
   # xlab(xlab) +
   # ylab(ylab1) +
    facet_grid(~replicate) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  ggConc <- ggplot(data = dfDataConc_long, aes(x=SurvivalTime, y = Conc)) +
    geom_line() +
    geom_point(data = dfDataConc_long) +
    #xlab(xlab) +
   # ylab(paste0(ylab2,"\n", x$data$unitData)) +
    ggtitle(main) +
    facet_grid(~Treatment) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank())

  ggOut <- cowplot::plot_grid(ggConc, ggSurv, align = "v", nrow = 2)

  ### add calculations of PPC, NMRSE, SPPE


  return(ggOut)
}
