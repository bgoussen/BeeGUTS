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
#' validation <- validate.beeSurvFit(fitBetacyfluthrin_Chronic, dataValidate)
#' object <- validation
#'  plot.beeSurvValidation(validation, dataValidate)
plot.beeSurvValidation <- function(object, dataValidate,
                            ...,
                            xlab = "Time [d]",
                            ylab1 = "Number of survivors",
                            ylab2 = "Concentration",
                            main = paste("Validation results for a BeeGUTS", dataValidate$typeData, "calibrated for",
                                         dataValidate$beeSpecies) ) {
  # Check for correct class
  if (!is(object,"beeSurvValid")) {
    stop("plot.beeSurvValidation: an object of class 'beeSurvValid' is expected")
  }
  if (!is(dataValidate,"beeSurvData")) {
    stop("plot.beeSurvData: an object of class 'beeSurvData' is expected")
  }

  # Extract data
   dfDataSurv_long <- as.data.frame(dataValidate$survData_long)
   dfDataSurv_long$replicate <- dfDataSurv_long$Treatment
   dfDataConc_long <- as.data.frame(dataValidate$concData_long)
  yLimits <- c(0, max(object$sim$NSurv, object$sim$Nsurv_qsup95_check))


  ggSurv <- ggplot(data = object$sim, aes(x = time, y = Nsurv_q50_valid,  group = replicate)) + ## should be transfer in NSurv, group = Treatment
    geom_line(color = "blue") +
   # geom_point(data = object$sim, aes(x=time, y=Nsurv)) +
    geom_point(data = dfDataSurv_long, aes(x=SurvivalTime, y=NSurv,  group = replicate)) +       # data from the experiment
    geom_ribbon( aes(x= time, ymin = Nsurv_qinf95_valid, ymax = Nsurv_qsup95_valid, group = replicate), fill = "blue", alpha = 0.2)+  ## should be transfer in NSurv
    scale_y_continuous(limits = yLimits) +
    xlab(xlab) +
    ylab(ylab1) +
    facet_grid(~replicate) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  ggConc <- ggplot(data = dfDataConc_long, aes(x=SurvivalTime, y = Conc)) +
    geom_line() +
    geom_point() +
    xlab(xlab) +
    ylab(paste0(ylab2,"\n", object$data$unitData)) +
    ggtitle(main) +
    facet_grid(~Treatment) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank())

  ggOut <- cowplot::plot_grid(ggConc, ggSurv, align = "v", nrow = 2)

  ### add calculations of PPC, NMRSE, SPPE


  return(ggOut)
}

