#' Plotting method for \code{beeSurvValid} objects
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
#' @import ggplot2 gridExtra
#' @importFrom stats quantile
#'
#' @export
#'
#' @examples
#' data(betacyfluthrinChronic) # Load dataset for validation
#' data(fitBetacyfluthrin_Chronic)
#' validation <- validate.beeSurvFit(fitBetacyfluthrin_Chronic, betacyfluthrinChronic)
#' plot.beeSurvValidation(validation)
plot.beeSurvValidation <- function(x,
                            ...,
                            xlab = "Time [d]",
                            ylab1 = "Number of survivors",
                            ylab2 = "Concentration",
                            main = paste("Validation results for a BeeGUTS", x$typeData, "calibrated for",
                                         x$beeSpecies) ) {
  # Check for correct class
  if (!is(x,"beeSurvValid")) {
    stop("plot.beeSurvValidation: an x of class 'beeSurvValid' is expected")
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

