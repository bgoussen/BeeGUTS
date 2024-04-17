#' Perform the extrapolation of the LDD50 to 27 days and check the presence of
#' reinforced toxicity as for EFSA revised guideline (2023) - Annex G
#'
#' When class of \code{object} is \code{beeSurvFit}, see \link[=TRT.beeSurvFit]{TRT.beeSurvFit}.
#'
#' Copyright 2023-2024 C. Romoli, ibacon GmbH
#'
#' @rdname TRT
#'
#' @param object An object used to select a method
#'
#' @return A \code{ggplot} object with graph of the LDD extrapolation compared
#' to the Haber's law and a data.frame with the calculations
#'
#' @import ggplot2
#'
#' @export
#'
TRT <- function(object){
  UseMethod("TRT")
}

#' Calculate the presence of Time Reinforced Toxicity for the compound from the
#' calibrated model \code{beeSurvFit} object.
#'
#' @param object An object of class \code{beeSurvFit}
#'
#' @return A object of class \code{ggplot} containing the graph of the comparison
#' between Haber's law and the predicted lethal doses at 10 and 27 days and a
#' data.frame with the plotted values.
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(fitBetacyfluthrin_Chronic)
#' TRT(fitBetacyfluthrin_Chronic)
#' }
TRT.beeSurvFit <- function(object){
  if (!is(object,"beeSurvFit")) {
    stop("predict.beeSurvFit: an object of class 'beeSurvFit' is expected")
  }
  # compute LDD50 at 10 days assuming constant concentration
  LDD50_10 <- LCx(object, X = 50, testType = "Chronic_Oral", timeLCx = 10,
                  concRange = NULL, nPoints = 100)
  # compute LDD50 at 27 days assuming constant concentration
  LDD50_27 <- LCx(object, X = 50, testType = "Chronic_Oral", timeLCx = 27,
                  concRange = NULL, nPoints = 100)

  # Check for Time Reinforced Toxicity (EFSA, 2023 - Annex G)
  ReinfTox <- LDD50_27$dfLCx$LCx[1] >= (LDD50_10$dfLCx$LCx[1] / 2.7)

  # output on screen
  if (ReinfTox){
    cat("No Time Reinforced Toxicity\n")
  } else {
    cat("Time Reinforced Toxicity cannot be excluded\n")
  }

  # Fill dataframe with the results
  dfplot <- data.frame(time=c(10,27), ldd50 = c(LDD50_10$dfLCx$LCx[1], LDD50_27$dfLCx$LCx[1]),
                       lddhab=c(LDD50_10$dfLCx$LCx[1], LDD50_10$dfLCx$LCx[1]/2.7),
                       ldd50_q2p5 = c(LDD50_10$dfLCx$LCx[2], LDD50_27$dfLCx$LCx[2]),
                       ldd50_q97p5 = c(LDD50_10$dfLCx$LCx[3], LDD50_27$dfLCx$LCx[3]),
                       trt = c(NA,as.logical(!ReinfTox)))

  # show graphical results comparing Haber's law and model predictions
  trt_plot <- ggplot(dfplot) +
    geom_errorbar(aes(x=time, y=ldd50, ymin=ldd50_q2p5, ymax=ldd50_q97p5),
                  width=0.05, linewidth=1) +
    geom_point(aes(x=time, y=ldd50, color="LDD_50"),size=3) +
    geom_line(aes(x=time, y=lddhab, color="Haber's law"),
              linetype="dashed", linewidth=1) +
    scale_x_log10() +scale_y_log10() + xlab("Time (d)") +
      ylab("LDD_50 (conc./bee/day)")+
    guides(color=guide_legend("Legend",
                              override.aes=list(shape = c(NA,19),
                                                linetype=c("dashed","solid"))))+
    scale_color_manual(values = c("red", "black"))

  #return plot and dataframe for further use
  return(list(trt_plot, dfplot))
}

