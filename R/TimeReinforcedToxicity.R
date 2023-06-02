#' Perform the extrapolation of the LDD50 to 27 days and check the presence of
#' reinforced toxicity as for EFSA revised guideline (2023)
#'
#' When class of \code{object} is \code{beeSurvFit}, see \link[=TRT.beeSurvFit]{TRT.beeSurvFit}.
#'
#' @rdname TRT
#'
#' @param object An object used to select a method
#' @param \dots Further arguments to be passed to generic methods
#'
#' @return Result of the TRT check and a graph of the LDD extrapolation compared to the Haber's law
#' @export
#'
TRT <- function(object, ...){
  UseMethod("TRT")
}


TRT.beeSurvFit <- function(object){
  if (!is(object,"beeSurvFit")) {
    stop("predict.beeSurvFit: an object of class 'beeSurvFit' is expected")
  }
  LDD50_10 <- LCx(object, X = 50, testType = "Chronic_Oral", timeLCx = 10,
                  concRange = NULL, nPoints = 100)
  LDD50_27 <- LCx(object, X = 50, testType = "Chronic_Oral", timeLCx = 27,
                  concRange = NULL, nPoints = 100)

  ReinfTox <- LDD50_27$dfLCx$LCx[1] >= (LDD50_10$dfLCx$LCx[1] / 2.7)

  if (ReinfTox){
    print("No Time Reinforced Toxicity")
  } else {
    print("Time Reinforced Toxicity cannot be excluded")
  }

  dfplot <- data.frame(time=c(10,27), ldd50 = c(LDD50_10$dfLCx$LCx[1], LDD50_27$dfLCx$LCx[1]),
                       lddhab=c(LDD50_10$dfLCx$LCx[1], LDD50_10$dfLCx$LCx[1]/2.7),
                       ldd50_q2p5 = c(LDD50_10$dfLCx$LCx[2], LDD50_27$dfLCx$LCx[2]),
                       ldd50_q97p5 = c(LDD50_10$dfLCx$LCx[3], LDD50_27$dfLCx$LCx[3]),
                       trt = c(1,ReinfTox))

  ggplot(dfplot) +
    geom_errorbar(aes(x=time, y=ldd50, ymin=ldd50_q2p5, ymax=ldd50_q97p5), width=0.05, size=1) +
    geom_point(aes(x=time, y=ldd50), size=3) +
    geom_line(aes(x=time, y=lddhab), linetype="solid", color='red', ) +
    scale_x_log10() +scale_y_log10() + xlab("Time (d)") + ylab("LDD_50 (conc./bee/day)")
}

