#' Perform comparison between LLD50 at 2 days and at 10 days
#' as for EFSA revised guideline (2023) - Section 6.6
#'
#' When class of \code{object} is \code{beeSurvFit},
#' see \link[=ShortTimeEffects.beeSurvFit]{ShortTimeEffects.beeSurvFit}.
#'
#' Copyright 2024 C. Romoli, ibacon GmbH
#'
#' @rdname ShortTimeEffects
#'
#' @param object An object used to select a method
#' @param concRange Argument of LCx, range of concentrations to find LDD50
#' @param fullcalculation Compute the LDD50 from day 1 to day 10 of the Chronic test.
#' This can increase the computation time
#'
#' @return A object of class \code{ggplot} containing the graph of the comparison
#' between LDD50 at day 2 and day 10 and the data.frame with the plotted values.
#'
#' @import ggplot2
#'
#' @export
#'
ShortTimeEffects <- function(object, concRange = NULL, fullcalculation=FALSE){
  UseMethod("ShortTimeEffects")
}

#' Calculate the presence of Time Reinforced Toxicity for the compound from the
#' calibrated model \code{beeSurvFit} object.
#'
#' @param object An object of class \code{beeSurvFit}
#' @param concRange Argument of LCx, range of concentrations to find LDD50
#' @param fullcalculation Compute the LDD50 from day 1 to day 10 of the Chronic test.
#' This can increase the computation time
#'
#' @return A object of class \code{ggplot} containing the graph of the comparison
#' between LDD50 at day 2 and day 10 and the data.frame with the plotted values.
#'
#' @export
#'
#' @examples
#' \donttest{
#' data(fitBetacyfluthrin_Chronic)
#' ShortTimeEffects(fitBetacyfluthrin_Chronic)
#' }
ShortTimeEffects.beeSurvFit <- function(object, concRange = NULL, fullcalculation=FALSE){
  if (!is(object,"beeSurvFit")) {
    stop("predict.beeSurvFit: an object of class 'beeSurvFit' is expected")
  }

  # get the maximum concentration and a reasonable number of points
  if (length(concRange)<1){
  maxcon=max(object$dataFit$conc)
  nPoints = 100} else {
    maxcon = max(concRange)
    nPoints = max(100,floor(100*max(object$dataFit$conc)/max(concRange)))
  }

  # compute LDD50 at 2 days assuming constant concentration
  LDD50_2 <- LCx(object, X = 50, testType = "Chronic_Oral", timeLCx = 2,
                  concRange = c(0,maxcon), nPoints = nPoints)
  if (is.na(LDD50_2$dfLCx$LCx[3])){
    cat(paste0("95% upperlimit on LDD50 value at day 2 is outside the given range [", 0,"-",maxcon ,"].
New calculation done with range increased by a factor 5.
If calculation still fails, try providing a larger range using the concRange argument.\n\n"))
    LDD50_2 <- LCx(object, X = 50, testType = "Chronic_Oral", timeLCx = 2,
                   concRange = c(0,maxcon*5), nPoints = 5*nPoints)
  # use updated values
  maxcon = maxcon*5
  nPoints = nPoints*5
  }

  # compute LDD50 at 10 days assuming constant concentration
  LDD50_10 <- LCx(object, X = 50, testType = "Chronic_Oral", timeLCx = 10,
                  concRange = c(0,maxcon), nPoints = nPoints)
  LDD50=list(LDD50_2$dfLCx,LDD50_10$dfLCx)

  if (anyNA(c(LDD50_2$dfLCx$LCx[2],LDD50_10$dfLCx$LCx[3],LDD50_2$dfLCx$LCx[1],LDD50_10$dfLCx$LCx[1]))){
    msg = paste(paste0("With the given concentration range [", 0,"-",maxcon ,"],"),
    "it was not possible to calculate all the LDD50 values.",
    "Try again with a wider range using the concRange argument.", sep = '\n')
    warning(msg)
  }

  # Check for fast expression of effects (EFSA, 2023 - Ch. 6.6)
  NoShortTox <- (LDD50_2$dfLCx$LCx[2]>LDD50_10$dfLCx$LCx[3]) ||
    (LDD50_2$dfLCx$LCx[1] > 3.*(LDD50_10$dfLCx$LCx[1] ))

  if (NoShortTox){
    msgeff = "No fast expression of effects"
    cat("No fast expression of effects\n")
  } else {
    msgeff = "Fast expression of effects is present"
    cat("Fast expression of effects is present\n")
  }


  temp=unlist(LDD50)
  LCx1 = as.numeric(temp[grepl("LCx1", names(temp))])
  LCx2 = as.numeric(temp[grepl("LCx2", names(temp))])
  LCx3 = as.numeric(temp[grepl("LCx3", names(temp))])

  # Fill dataframe with the results
  dfplot <- data.frame(time=c(2,10), ldd50 = LCx1,
                       ldd50_q2p5 = LCx2,
                       ldd50_q97p5 = LCx3)

  ste_plot <- ggplot(dfplot) +
    geom_errorbar(aes(x=time, y=ldd50, ymin=ldd50_q2p5, ymax=ldd50_q97p5),
                  width=0.05, linewidth=1) +
    geom_point(aes(x=time, y=ldd50),size=3) +
    geom_line(aes(x=c(1,10), y=LCx2[1]), color='red', linetype=2)+
    geom_line(aes(x=c(1,10), y=LCx3[2]), color='red', linetype=2)+
    xlab("Time (d)") +
    ylab("LDD_50 (conc./bee/day)") + ylim(c(0, max(LCx3)))

  if (fullcalculation){
    LDD50=list(1:10)
    for (i in c(1,3,4,5,6,7,8,9)){
      LDD50_val <- LCx(object, X = 50, testType = "Chronic_Oral", timeLCx = i,
                       concRange = c(0,maxcon), nPoints = nPoints)$dfLCx
      if(is.na(LDD50_val$LCx[3])){
        # largely increase the range to be able to calculate LDD50 at early times
        # in case the predefined ranges failed
        LDD50_val <- LCx(object, X = 50, testType = "Chronic_Oral", timeLCx = i,
                         concRange = c(0,5*maxcon), nPoints = 5*nPoints)$dfLCx
      }
      LDD50[[i]] = LDD50_val
    }
    LDD50[[2]] = LDD50_2$dfLCx
    LDD50[[10]] = LDD50_10$dfLCx
    temp=unlist(LDD50)
    LCx1 = as.numeric(temp[grepl("LCx1", names(temp))])
    LCx2 = as.numeric(temp[grepl("LCx2", names(temp))])
    LCx3 = as.numeric(temp[grepl("LCx3", names(temp))])

    dfplot <- data.frame(time=c(1:10), ldd50 = LCx1,
                         ldd50_q2p5 = LCx2,
                         ldd50_q97p5 = LCx3)
    ste_plot <- ggplot(dfplot) +
      geom_errorbar(aes(x=time, y=ldd50, ymin=ldd50_q2p5, ymax=ldd50_q97p5),
                    width=0.05, linewidth=1) +
      geom_point(aes(x=time, y=ldd50),size=3) +
      geom_line(aes(x=c(1:10), y=LCx2[2]), color='red', linetype=2)+
      geom_line(aes(x=c(1:10), y=LCx3[10]), color='red', linetype=2)+
      xlab("Time (d)") +
      ylab("LDD_50 (conc./bee/day)") + ylim(c(0, max(LCx3)))
  }

  #return plot and dataframe for further use
  return(list(ste_plot, dfplot, paste(msgeff, msg, sep='\n')))
}

