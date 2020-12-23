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
#'
#' @return
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
plot.beeSurvData <- function(x,
                             xlab = "Time [d]",
                             ylab1 = "Number of survivors",
                             ylab2 = "Concentration",
                             main = x$typeData) {
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
#'
#' @return
#'
#' @import ggplot2
#'
#' @export
#'
#' @examples
plot.beeSurvFit <- function(x,
                            xlab = "Time [d]",
                            ylab1 = "Number of survivors",
                            ylab2 = "Concentration",
                            main = x$typeData) {
  # Check for correct class
  if (!is(x,"beeSurvFit")) {
    stop("plot.beeSurvFit: an object of class 'beeSurvFit' is expected")
  }

  # Extract data
  dfDataSurv <- as.data.frame(x$survData)
  #dfModelSurv <- as.data.frame(x$survModel)
  dfDataConc <- as.data.frame(x$concData)
  dfModelConc <- as.data.frame(x$concModel)

  # Transform into long data
  dfDataSurv_long <- tidyr::gather(dfDataSurv, Treatment, NSurv, -SurvivalTime)
  dfDataConc_long <- tidyr::gather(dfDataConc, Treatment, Conc, -SurvivalTime)
  dfModelConc_long <- tidyr::gather(dfModelConc, Treatment, Conc, -SurvivalTime)

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
    facet_grid(~Treatment) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank())

  ggOut <- cowplot::plot_grid(ggConc, ggSurv, align = "v", nrow = 2)
  return(ggOut)
}
