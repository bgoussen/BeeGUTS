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
#' @return A graphic with the input data
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
  plotlist <- list()  # list of plots to be returned
  for (i in 1:x$nDatasets){
      main = paste("Data from a", x$typeData[i], "test on",
                 x$beeSpecies)
      # Extract data
      dfDataSurv_long <- as.data.frame(x$survData_long[[i]])
      dfDataConc_long <- as.data.frame(x$concData_long[[i]])
      dfModelConc_long <- as.data.frame(x$concModel_long[[i]])

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
        ylab(paste0(ylab2,"\n", x$unitData[[i]])) +
        ggtitle(main) +
        facet_grid(~Treatment) +
        theme(axis.title.x=element_blank(),
             axis.text.x=element_blank())

      ggOut <- cowplot::plot_grid(ggConc, ggSurv, align = "v", nrow = 2)
      plotlist <- append(plotlist, list(ggOut))
  }
  # return(ggOut)

  # Change the layout of the plots in order to use plotly so as to
  # have interactivity (to in the end switch datasets)
  # this is a test using plotly but does not work very well
  # x <- list(title = "Time [d]")
  # y1 <- list(title = "Nsurv")
  # y2 <- list(title = paste0(ylab2,"\n", x$unitData))
  # ggSurvy <- ggplotly(ggSurv + ylab(" ") + xlab(" "))
  # ggConcy <- ggplotly(ggConc + ylab(" ") + xlab(" "))
  # ggConcy %>% layout(xaxis = x, yaxis = y2)#, margin = list(l = 75, b =50))
  # ggSurvy %>% layout(xaxis = x, yaxis = y1)#, margin = list(l = 75, b =50))
  # #return(ggConcy)
  # subplots <- subplot(ggConcy, ggSurvy, nrows = 2, margin=0.03,heights = c(0.4,0.4), titleX = TRUE, titleY = TRUE)
  # #layout(subplots, xaxis=list(title=x))
  # #, titleY = TRUE,nrows = 2, margin=0.03, )%>%
  #    #layout(xaxis = list(title = x, anchor="y2"), yaxis = c(yy2,y1))
  # return(subplots)
  return(plotlist)
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
#' @return A graphic with the results of the fit
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

  dfDataSurv_long_full <- dplyr::bind_rows(x$data$survData_long)
  # extract the indices of the various datasets from the global table
  dfDataSurv_index = dfDataSurv_long_full %>%
    dplyr::mutate(idAll = dplyr::row_number() ) %>%
    dplyr::group_by(Dataset) %>%
    dplyr::summarise(idS_lw = min(idAll),
                     idS_up = max(idAll))
  #dfModelSurv <- as.data.frame(x$survModel)
  lsNsurv_sim <- rstan::extract(x$stanFit, pars = 'Nsurv_sim')

  plotlist <- list()  # lists to add the plots for each dataset

  for (i in 1:x$data$nDatasets) {
    # Extract data
    dfDataSurv_long <- as.data.frame(x$data$survData_long[[i]])
    dfDataConc_long <- as.data.frame(x$data$concData_long[[i]])
    dfModelConc_long <- as.data.frame(x$data$concModel_long[[i]])
    #Extract the right simulated values for each dataset
    lsNsurv_sim_dataset <- lsNsurv_sim[[1]][,dfDataSurv_index$idS_lw[i]:dfDataSurv_index$idS_up[i]]
    # Compute quantiles of simulations and compile all in a dataframe
    dfDataSurv_long$simQ50 <- apply(lsNsurv_sim_dataset, 2, quantile, 0.5)
    dfDataSurv_long$simQinf95 <- apply(lsNsurv_sim_dataset, 2, quantile, 0.025)
    dfDataSurv_long$simQsup95 <- apply(lsNsurv_sim_dataset, 2, quantile, 0.975)
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
      ylab(paste0(ylab2,"\n", x$data$unitData[[i]])) +
      ggtitle(main[i]) +
      facet_grid(~Treatment) +
      theme(axis.title.x=element_blank(),
            axis.text.x=element_blank())

    ggOut <- cowplot::plot_grid(ggConc, ggSurv, align = "v", nrow = 2)
    plotlist <- append(plotlist, list(ggOut))
  }
  return(plotlist)
}



#' Plotting method for \code{beeSurvValidation} objects
#'
#' @description This is the generic \code{plot} S3 method for the \code{beeSurvValid}
#' class. It plots the number of survivors as a function of time as well as the reconstructed
#' concentrations for \code{"Acute_Oral"} and \code{"Acute_Contact"} test types.
#'
#' @param x An object of class \code{beeSurvValid}
#' @param xlab A character string for the label of the x-axis
#' @param ylab1 A character string for the label of the y-axis of the survivor plots
#' @param ylab2 A character string for the label of the y-axis of the concentration plots
#' @param main A character string for the title label plot
#' @param ... Additional parameters to generic plot functions (not used)
#'
#' @return A graphic with the results of the validation
#'
#' @import ggplot2
#' @importFrom stats quantile
#'
#' @export
#'
#' @examples
#' \dontrun{
#' data(betacyfluthrinChronic) # Load dataset for validation
#' data(fitBetacyfluthrin_Chronic)
#' validation <- validate(fitBetacyfluthrin_Chronic, betacyfluthrinChronic)
#' plot(validation)
#' }
plot.beeSurvValidation <- function(x,
                                   ...,
                                   xlab = "Time [d]",
                                   ylab1 = "Number of survivors",
                                   ylab2 = "Concentration",
                                   main = paste("Validation of a BeeGUTS model calibrated for",
                                                x$beeSpecies, "on a ", x$typeData, "for", x$beeSpeciesVal) ) {
  # Check for correct class
  if (!is(x,"beeSurvValidation")) {
    stop("plot.beeSurvValidation: an object of class 'beeSurvValidation' is expected")
  }

  yLimits <- c(0, max(x$sim$NSurv, x$sim$Nsurv_qsup95_check))


  EFSA_criteria <- x$EFSA$Percent_PPC
  EFSA_criteria$PPC <- round(EFSA_criteria$PPC, digits = 2)
  EFSA_criteria$PPC_global <- ""
  EFSA_criteria$PPC_global[1] <- round(x$EFSA$Percent_PPC_global, digits = 2)
  EFSA_criteria$NRMSE <- round(x$EFSA$Percent_NRMSE$NRMSE, digits = 2)
  EFSA_criteria$NRMSE_global <- ""
  EFSA_criteria$NRMSE_global[1] <- round(x$EFSA$Percent_NRMSE_global, digits = 2)
  EFSA_criteria$SPPE <-  round(x$EFSA$Percent_SPPE$SPPE, digits = 2)
  ###############################################
  colnames(x$sim)[3] <- "Treatment" # Rename column name for plotting purposes

  ggSurv <- ggplot(data = x$sim, aes(x = time, y = Nsurv_q50_valid,  group = Treatment)) +
    geom_line(color = "blue") +
    geom_point(data = x$sim, aes(x=time, y=Nsurv,  group = Treatment)) +
    geom_ribbon( aes(x= time, ymin = Nsurv_qinf95_valid, ymax = Nsurv_qsup95_valid, group = Treatment), fill = "blue", alpha = 0.2)+
    scale_y_continuous(limits = yLimits) +
    xlab(xlab) +
    ylab(ylab1) +
    facet_grid(~Treatment) +
    theme(
      strip.background = element_blank(),
      strip.text.x = element_blank()
    )

  ggConc <- ggplot(data = x$dataModel, aes(x=SurvivalTime, y = Conc)) +
    geom_line() +
    geom_point(data = x$data) +
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
#' @return A graphic with results of the forward prediction
#'
#' @import ggplot2
#' @importFrom stats quantile
#'
#' @export
#'
#' @examples
#' \dontrun{
#' dataPredict <- data.frame(time = c(1:10, 1:10, 1:10),
#'                      conc = c(rep(5, 10), rep(10, 10), rep(15, 10)),
#'                      replicate = c(rep("rep1", 10), rep("rep2", 10), rep("rep3", 10)),
#'                      NSurv = c(rep(5, 10), rep(10, 10), rep(15, 10)))
#' data(fitBetacyfluthrin_Chronic)
#' prediction <- predict(fitBetacyfluthrin_Chronic, dataPredict)
#' plot(prediction)
#' }
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
#' @return A graphic with the traceplots and densities of the fit
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






#' Plotting method for \code{ppc} objects
#'
#' @param x An object of class \code{ppc}.
#' @param data_type  A string designating the type of data to be plotted: \code{length},
#' \code{reproduction} or \code{exposure}
#' @param \dots  Further arguments to be passed to generic methods.
#'
#' @return  an object of class \code{ggplot}.
#'
#' @example
#' data(fitBetacyfluthrin_Chronic)
#' out <- ppc(fitBetacyfluthrin_Chronic)
#' plot(out)
#'
#' @export
plot.ppc <- function(x, ...) {
  Nsurv_ppc <- x
  ppc_pct<- round(nrow(Nsurv_ppc[Nsurv_ppc$col=="green",])/nrow(Nsurv_ppc)*100, digits = 2)
  nrmse<- round(sqrt(sum((Nsurv_ppc$value-Nsurv_ppc$median)^2, na.rm = TRUE)/nrow(Nsurv_ppc))/mean(Nsurv_ppc$value,na.rm = TRUE)*100, digits=2)

  ggOut <-ggplot() +
    geom_segment(aes(x = value, xend = value,
                     y =q_0.025 , yend =q_0.975 ), data = Nsurv_ppc,
                 color = Nsurv_ppc$col)+
    geom_point(aes(x = value, y = median), Nsurv_ppc)+
    geom_abline(intercept = 0, slope = 1, size=0.7)+
    expand_limits(y = 0) +
    expand_limits(x = 0) +
    theme_minimal()+
    coord_fixed(ratio=1)+
    labs(x = "Observed number of survivors",
         y= "Predicted number of survivors") +
    theme(axis.title = element_text(size=7))+
    ggtitle(paste0("Survival \nPPC= ",ppc_pct,"%", "\nNRMSE= ", nrmse,"%"))

  return(ggOut)
}
