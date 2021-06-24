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
#' data(betacyfluthrinChronic) # Load dataset for validation
#' data(fitBetacyfluthrin_Chronic) # Load fit object
#' validation <- validate.beeSurvFit(fitBetacyfluthrin_Chronic, betacyfluthrinChronic)
#' plot(validation, betacyfluthrinChronic)
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


  #############################################################################################################
  ### Calculations of PPC, NMRSE, SPPE applying the morse function predict_Nsurv_check

  library(dplyr)
  df_global <- object$sim %>%
    mutate(ppc_matching_valid = ifelse(Nsurv_qinf95_valid > Nsurv | Nsurv_qsup95_valid < Nsurv, 0, 1),
           SE_id = (Nsurv - Nsurv_q50_valid)^2)



  df_ppc <- df_global %>%
    select(Nsurv, time, Nsurv_q50_valid, Nsurv_q50_check, replicate, ppc_matching_valid) %>%
    group_by(replicate) %>%
    summarise(PPC = mean(ppc_matching_valid)*100)

  percent_ppc_timeserie <- sum(df_global$ppc_matching_valid) / nrow(df_global) * 100

  # NRMSE
  df_nrmse <- df_global %>%
    select(Nsurv, time, Nsurv_q50_valid, Nsurv_q50_check, replicate, SE_id) %>%
    group_by(replicate) %>%
    summarise(NRMSE = sqrt(mean(SE_id)) / mean(Nsurv) * 100)

  nrmse <- sqrt(mean(df_global$SE_id)) / mean(df_global$Nsurv) * 100


  # SPPE

  df_sppe <- df_global %>%
    select(Nsurv, time, Nsurv_q50_valid, Nsurv_q50_check, replicate) %>%
    group_by(replicate) %>%
    arrange(replicate,time) %>%
    summarise(SPPE = (last(Nsurv) - last(Nsurv_q50_valid)) / first(Nsurv) * 100 )


  EFSA_criteria <- as.data.frame(df_ppc)
  EFSA_criteria$PPC_global <- ""
  EFSA_criteria$PPC_global[1] <- percent_ppc_timeserie
  EFSA_criteria$NRMSE <- as.data.frame(df_nrmse$NRMSE)
  EFSA_criteria$NRMSE_global <- ""
  EFSA_criteria$NRMSE_global[1] <- nrmse
  EFSA_criteria$SPPE <- as.data.frame(df_sppe$SPPE)
  ###############################################

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

 #table <- ggplot(data = dfDataConc_long, aes(x=SurvivalTime, y = Conc)) + annotation_custom(gridExtra::tableGrob(EFSA_criteria), xmin=35, xmax=50, ymin=-2.5, ymax=-1)
  table <- gridExtra::tableGrob(EFSA_criteria, rows=NULL)

  ggOut1 <- cowplot::plot_grid(ggConc, ggSurv, align = "v",nrow = 2)
  ggOut <- cowplot::plot_grid(ggOut1, table, nrow = 2)



#############################################################################
  return(ggOut)
}

