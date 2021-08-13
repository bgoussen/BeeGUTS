#----------------------------- Internal functions -----------------------------


# Prepare data to be used for the beeGUTS Bayesian inference in stan
dataFitStan <- function(data,
                        modelType = NULL,
                        odeControl = NULL,
                        priorsList = NULL) {

  # Check correct user inputs
  if (is.null(modelType) || !(modelType %in% c("SD", "IT"))) {
    stop("You need to specifiy a correct 'modelType' amongst 'SD' and 'IT'.
         'PROPER' is not yet implemented. When selecting 'IT' please also
         provide a 'distribution' parameter amongst 'loglogistic' and 'lognormal'.")
  }


  # Prepare priors
  if (is.null(priorsList)) {
    priors <- priorsBeeGUTS(data, modelType = modelType)$priorsList
  } else {
    # ADD CHECK FOR CORRECT PRIOR LIST !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    priors <- priorsList
  }

  lsOUT <- unlist(list(priors, odeControl), recursive = FALSE)

  # Prepare data
  ## Number of groups.
  ## Different datasets are treated as additional groups
  nDatasets <- data$nDatasets
  nGroups <- c()
  for (i in 1:nDatasets) {nGroups<-append(nGroups,length(unique(data$survData_long[[i]]$Treatment)))}
  lsOUT$nGroup <- sum(nGroups)

  # join the datasets to treat everything as a single group
  # Concentrations
  dataConc <- dplyr::bind_rows(data$concModel_long) %>%
    dplyr::filter(!is.na(Conc)) %>%
    dplyr::arrange(Dataset, Treatment, SurvivalTime) %>%
    dplyr::mutate(idAll = dplyr::row_number() )

  dataConc_id = dataConc %>%
    dplyr::group_by(Dataset, Treatment) %>%
    dplyr::summarise(idC_lw = min(idAll),
                     idC_up = max(idAll))

  lsOUT$nData_conc <- nrow(dataConc)

  lsOUT$conc <- dataConc$Conc
  lsOUT$tconc <- dataConc$SurvivalTime

  lsOUT$replicate_conc <- dataConc$Treatment

  lsOUT$idC_lw <- dataConc_id$idC_lw
  lsOUT$idC_up <- dataConc_id$idC_up

  # Survival
  dataNsurv <- dplyr::bind_rows(data$survData_long) %>%
    dplyr::filter(!is.na(NSurv)) %>%
    dplyr::arrange(Dataset, Treatment, SurvivalTime) %>%
    dplyr::mutate(idAll = dplyr::row_number() ) %>%
    dplyr::group_by(Dataset, Treatment) %>%
    dplyr::mutate(Nprec = ifelse( SurvivalTime == min(SurvivalTime), NSurv, dplyr::lag(NSurv) ),
                  Ninit = max(NSurv)) %>%  # since it is grouped by replicate
    dplyr::ungroup()

  dataNsurv_id = dataNsurv %>%
    dplyr::group_by(Dataset, Treatment) %>%
    dplyr::summarise(idS_lw = min(idAll),
                     idS_up = max(idAll))


  lsOUT$nData_Nsurv <- nrow(dataNsurv)

  lsOUT$Nsurv <- dataNsurv$NSurv
  lsOUT$Nprec <- dataNsurv$Nprec
  lsOUT$Ninit <- dataNsurv$Ninit

  lsOUT$tNsurv <- dataNsurv$SurvivalTime
  lsOUT$replicate_Nsurv <- dataNsurv$Treatment

  lsOUT$idS_lw <- dataNsurv_id$idS_lw
  lsOUT$idS_up <- dataNsurv_id$idS_up

  return(lsOUT)
}



# Prepare priors (Adapted from Virgile Baudrot gutsRstan)
priorsBeeGUTS <- function(x, modelType = NULL){

  # Remove time = 0
  dataSurv <- dplyr::bind_rows(x$survData_long)
  dataSurv <- dplyr::filter(dataSurv, SurvivalTime != 0)
  dataConc <- dplyr::bind_rows(x$concModel_long)
  dataConc <- dplyr::filter(dataConc, SurvivalTime != 0)
  #dataSurv <- dplyr::filter(x$survData_long, SurvivalTime != 0)
  #dataConc <- dplyr::filter(x$concModel_long, SurvivalTime != 0)

  # Parameter calculation of concentration min and max
  concMin <- 1e-6 # here consider minimal concentration for prior to be close to 0. Original: min(data$conc[data$conc != 0], na.rm = TRUE) # to remove 0 and NA
  concMax <- max(dataConc$Conc, na.rm = TRUE)

  timeMin <- min(dataSurv$SurvivalTime)
  timeMax <- max(dataSurv$SurvivalTime)

  concUniq <- sort(unique(dataConc$Conc))
  concUniq_Prec <- dplyr::lag(concUniq)
  concMinDelta <- min(concUniq - concUniq_Prec, na.rm = TRUE)

  # dominant rate constant: kd
  kdMax <- -log(0.001) / timeMin
  kdMin <- 1e-6 # Here consider the kd has more chances to go to slow kinetics -log(0.999) / (timeMax)

  # background hazard rate
  hbMax <- -log(0.5) / timeMin
  hbMin <- -log(0.999) / timeMax

  # killing rate parameter: bw
  bwMax <- -log(0.001) / (timeMin * concMinDelta)
  bwMin <- -log(0.999) / (timeMax * (concMax - concMin))

  # beta
  betaMin_log10 <- -2
  betaMax_log10 <- 2

  priorsMinMax <- list(
    concMin = concMin,
    concMax = concMax,
    kdMin = kdMin,
    kdMax = kdMax,
    hbMin = hbMin,
    hbMax = hbMax,
    bwMin = bwMin,
    bwMax = bwMax,
    zwMin = concMin,
    zwMax = concMax,
    mwMin = concMin,
    mwMax = concMax,
    betaMin = betaMin_log10,
    betaMax = betaMax_log10)

  elMinMax_general <- c("kdMin", "kdMax", "hbMin", "hbMax")
  elMinMax_SD <- c(elMinMax_general, c("bwMin", "bwMax", "zwMin", "zwMax"))
  elMinMaxt_IT <- c(elMinMax_general, c("mwMin", "mwMax", "betaMin", "betaMax"))
#  elMinMax_PROPER <- c(elMinMax_general, c("bwMin", "bwMax", "mwMin", "mwMax", "betaMin", "betaMax"))

  priorsMinMax <- switch(modelType,
                         IT = priorsMinMax[elMinMaxt_IT],
                         SD = priorsMinMax[elMinMax_SD])#,
                         #PROPER = priorsMinMax[elMinMax_PROPER])

  ##
  ## Construction of the list of priors
  ##
  priorsList <-  list(
    ## dominant rate constant: kd
    kdMean_log10 = .priorMean(kdMin, kdMax),
    kdSD_log10 = .priorSD(kdMin, kdMax),
    ## background hazard rate
    hbMean_log10 =  .priorMean(hbMin, hbMax),
    hbSD_log10 = .priorSD(hbMin, hbMax),
    ## killing rate parameter: bw
    bwMean_log10 = .priorMean(bwMin, bwMax),
    bwSD_log10 = .priorSD(bwMin, bwMax),
    ## non effect threshold: zw
    zwMean_log10 = .priorMean(concMin, concMax),
    zwSD_log10 = .priorSD(concMin, concMax),
    ## non effect threshold: scale parameter & median of a log-logistic distribution
    mwMean_log10 = .priorMean(concMin, concMax),
    mwSD_log10 = .priorSD(concMin, concMax),
    ## shape parameter of a log-logistic distribution
    betaMin_log10 = betaMin_log10,
    betaMax_log10 = betaMax_log10
  )

  elList_general <- c("kdMean_log10", "kdSD_log10", "hbMean_log10", "hbSD_log10")
  elList_SD <- c(elList_general, c("bwMean_log10", "bwSD_log10", "zwMean_log10", "zwSD_log10"))
  elList_IT <- c(elList_general, c("mwMean_log10", "mwSD_log10", "betaMin_log10", "betaMax_log10"))
  # elList_PROPER <- c(elList_general, c("bwMean_log10", "bwSD_log10", "mwMean_log10", "mwSD_log10", "betaMin_log10", "betaMax_log10"))


  priorsList <- switch(modelType,
                       IT = priorsList[elList_IT],
                       SD = priorsList[elList_SD])#,
                       # ROPER = priorsList[elList_PROPER])

  return(list(priorsList = priorsList,
              priorsMinMax = priorsMinMax))
}


# internal --------------------------------------------------------------------

# Compute priors Mean and SD for lognormal distribution

.priorMean <- function(xMin, xMax){
  (log10(xMax) + log10(xMin)) / 2
}

.priorSD <- function(xMin, xMax){
  (log10(xMax) - log10(xMin)) / 4
}
