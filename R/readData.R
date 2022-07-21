#' Read and format the data for the BeeGUTS model
#'
#' @description Read data from a \code{text} or \code{csv} file and recalculate the
#' exposure profile depending on the type of experiment (acute oral, acute contact, chronic oral).
#'
#' @param file_location List of Locations of text files containing each two datasets, one for the survival data,
#' and one for the concentration data. Both datasets must be included in the same file and contain the same number of column in the same order.
#' The following columns must be included in the survival dataset:
#' \itemize{
#'     \item \code{Survival time \[d\]}: a vector of time in days
#'     \item \code{Control} A vector of number of survivors for the control
#'     \item \code{T1} - \code{Tn} A vector of number of survivors for the treatments
#'     T1 to Tn, one column per treatment.
#'     }
#' A line containing the \code{Concentration unit} must be included directly after the end of
#' the last row of the survival data.
#'
#' The following columns must be included in the concentration dataset
#' \itemize{
#'     \item \code{Concentration time \[d\]}: a vector of time in days.
#'     \item \code{Control} A vector of concentrations for the control
#'     \item \code{T1} - \code{Tn} A vector of concentration for the treatments
#'     T1 to Tn, one column per treatment.
#'     }
#'     For the \code{Acute_Oral} and \code{Acute_Contact}, only the initial
#'     exposure concentration at time 0 is required.
#'
#' See detail section for example
#'
#' @param test_type list of test types amongst "Acute_Oral", "Acute_Contact", and "Chronic_Oral"
#' this list must have the same length of the list of file locations
#' @param bee_species the bee type. At the moment only "Honey_Bee" is supported
#' @param NA_string a character vector of strings which are to be interpreted as NA values
#' @param ... Optional arguments to be passed to the concentration reconstruction (e.g.
#' \itemize{
#'  \item \code{k_sr =} for the stomach release rate (d-1), default is 0.625,
#'  \item \code{k_ca =} contact availability rate (d-1), default is 0.4), or
#'  \item \code{cTime =} the duration of exposure in days for the acute oral tests, default is 0.25 d
#'  \item \code{cstConcCal = } logical, recalculate concentration in the Chronic_Oral test from mg a.s./kg feed to Xg/bee (default is TRUE)
#'  \item \code{f_rate = } numerical vector, feeding rate used in the concentration recalculation in the Chronic_Oral (default is 25 mg/bee/day)
#'  \item \code{targConc =} numerical scalar, target concentration unit in the recalculation in the Chronic_Oral, 1 for µg/bee, 2 for ng/bee, 3 for mg/bee (default is 1).
#' }
#'
#' @return An object of class \code{beeSurvData}, which is a list with the following information:
#' \item{nDatasets}{Number of files passed to the function}
#' \item{survData}{A table containing the survival data as entered by the user in the input file}
#' \item{survData_long}{A data frame containing the survival data in long format for model purposes}
#' \item{concData}{A table containing the concentration data as entered by the user in the input file}
#' \item{concData_long}{A data frame containing concentration data in long format}
#' \item{unitData}{A character vector containing the units of the data as entered in the line \code{Concentration unit}
#' of the input file}
#' \item{typeData}{A character vector containing the type of experiment}
#' \item{beeSpecies}{A character vector containing the type bee}
#' \item{concModel}{A data frame containing the concentration data as recalculated by the model}
#' \item{concModel_long}{A data frame containing the concentration data as recalculated by the model in a long format}
#' Each element of the list is itself a list to account for multiple files that can be passed as input.
#'
#' @details
#' The filename must begin with name of the chemical substance being tested and
#' each word of the filename should be separated via an underscore '_'.
#'
#' #' Example of formatting of the input file for a chronic oral study
#' \tabular{lllllll}{
#' Survival time \[d\] \tab	Control	\tab T1	\tab T2	\tab T3	\tab T4	\tab T5 \cr
#' 0	\tab 120	\tab 120	\tab 120	\tab 120	\tab 120	\tab 120 \cr
#' 1	\tab 120	\tab 118	\tab 117	\tab 112	\tab 115	\tab 94 \cr
#' 2	\tab 120	\tab 118	\tab 115	\tab 112	\tab 98	\tab 88 \cr
#' 3	\tab 120	\tab 118	\tab 114	\tab 106	\tab 83	\tab 27 \cr
#' 4	\tab 119	\tab 118	\tab 113	\tab 103	\tab 67	\tab 9 \cr
#' 5	\tab 119	\tab 118	\tab 112	\tab 100	\tab 43	\tab 3 \cr
#' Concentration unit: ug/bee/day \tab\tab\tab\tab\tab\tab \cr
#' Concentration time \[d\]	\tab Control	\tab T1	\tab T2	\tab T3	\tab T4	\tab T5 \cr
#' 0	\tab 0	\tab 3	\tab 7	\tab 12	\tab 41	\tab 68 \cr
#' 5	\tab 0	\tab 3	\tab 7	\tab 12	\tab 41	\tab 68
#' }
#'
#' @export
#'
#' @examples
#' \donttest{
#' file_location <- system.file("extdata", "betacyfluthrin_chronic_ug.txt", package = "BeeGUTS")
#' lsData <- dataGUTS(file_location = c(file_location),
#'                   test_type = c('Chronic_Oral'), cstConcCal = FALSE)
#' }
dataGUTS <- function(file_location = NULL,
                     test_type = NULL,
                     bee_species = "Honey_Bee",
                     NA_string = getOption("datatable.na.strings","NA"),
                     ...) { # Possibility to add non default ksR, and kca

  ## check that file_location and test_types have the same length
  if (length(file_location) != length(test_type)){
    stop("Mismatch between number of files and number of tests.")
  }
  name_chemical <- c() # array to store the name of the chemical extracted from the filename
  for (i in 1:length(file_location)){
    # Ensure a correct filename and a correct types is entered
    if (is.null(file_location[i]) || !file.exists(file_location[i]) ||
        (!grepl("\\.txt$", file_location[i]) && !grepl("\\.csv$", file_location[i])) ) {
      stop("You need to specify a path to the correct 'file_location' with a '.txt.' or '.csv' extension.")
    }
    if (is.null(test_type[i]) ||  !(test_type[i] %in% c("Acute_Oral", "Acute_Contact", "Chronic_Oral"))) {
      stop("You need to specifiy a correct data 'test_type' amongst 'Acute_Oral', 'Acute_Contact', or 'Chronic_Oral'.")
    }
    # sanity check on the inserted chemical species
    splitpath <- strsplit(strsplit(file_location[i], "_")[[1]],"/")[[1]]
    name_chemical <- append(name_chemical, splitpath[length(splitpath)])
  }
  if (is.null(bee_species) ||  !(bee_species %in% c("Honey_Bee"))) {
    stop("You need to specifiy a correct 'bee_species' amongst 'HoneyBee'.
         Other types of bees are not yet implemented.")
  }
  # check that there are no multiple entries in the bee_species argument
  if (length(bee_species) > 1){
    warning("You entered multiple entries for the bee species. Only one is required.
            Calibration on different species is not possible.
            Using only the first entry.")
    bee_species <- bee_species[[1]] # to make sure that the entry is always a string even if a list is passed
  }

  # Empty arrays for the objects to be returned. Values for each test are appended
  tbSurv <- list()
  tbConc <- list()
  tbSurv_long <- list()
  tbConc_long <- list()
  chUnits <- list()
  # use single value for now
  bee_species <- bee_species #as.list(rep(bee_species, length(file_location)))
  dfConcModel <- list()
  dfConcModel_long <- list()

  # Load the survival data from the file
  # Check where the survival data starts and ends for each file
  nDatasets <- length(file_location)
  for (i in 1:nDatasets){
    rawData <- readLines(file_location[i])
    skipLine_surv <- grep("Survival", rawData)
    nrowLine_surv <- grep("Concentration unit", rawData)
    # Load the survival data
    tbSurv_aux <- data.table::fread(file_location[i], skip = skipLine_surv - 1L, header = T, nrow = nrowLine_surv - (skipLine_surv + 1L),
                                    na.strings = NA_string)
    colnames(tbSurv_aux)[1] <- c("SurvivalTime") # Set unique name for time column
    tbSurv_aux$Dataset <- i
    tbSurv <- append(tbSurv, list(tbSurv_aux))
    # Load the concentration data from the file
    # Check where the concentration data starts and ends
    skipLine_conc <- grep("Concentration time", rawData)
    # Load the concentration data
    tbConc_aux <- data.table::fread(file_location[i], skip = skipLine_conc - 1L, header = T, na.strings = NA_string)
    tbConc_aux$Dataset <- i
    colnames(tbConc_aux)[1] <- c("SurvivalTime")
    tbConc <- append(tbConc, list(tbConc_aux))
    if (ncol(tbSurv_aux)  != ncol(tbConc_aux)) {
      stop("The number of columns in the survival dataset differs from the number of
         columns in the concentration dataset")
    }
    # Load the units
    # TODO: insert code to extract exact value of units
    # TODO: recalculate if units are different for different tests
    # remove all possible whitespaces and substitute with simple whitespace
    chUnits <- append(chUnits, gsub("[[:blank:]]", " ",rawData[nrowLine_surv]))

    # Recalculate the concentrations based on the experiment type
    if(test_type[i] == "Acute_Oral") {
      dfConcModel_aux <- concAO(tbConc_aux[1,-1], expTime = max(tbSurv_aux[,1]), ...)
      dfConcModel_aux$Dataset <- i # reallocate the column because it gets overwritten
    } else if(test_type[i] == "Acute_Contact") {
      dfConcModel_aux <- concAC(tbConc_aux[1,-1], expTime = max(tbSurv_aux[,1]), ...)
      dfConcModel_aux$Dataset <- i # reallocate the column because it gets overwritten
    } else {
      lsConcModel_aux <- concCst(tbConc_aux, ...)
      dfConcModel_aux <- lsConcModel_aux$Concentrations
      if (!is.null(lsConcModel_aux$Units)) {
        chUnits[[i]] <- lsConcModel_aux$Units
      }
    }
    dfConcModel <- append(dfConcModel, list(dfConcModel_aux))

    # Transform into long data
    tbSurv_long_aux <- tidyr::gather(tbSurv_aux, Treatment, NSurv, -SurvivalTime, -Dataset)
    tbSurv_long_aux <- na.omit(tbSurv_long_aux) # Remove NAs
    tbConc_long_aux <- tidyr::gather(tbConc_aux, Treatment, Conc, -SurvivalTime, -Dataset)
    tbConc_long_aux <- na.omit(tbConc_long_aux) # Remove NAs
    dfConcModel_long_aux <- tidyr::gather(dfConcModel_aux, Treatment, Conc, -SurvivalTime, -Dataset)
    dfConcModel_long_aux <- na.omit(dfConcModel_long_aux) # Remove NAs

    tbSurv_long <- append(tbSurv_long, list(tbSurv_long_aux))
    tbConc_long <- append(tbConc_long, list(tbConc_long_aux))
    dfConcModel_long <- append(dfConcModel_long, list(dfConcModel_long_aux))
  }

  ## TODO: This part has to be improved substantially. Needs to account for all
  ##       possible ways to write the units in the experimental data file.
  units_check <- length(unique(chUnits)) == 1
  if (!units_check){
    warning("!!! IMPORTANT NOTE !!!
            Check the units in the data file. There seems to be a mismatch.
            You can continue with the fit, but the results might be incorrect.
            See 'object$unitData' for more information.")
  }
  ## TODO: Find a good strategy to ensure safety checks
  ## to ensure using the same chemical species
  chemical_check <- length(unique(tolower(name_chemical))) == 1
  if (!chemical_check){
    warning("!!! IMPORTANT NOTE !!!
              Make sure you know what you are doing.
              Chemical species extracted from filename of the two files does not match.
              The data will continue to be read, but there might be inconsistencies.")
  }

  # Return
  lsOut <- list(nDatasets = nDatasets,
                survData = tbSurv,
                survData_long = tbSurv_long,
                concData = tbConc,
                concData_long = tbConc_long,
                unitData = chUnits,
                typeData = test_type,
                beeSpecies = bee_species,
                concModel = dfConcModel,
                concModel_long = dfConcModel_long)
  class(lsOut) <- "beeSurvData"

  # This is to keep the compatibility with the current code if there is a single file passed
  # TODO: Remove once all the changes have been implemented
  #if (length(file_location)==1){
  #    for (name in names(lsOut)) {lsOut[name]<-lsOut[name][[1]]}
  #}

  return(lsOut)
}


# Internal
# Sub-function to recalculate the concentrations based on the type of test and species

# Acute oral tests
#' Recalculate concentration for the acute oral tests for bees
#'
#' @param cExt A dataframe of concentrations at time 0 concentration applied
#' @param cTime The duration of exposure in days, default is 0.25 d
#' @param k_sr Stomach release rate (d-1), default is 0.625
#' @param expTime The duration of the experiment in days
#' @param ... Not used
#'
#' @return A data frame containing a column with the time points and a column with the
#' recalculated concentrations
#' @export
#'
#' @examples conc <- concAO(cExt = cbind(3.5, 6, 8, 10), cTime = 0.25, expTime = 4)
concAO <- function(cExt, cTime = 0.25, expTime, k_sr = 0.625, ...) {
  timePoint <- seq(0, expTime, 0.1)
  cExt <- cExt[rep(seq_len(nrow(cExt)), each = length(timePoint)),] # Expend cExt to allow concentration calculation for all time points
  out <- (cExt * (timePoint / cTime) * (timePoint <= cTime))  + (cExt * exp(-k_sr * (timePoint - cTime)) * (timePoint > cTime))
  return(data.frame(SurvivalTime = timePoint, out))
}

# Acute contact test
#' Recalculate the concentrations for the acute contact tests for bees
#'
#' @param cExt The concentration applied
#' @param expTime The duration of the experiment in days
#' @param k_ca Contact availability rate (d-1), default is 0.4
#' @param ... Not used
#'
#' @return A data frame containing a column with the time points and a column with the
#' recalculated concentrations
#' @export
#'
#' @examples conc <- concAC(cbind(3.1, 4, 6, 8), 4)
concAC <- function(cExt, expTime, k_ca = 0.4, ...) {
  timePoint <- seq(0, expTime, 0.1)
  cExt <- cExt[rep(seq_len(nrow(cExt)), each = length(timePoint)),] # Expend cExt to allow concentration calculation for all time points
  out <-cExt * exp(-k_ca * timePoint)
  return(data.frame(SurvivalTime = timePoint, out))
}



# Chronic oral test
#' Recalculate the concentrations for the chronic oral tests for bees from
#' mg a.s./kg feed to \eqn{\mu}g/bee
#'
#' @param f_rate A vector containing the feeding rates of the bees in mg/bee/day. If the vector
#' is of size 1, the same feeding rate is used for all test conditions. If the vector
#' is of size >1, it should be of the same size as the number of condition and one
#' feeding rate must be provided per condition. Default is 25 mg/bee/day
#' @param cstConcCal Logical. Indicate if concentrations should be recalculated from
#' mg a.s./kg feed to Xg/bee
#' @param cExt The concentration dataframe in mg a.s./kg feed
#' @param targConc A numerical scalar representing the unit of the target concentration amongst (default = 1)
#' \itemize{
#' \item \code{1} for \eqn{\mu}g a.s./bee
#' \item \code{2} for ng a.s./bee
#' \item \code{3} for mg a.s./bee
#' }
#' @param ... Not used
#'
#' @return A data frame containing a column with the time points and a column with the
#' recalculated concentrations
#' @export
#'
#' @examples
#' cExt <- data.frame(SurvivalTime = c(0,10), Control = c(0,0),
#'         T1 = c(1, 1), T2 = c(5, 5), Dataset = c(1, 1))
#' conc <- concCst(cExt, targConc = 2)
concCst <- function(cExt, f_rate = c(25), targConc = 1, cstConcCal = TRUE, ...) {
  if (cstConcCal == FALSE) { # If recalculating chronic concentrations is not necessary, return early
    return(list(Units = NULL, Concentrations = data.frame(SurvivalTime = cExt[,1], cExt[,2:ncol(cExt)])))
  }

  if (cstConcCal == TRUE) {
    tmpConc <- cExt[,2:(ncol(cExt) - 1L)] # Remove dataset number for concentration calculations
    if (!(targConc %in% c(1, 2, 3))) {
      stop("targConc should be 1, 2, or 3")
    }
    concConvert <- switch(targConc, 1, 1000, 0.001) # Choose the correct target concentration
    concUnit <- switch(targConc, "\u00b5g/bee/day", "ng/bee/day", "mg/bee/day")
    if (length(f_rate) == 1){ # If only one feeding rate is provided, use it for all conditions
      f_rate <- rep(f_rate, times = length(tmpConc))
      out <- mapply('*', tmpConc, f_rate) / 1000 * concConvert # Correspond to (f_rate/1000) * tmpConc
    } else if (length(f_rate) == length(tmpConc)) { # If more than one feeding rate is provided, it should be of the same length than the number of conditions
      out <- mapply('*', tmpConc, f_rate) / 1000 * concConvert # Correspond to (f_rate/1000) * tmpConc
    } else {
      stop("Feeding rates should either be provided as a mean for the whole study
           or per treatment")
    }
    return(list(Units = concUnit, Concentrations = data.frame(SurvivalTime = cExt[,1], out, Dataset = cExt [,ncol(cExt)])))
  }
}
