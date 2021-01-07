#' Read and format the data for the BeeGUTS model
#'
#' @description DESCRIPTION TO COMPLETE
#'
#' @param file_location Location of a text file containing two datasets, one for the survival data,
#' and one for the concentration data. Both datasets must contain the same number of column in the same order.
#' The following columns must be included:
#' \itemize{
#'     \item \code{Survival time [d]}: a vector of time in days
#'     \item \code{Control} A vector of number of survivors for the control
#'     \item \code{T1} - \code{Tn} A vector of number of survivors for the treatments
#'     T1 to Tn, one column per treatment.
#'     }
#' \itemize{
#'     \item \code{Concentration time [d]}: a vector of time in days.
#'     \item \code{Control} A vector of concentrations for the control
#'     \item \code{T1} - \code{Tn} A vector of concentration for the treatments
#'     T1 to Tn, one column per treatment.
#'     }
#'     For the \code{Acute_Oral} and \code{Acute_Contact}, only the initial
#'     exposure concentration at time 0 is required.
#'
#' @param test_type the test type amongst "Acute_Oral", "Acute_Contact", and "Chronic_Oral"
#' @param bee_species the bee type. At the moment only "Honey_Bee" is supported
#' @param ... Optional arguments to be passed to the concentration reconstruction (e.g.
#'  \code{k_sr =} for the stomach release rate (d-1), default is 0.625,
#'  \code{k_ca =} contact availability rate (d-1), default is 0.4), or
#'  \code{cTime =} the duration of exposure in days for the acute oral tests, default is 0.25 d
#'
#' @return DESCRIPTION TO COMPLETE
#' @export
#'
#' @examples
#' \dontrun{
#' file_location <- system.file("extdata", "betacyfluthrin_chronic_ug.txt", package = "BeeGUTS")
#' lsData <- dataGUTS(file_location = file_location, test_type = 'Chronic_Oral')
#' }
dataGUTS <- function(file_location = NULL,
                     test_type = NULL,
                     bee_species = "Honey_Bee",
                     ...) { # Possibility to add non default ksR, and kca

  # Ensure a correct filename and a correct types is entered
  if (is.null(file_location) || !file.exists(file_location) ||
      (!grepl("\\.txt$", file_location) && !grepl("\\.csv$", file_location)) ) {
    stop("You need to specify a path to the correct 'file_location' with a '.txt.' or '.csv' extension.")
  }
  if (is.null(test_type) ||  !(test_type %in% c("Acute_Oral", "Acute_Contact", "Chronic_Oral"))) {
    stop("You need to specifiy a correct data 'test_type' amongst 'Acute_Oral', 'Acute_Contact', or 'Chronic_Oral'.")
  }
  if (is.null(bee_species) ||  !(bee_species %in% c("Honey_Bee"))) {
    stop("You need to specifiy a correct 'bee_species' amongst 'HoneyBee'.
         Other types of bees are not yet implemented.")
  }

  # Load the survival data from the file
  # Check where the survival data starts and ends
  rawData <- readLines(file_location)
  skipLine_surv <- grep("Survival", rawData)
  nrowLine_surv <- grep("Concentration unit", rawData)
  # Load the survival data
  tbSurv <- data.table::fread(file_location, skip = skipLine_surv, header = T, nrow = nrowLine_surv - (skipLine_surv + 1L) )
  colnames(tbSurv)[1] <- c("SurvivalTime") # Set unique name for time column

  # Load the concentration data from the file
  # Check where the concentration data starts and ends
  skipLine_conc <- grep("Concentration time", rawData)
  # Load the concentration data
  tbConc <- data.table::fread(file_location, skip = skipLine_conc, header = T)
  colnames(tbConc)[1] <- c("SurvivalTime")

  if (ncol(tbSurv)  != ncol(tbConc)) {
    stop("The number of columns in the survival dataset differs from the number of
         columns in the concentration dataset")
  }

  # Load the units
  chUnits <- rawData[nrowLine_surv]

  # Recalculate the concentrations based on the experiment type
  if(test_type == "Acute_Oral") {
    dfConcModel <- concAO(tbConc[1,-1], expTime = max(tbSurv[,1]), ...)
    } else if(test_type == "Acute_Contact") {
    dfConcModel <- concAC(tbConc[1,-1], expTime = max(tbSurv[,1]), ...)
    } else {
    dfConcModel <- data.frame(SurvivalTime = tbConc[,1], tbConc[,2:ncol(tbConc)])
  }

  # Transform into long data
  tbSurv_long <- tidyr::gather(tbSurv, Treatment, NSurv, -SurvivalTime)
  tbConc_long <- tidyr::gather(tbConc, Treatment, Conc, -SurvivalTime)
  dfConcModel_long <- tidyr::gather(dfConcModel, Treatment, Conc, -SurvivalTime)

  # Return
  lsOut <- list(survData = tbSurv,
                survData_long = tbSurv_long,
                concData = tbConc,
                concData_long = tbConc_long,
                unitData = chUnits,
                typeData = test_type,
                beeSpecies = bee_species,
                concModel = dfConcModel,
                concModel_long = dfConcModel_long)
  class(lsOut) <- "beeSurvData"
  return(lsOut)
}


# Internal
# Sub-function to recalculate the concentrations based on the type of test and species
# Acute oral tests


#' Title
#'
#' @param cExt A dataframe of concentrations at time 0 concentration applied
#' @param cTime The duration of exposure in days, default is 0.25 d
#' @param k_sr Stomach release rate (d-1), default is 0.625
#' @param expTime The duration of the experiment in days
#'
#' @return
#' @export
#'
#' @examples conc <- concAO(cExt = cbind(3.5, 6, 8, 10), cTime = 0.25, expTime = 4)
concAO <- function(cExt, cTime = 0.25, expTime, k_sr = 0.625) {
  timePoint <- seq(0, expTime, 0.1)
  cExt <- cExt[rep(seq_len(nrow(cExt)), each = length(timePoint)),] # Expend cExt to allow concentration calculation for all time points
  out <- (cExt * (timePoint / cTime) * (timePoint <= cTime))  + (cExt * exp(-k_sr * (timePoint - cTime)) * (timePoint > cTime))
  return(data.frame(SurvivalTime = timePoint, out))
}

# Acute contact test
#' Title
#'
#' @param cExt The concentration applied
#' @param expTime The duration of the experiment in days
#' @param k_ca Contact availability rate (d-1), default is 0.4
#'
#' @return
#' @export
#'
#' @examples conc <- concAC(cbind(3.1, 4, 6, 8), 4)
concAC <- function(cExt, expTime, k_ca = 0.4) {
  timePoint <- seq(0, expTime, 0.1)
  cExt <- cExt[rep(seq_len(nrow(cExt)), each = length(timePoint)),] # Expend cExt to allow concentration calculation for all time points
  out <-cExt * exp(-k_ca * timePoint)
  return(data.frame(SurvivalTime = timePoint, out))
}

