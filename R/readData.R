data_GUTS <- function(file_location = NULL, test_type = NULL, bee_species = "HoneyBee") {
  # Ensure a correct filename and a correct types is entered
  if (is.null(file_location) || !file.exists(file_location) ||
      (!grepl("\\.txt$", file_location) && !grepl("\\.csv$", file_location)) ) {
    stop("You need to specify a path to the correct 'file_location' with a '.txt.' or '.csv' extension.")
  }
  if (is.null(test_type) ||  !(test_type %in% c("AcuteOral", "AcuteContact", "ChronicOral"))) {
    stop("You need to specifiy a correct data 'test_type' amongst 'AcuteOral', 'AcuteContact', or 'ChronicOral'.")
  }
  if (is.null(bee_species) ||  !(bee_species %in% c("HoneyBee"))) {
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

  # Load the concentration data from the file
  # Check where the concentration data starts and ends
  skipLine_conc <- grep("Concentration time", rawData)
  # Load the concentration data
  tbConc <- data.table::fread(file_location, skip = skipLine_conc, header = T)

  # Units
  chUnits <- rawData[nrowLine_surv]

  # Return
  lsOut <- list(survData = tbSurv,
                concData = tbConc,
                unitData = chUnits,
                typeData = test_type,
                beeSpecies = bee_species)
  class(lsOut) <- "survData"
  return(lsOut)
}

test <- data_GUTS(file_location = file.choose(), test_type = 'ChronicOral')

