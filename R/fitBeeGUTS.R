#' Fit a GUTS model for bees survival analysis using Bayesian Inference (stan)
#'
#' @description DESCRIPTION TO COMPLETE
#'
#' @param data
#' @param modelType
#' @param distribution
#' @param priorsList
#' @param parallel
#' @param nCores
#' @param nChains
#' @param nIter
#' @param nWarmup
#' @param thin
#' @param adaptDelta
#' @param odeIntegrator
#' @param relTol
#' @param absTol
#' @param maxSteps
#' @param ... Additional parameters to be passed to \code{sampling} from \code{stan}
#'
#' @return
#' @export
#'
#' @examples
#' \dontrun{
#' data(betacyfluthrinChronic)
#' fit <- fitBeeGUTS(betacyfluthrinChronic, modelType = "SD", nIter = 1000)
#' }
fitBeeGUTS <- function(data, # CHECK CORRECT DATA OBJECT IS USED !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                       modelType = NULL,
                       distribution = "loglogistic",
                       priorsList = NULL,
                       parallel = TRUE,
                       nCores = parallel::detectCores()-1L,
                       nChains = 3,
                       nIter = 2000,
                       nWarmup = floor(nIter / 2),
                       thin = 1,
                       adaptDelta = 0.95,
                       odeIntegrator = "rk45",
                       relTol = 1e-8,
                       absTol = 1e-8,
                       maxSteps = 1000,
                       ...) {
 # Check correct user inputs
  if (is.null(modelType) || !(modelType %in% c("SD", "IT"))) {
    stop("You need to specifiy a correct 'modelType' amongst 'SD' and 'IT'.
         'PROPER' is not yet implemented. When selecting 'IT' please also
         provide a 'distribution' parameter amongst 'loglogistic' and 'lognormal'.")
  }
  if (!(distribution %in% c("loglogistic", "lognormal"))) {
    stop("You need to specifiy a correct 'distribution' amongst 'loglogistic' and 'lognormal'.")
  }
  if (!(odeIntegrator %in% c("rk45"))) {
    stop("You need to specifiy a correct 'odeIntegrator' amongst 'rk45'.
         'bdf' is not yet implemented")
  }

  # Regroup control for the ode solver
  odeControl <- list(relTol = relTol, absTol = absTol, maxSteps = maxSteps)

  # Prepare data for inference with stan
  lsFullData <- dataFitStan(data, modelType, odeControl, priorsList)
  lsStanData <- lsFullData
  lsStanData$replicateConc <- NULL # NECESSARY?????????????????????????????????????????????????????????
  lsStanData$replicateNsurv <- NULL # NECESSARY?????????????????????????????????????????????????????????
  lsStanData$Ninit <- NULL # NECESSARY?????????????????????????????????????????????????????????

  if (modelType == "SD") {
    modelObject <- stanmodels$GUTS_SD
  }
  if (modelType == "IT") {
    modelObject <- stanmodels$GUTS_IT
    lsStanData$distribution <- switch(distribution, loglogistic = 1, lognormal = 2)
  }

  # Set options for parallel computing
  if (parallel == TRUE) {
    op <- options()
    options(mc.cores = as.integer(nCores))
  }

  # Sample MCMC chains
  fit <- rstan::sampling(
    object = modelObject,
    data = lsStanData,
    chains = nChains,
    iter = nIter,
    warmup = nWarmup,
    thin = thin,
    control = list(adapt_delta = adaptDelta),
    ...)

  # cleanup parallel computing options
  if (parallel == TRUE) {
    options(op)
  }

  # Infos on MCMC chains
  setupMCMC <- data.frame(nIter = nIter,
                          nChains = nChains,
                          thinInterval = thin,
                          nWarmup = nWarmup)

  ## Warnings on fit quality

  outRhat <- rstan::summary(fit)$summary[, "Rhat"]

  if (!all(outRhat < 1.1, na.rm = TRUE)){
    msg <- "
    *** Markov chains did not converge! Do not analyze results! ***.
    Plot MCMC chains and try the following options:
    (1) if one or more chain are a simple stable line, increase 'adapt_delta' (default is 0.95).
    (2) if the variabbility between chain is great, you can increase the number of iteration (default is 2000 iteration).
    (3) if 'Conditional_Psurv_hat' is greater than 1, the ODE integration is wrong. So you can reduce the tolerance of the ODE integrator."
    warning(msg, call. = FALSE)
    print(outRhat)
  } else {
    msg <- "NA"
  }

  # Return
  lsOut <- list(stanFit = fit,
                data = data,
                dataFit = lsFullData,
                setupMCMC = setupMCMC,
                modelType = modelType,
                distribution = ifelse(modelType == "IT", distribution, "NA"),
                messages = msg)
  class(lsOut) <- "beeSurvFit"

  return(lsOut)
}
