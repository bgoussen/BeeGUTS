#' Predict method for \code{beeSurvFit} objects
#'
#' @description This is the generic \code{predict} S3 method for the \code{beeSurvFit}
#' class. It predict the survival over time for the concentration profiles entered by the user.
#' No concentration reconstructions are performed here. Functions [odeGUTS::predict_ode()]
#' from the \code{morse} package is used. This might be changed in a future update
#'
#' @param object An object of class \code{beeSurvFit}
#' @param dataPredict Data to predict in the format as a dataframe containing the
#' following column:
#' \itemize{
#'     \item \code{time}: A vector of time in days
#'     \item \code{conc}: A vector of number of survivors of same length
#'     \item \code{replicate} A vector replicate name
#' }
#' @param ... Additional arguments to be parsed to the  \code{predict.survFit} method from \code{odeGUTS} (e.g.
#'  \code{mcmc_size = 1000} is to be used to reduce the number of mcmc samples in order to speed up
#'  the computation. \code{mcmc_size} is the number of selected iterations for one chain. Default
#'  is 1000. If all MCMC is wanted, set argument to \code{NULL}.
#'  \code{hb_value  = FALSE} the background mortality \code{hb} is set to a fixed value.
#' If \code{TRUE}, parameter \code{hb} taken from the posterior (only works if
#' one \code{hb} value was estimated. The default is \code{FALSE}.
#'  \code{hb_valueFORCED  = 0} hb_valueFORCED If \code{hb_value} is \code{FALSE}, it fix \code{hb}. The default is \code{0}
#'
#' @return A \code{beeSurvPred} object containing the results of the forwards prediction
#' @export
#'
#' @examples
#' \donttest{
#' dataPredict <- data.frame(time = c(1:5, 1:15),
#'                           conc = c(rep(5, 5), rep(15, 15)),
#'                           replicate = c(rep("rep1", 5), rep("rep2", 15)))
#' data(fitBetacyfluthrin_Chronic)
#' prediction <- predict2(fitBetacyfluthrin_Chronic, dataPredict)
#' }
predict2 <- function(object,
                                dataPredict,
                                userhb_value=0,
                                calib_hb=FALSE,
                                ndatahb=1) {

  # Check for correct class
  if (!is(object,"beeSurvFit")) {
    stop("predict.beeSurvFit: an object of class 'beeSurvFit' is expected")
  }

  if (calib_hb==TRUE) {
      if (object$data$nDatasets>1){
        parhb = paste0("hb_log10[",ndatahb,"]")
      }

      if(object$modelType == "SD"){
        morseObject <- list(mcmc = rstan::As.mcmc.list(object$stanFit, pars = c(parhb, "kd_log10", "zw_log10", "bw_log10")),
                            model_type = object$modelType)
        class(morseObject) <- "survFit"

        for(i in 1:object$setupMCMC$nChains) {
          colnames(morseObject$mcmc[[i]]) <- c("hb_log10", "kd_log10", "z_log10", "kk_log10")
        }
      } else if(object$modelType == "IT") {
        morseObject <- list(mcmc = rstan::As.mcmc.list(object$stanFit, pars = c(parhb, "kd_log10", "mw_log10", "beta_log10")),
                            model_type = object$modelType)
        class(morseObject) <- "survFit"

        for(i in 1:object$setupMCMC$nChains) {
          colnames(morseObject$mcmc[[i]]) <- c("hb_log10", "kd_log10", "alpha_log10", "beta_log10")
        }
      } else {
        stop("Wrong model type. Model type should be 'SD' or 'IT'")
      }

      outMorse <- odeGUTS::predict_ode(morseObject, dataPredict, hb_value = TRUE)
  } else {
      if (length(userhb_value)==1){

        if(object$modelType == "SD"){
          morseObject <- list(mcmc = rstan::As.mcmc.list(object$stanFit, pars = c("kd_log10", "zw_log10", "bw_log10")),
                              model_type = object$modelType)
          class(morseObject) <- "survFit"

          for(i in 1:object$setupMCMC$nChains) {
            colnames(morseObject$mcmc[[i]]) <- c("kd_log10", "z_log10", "kk_log10")
          }
        } else if(object$modelType == "IT") {
          morseObject <- list(mcmc = rstan::As.mcmc.list(object$stanFit, pars = c("kd_log10", "mw_log10", "beta_log10")),
                              model_type = object$modelType)
          class(morseObject) <- "survFit"

          for(i in 1:object$setupMCMC$nChains) {
            colnames(morseObject$mcmc[[i]]) <- c("kd_log10", "alpha_log10", "beta_log10")
          }
        } else {
          stop("Wrong model type. Model type should be 'SD' or 'IT'")
        }

        outMorse <- odeGUTS::predict_ode(morseObject, dataPredict,
                                         hb_value = FALSE,
                                         hb_valueFORCED = userhb_value)


      } else if (length(userhb_value) == 2){
        hbMed_log10  = log10(userhb_value[1])
        hbSD_log10   = abs( log10(userhb_value[1] * (1+userhb_value[2])) - log10(userhb_value[1]) )

        simhb = data.frame(hb_log10 = rnorm(object$setupMCMC$nIter-object$setupMCMC$nWarmup,
                                            hbMed_log10,
                                            hbSD_log10))

        if(object$modelType == "SD"){
          morseObject <- list(mcmc = rstan::As.mcmc.list(object$stanFit, pars = c("kd_log10", "zw_log10", "bw_log10")),
                              model_type = object$modelType)
          class(morseObject) <- "survFit"

          for(i in 1:object$setupMCMC$nChains) {
            morseObject$mcmc[[i]] = cbind(morseObject$mcmc[[i]], simhb$hb_log10)
            colnames(morseObject$mcmc[[i]]) <- c("kd_log10", "z_log10", "kk_log10","hb_log10")
          }
        } else if(object$modelType == "IT") {
          morseObject <- list(mcmc = rstan::As.mcmc.list(object$stanFit, pars = c("kd_log10","mw_log10", "beta_log10")),
                              model_type = object$modelType)
          class(morseObject) <- "survFit"

          for(i in 1:object$setupMCMC$nChains) {
            morseObject$mcmc[[i]] = cbind(morseObject$mcmc[[i]],simhb$hb_log10)
            colnames(morseObject$mcmc[[i]]) <- c("kd_log10", "alpha_log10", "beta_log10","hb_log10")
          }
        } else {
          stop("Wrong model type. Model type should be 'SD' or 'IT'")
        }

        outMorse <- odeGUTS::predict_ode(morseObject, dataPredict,
                                         hb_value = TRUE)

      }
      else {
        stop("Allowed values for userhb_value:
                - single element for hb value
                - array with two elements, one for hb and one for the width of
             its distribution for simulations")
      }

    }
    # Calculate summary to embed mean posteriors values with outputs
    invisible(utils::capture.output(outSummary <- summary(object)))

    # Return
    lsOut <- list(parsPost = outSummary$Qposteriors,
                  modelType = object$modelType,
                  unitData = object$data$unitData,
                  beeSpecies = object$data$beeSpecies,
                  setupMCMC = object$setupMCMC,
                  sim = outMorse$df_quantile
    )
    class(lsOut) <- c("beeSurvPred", class(lsOut))

    return(lsOut)



}

