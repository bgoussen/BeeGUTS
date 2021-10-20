#' 'BeeGUTS' package; a package to perform GUTS modelling for Bee experiments.
#'
#' @description Provide tools to analyse the survival toxicity tests performed for
#' bee species. It can be used to fit a Toxicokinetic-Toxicodynamic (TKTD) model
#' adapted for bee standard studies (acute oral, acute contact, and chronic oral studies).
#' The TKTD model used is the General Unified Threshold model of Survival (GUTS).
#'
#' The package follows the concept and assumptions presented in Baas et al (submitted)
#'
#' @docType package
#' @name BeeGUTS-package
#' @aliases BeeGUTS
#' @useDynLib BeeGUTS, .registration = TRUE
#' @import methods
#' @import Rcpp
#' @importFrom rstan sampling
#' @importFrom magrittr %>%
#'
#' @references
#' Baas, J., Goussen, B., Miles, M., Preuss, T.G., Roessing, I. (submitted).
#' BeeGUTS – new integrative TKTD model for honey bees approach moving from single point estimates of toxicity and exposure to a holistic link between exposure and effect.
#'
#' Jager, T., Albert, C., Preuss, T.G. and Ashauer, R. (2011). General Unified Threshold model of Survival - a toxicokinetic-toxicodynamic framework for ecotoxicology.
#' \doi{10.1021/es103092a}
#'
#' Jager, T. and Ashauer, R. (2018). Modelling survival under chemical stress. A comprehensive guide to the GUTS framework. Version 1.0
#' \url{https://leanpub.com/guts_book}
#'
#' EFSA PPR Scientific Opinion (2018). Scientific Opinion on the state of the art of Toxicokinetic/Toxicodynamic (TKTD) effect models for regulatory risk assessment of pesticides for aquatic organisms.
#' \url{https://www.efsa.europa.eu/en/efsajournal/pub/5377}
#'
#' Stan Development Team (2020). RStan: the R interface to Stan. R package version 2.21.2.
#' \url{https://mc-stan.org}
NULL



#' Survival datasets for \emph{Honey bees} exposed to
#' constant concentration of Betacyfluthrin for 10 days.
#'
#' @name betacyfluthrinChronic
#' @docType data
#' @usage data(betacyfluthrinChronic)
#' @format A list of class \code{beeSurvData} constructed by \code{dataGUTS} containing:
#' \describe{
#' \item{\code{survData}}{A data frame containing the survival information over time
#' for five treatments and a control in a wide format.}
#' \item{\code{survData_long}}{A data frame containing the survival information over time
#' for five treatments and a control in a long format.}
#' \item{\code{concData}}{A data frame containing the concentration information over time
#' for five treatments and a control in a wide format.}
#' \item{\code{concData_long}}{A data frame containing the concentration information over time
#' for five treatments and a control in a long format.}
#' \item{\code{unitData}}{A character string containing the units of the concentration data.}
#' \item{\code{typeData}}{A character string containing the type of data (here Chronic_Oral).}
#' \item{\code{beeSpecies}}{A character string containing the species of bee of interest (here Honey_Bee).}
#' \item{\code{concModel}}{A data frame containing the concentration information recalculated
#' for the species of bee and test type of interest in a wide format.}
#' \item{\code{concModel_long}}{A data frame containing the concentration information recalculated
#' for the species of bee and test type of interest in a long format.}
#' }
#' @references Bayer data.
#' @keywords dataset
NULL




#' Model calibration results datasets for \emph{Honey bees} exposed to
#' constant concentration of Betacyfluthrin for 10 days.
#'
#' @name fitBetacyfluthrin_Chronic
#' @docType data
#' @usage data(fitBetacyfluthrin_Chronic)
#' @format A list of class \code{beeSurvFit} constructed by \code{fitBeeGUTS} containing:
#' \describe{
#' \item{\code{stanFit}}{A 'stanfit'  object containing the results of the calibration.}
#' \item{\code{data}}{A 'beeSurvData' objects with the user data used for the calibration.}
#' \item{\code{dataFit}}{A list containing the priors and data formatted for the calibration algorithm.}
#' \item{\code{setupMCMC}}{A list containing the setup used for the MCMC.}
#' \item{\code{modelType}}{A character string containing the type of GUTS model used (here 'SD').}
#' \item{\code{distribution}}{A character string containing the distribution used (IT only, here 'NA').}
#' \item{\code{messages}}{A character string containing the error messages if Rhat >1.1 (here 'NA').}
#' }
#' @references Bayer data.
#' @keywords dataset
NULL
