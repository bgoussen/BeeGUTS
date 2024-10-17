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
#' @importFrom Rcpp loadModule
#' @import rstantools
#' @import RcppParallel
#' @importFrom foreach %dopar%
#' @importFrom rstan sampling
#' @importFrom magrittr %>%
#' @importFrom stats runif
#' @importFrom stats rnorm
#' @importFrom utils combn
#'
#' @references
#' Baas, J., Goussen, B., Taenzler, V., Roeben, V., Miles, M., Preuss,
#' T.G., van den Berg, S. and Roessink, I. (2024), Comparing Sensitivity
#' of Different Bee Species to Pesticides: A TKTD modeling approach.
#' Environ Toxicol Chem, 43: 1431-1441.
#' \doi{10.1002/etc.5871}
#'
#' Baas, J., Goussen, B., Miles, M., Preuss, T.G., Roessing, I. (2022).
#' BeeGUTS—A Toxicokinetic–Toxicodynamic Model for the Interpretation and
#' Integration of Acute and Chronic Honey Bee Tests.
#' \doi{10.1002/etc.5423}
#'
#' Jager, T., Albert, C., Preuss, T.G. and Ashauer, R. (2011). General Unified
#' Threshold model of Survival - a toxicokinetic-toxicodynamic framework for ecotoxicology.
#' \doi{10.1021/es103092a}
#'
#' Jager, T. and Ashauer, R. (2018). Modelling survival under chemical stress.
#' A comprehensive guide to the GUTS framework. Version 1.0
#' \url{https://leanpub.com/guts_book}
#'
#' EFSA PPR Scientific Opinion (2018). Scientific Opinion on the state of the
#' art of Toxicokinetic/Toxicodynamic (TKTD) effect models for regulatory risk
#' assessment of pesticides for aquatic organisms.
#' \url{https://www.efsa.europa.eu/en/efsajournal/pub/5377}
#'
#' EFSA (European Food Safety Authority), Adriaanse P, Arce A, Focks A,
#' Ingels B, Jölli D, Lambin S, Rundlöf M, Süßenbach D, Del Aguila M, Ercolano V,
#' Ferilli F, Ippolito A, Szentes Cs, Neri FM, Padovani L, Rortais A,
#' Wassenberg J and Auteri D, (2023). Revised guidance on the risk assessment
#' of plant protection products on bees (Apis mellifera, Bombus spp. and
#' solitary bees). EFSA Journal \doi{10.2903/j.efsa.2023.7989}
#'
#' Stan Development Team (2020). RStan: the R interface to Stan. R package
#' version 2.21.2.
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
#' \item{\code{nDatasets}}{An integer representing the number of datasets used.}
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
#' \item{\code{messages}}{A data frame containing the warning messages returned by the function.}
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
