% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/validateBeeGUTS.R
\name{validate}
\alias{validate}
\title{Validation method for \code{beeSurvFit } objects}
\usage{
validate(object, dataValidate, ...)
}
\arguments{
\item{object}{An object of class \code{beeSurvFit}}

\item{dataValidate}{Data to validate in the format of the experimental data used for fit (dataGUTS)}

\item{...}{Additional arguments to be parsed to the  \code{predict.survFit} method from \code{odeGUTS} (e.g.
\code{mcmc_size = 1000} is to be used to reduce the number of mcmc samples in order to speed up
the computation. \code{mcmc_size} is the number of selected iterations for one chain. Default
is 1000. If all MCMC is wanted, set argument to \code{NULL}.,
\code{hb_value  = FALSE} the background mortality \code{hb} is taken into account from the posterior.
If \code{FALSE}, parameter \code{hb} is set to a fixed value. The default is \code{FALSE}.
\code{hb_valueFORCED  = 0} hb_valueFORCED If \code{hb_value} is \code{FALSE}, it fix \code{hb}. The default is \code{0}}
}
\value{
An object of class \code{beeSurvValidation}.
}
\description{
This is a \code{validation} method for the
\code{beeSurvFit} object. It perform forwards predictions for a specific concentration
profile and compare these prediction to the respective experimental data.
}
