% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ppc.R
\name{ppc.beeSurvFit}
\alias{ppc.beeSurvFit}
\title{Posterior predictive check method for \code{beeSurvFit} objects}
\usage{
\method{ppc}{beeSurvFit}(x)
}
\arguments{
\item{x}{an object of class \code{beeSurvFit}}
}
\value{
a \code{data.frame} of class \code{ppc}
}
\description{
Posterior predictive check method for \code{beeSurvFit} objects
}
\examples{
data(fitBetacyfluthrin_Chronic)
out <- ppc(fitBetacyfluthrin_Chronic)

}
