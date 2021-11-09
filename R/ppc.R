#' Generates an object to be used in posterior predictive check for \code{beeSurvFit}, \code{beeSurvPred}
#'
#' @param x  an object used to select a method \code{ppc}
#'
#' @export
#'
ppc <- function(x){
  UseMethod("ppc")
}



#' Posterior predictive check method for \code{beeSurvFit} objects
#'
#' @param x an object of class \code{beeSurvFit}
#'
#'
#' @return a \code{data.frame} of class \code{ppc}
#'
#' @examples
#' @export
#'
ppc.beeSurvFit <- function(x){
  NsurvPred_all<- as.data.frame(x$stanFit, pars = "Nsurv_ppc")
  NsurvPred_quantiles<- NsurvPred_all%>%
    tidyr::pivot_longer(cols = tidyr::starts_with('Nsurv'),
                        names_to = "ppc",
                        values_to = "value")%>%
    dplyr::group_by(ppc)%>%
    dplyr::summarise(median = stats::quantile(value,  0.5, na.rm = TRUE),
                     q_0.025=stats::quantile(value,  0.025, na.rm = TRUE),
                     q_0.975=stats::quantile(value,  0.975, na.rm = TRUE))

  NsurvData_all<- data.frame(value=x$dataFit$Nsurv, id=seq(1,x$dataFit$nData_Nsurv, 1))%>%
    dplyr::mutate(ppc=paste0("Nsurv_ppc[",id, "]"))

  Nsurv_ppc<- dplyr::full_join( NsurvPred_quantiles,  NsurvData_all, by="ppc")%>%
    dplyr::mutate(col=ifelse(value<q_0.025|value>q_0.975, "red", "green")) %>%
    dplyr::arrange(id)

  Nsurv_ppc$data<-"Survival"

  class(Nsurv_ppc) <- c("ppc", class(Nsurv_ppc))
  return(Nsurv_ppc)
}
