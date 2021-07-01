# Workaround to get rid of "No visible binding for global variable" notes 
# in package check. This notes are caused by uses of dplyr and tidyr. 
if(getRversion() >= "2.15.1")  utils::globalVariables(c(
  "conc", "Treatment", "SurvivalTime", "idAll", "NSurv", "simQ50", "simQinf95",
  "simQsup95", "time", "q50", "qinf95", "qsup95", "Nsurv_q50_valid",
  "Nsurv_qinf95_valid", "Nsurv_qsup95_valid"))