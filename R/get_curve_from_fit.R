#' Test a time series for rhythmicity by harmonic regression method
#'
#' @param fit.x Fit output from f24_R2_cycling()
#' @param tvec Time vector in hours
#' @param period Period of oscillations in hours
#' @return y: Predicted x given fit.x and tvec
#' 
get_curve_from_fit <- function(fit.x, tvec, period = 24){
  jomega <- 2 * pi / period
  y <- fit.x[["mean"]] + (fit.x[["amp"]] / 2) * cos(jomega * (tvec - fit.x[["phase"]]))
  return(y)
}
