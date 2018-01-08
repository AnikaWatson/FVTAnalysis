#' TrigModel
#'
#' Fits a series of trig functions to data
#'
#' @param M Data of type numeric.
#' @param X The x-values corresponding to the data.
#' @param N The precision of the fit. When N=2 the second harmonic is added, and so on and so on.
#'
#' @return A function that fits the data.
#'
#' @export

TrigModel <- function (M, X, N){
  trigfun <- rep(NA, N)
  for (i in 1:N) {
    trigfun[i] <- sin((2*i)*pi/per*X)+cos((2*i)*pi/per*X)
  }
  lm(M ~ sum(trigfun))
}
