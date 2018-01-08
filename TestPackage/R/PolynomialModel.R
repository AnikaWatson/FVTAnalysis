#' PolyModel
#'
#' Fits a series of polynomials to data
#'
#' @param M Data of type numeric.
#' @param X The x-values corresponding to the data.
#' @param N The order of the highest term in the series.
#'
#' @return A function that fits the data.
#'
#' @export

PolyModel <- function (M, X, N){
  lm(M ~ poly(x = X, n = N))
}



