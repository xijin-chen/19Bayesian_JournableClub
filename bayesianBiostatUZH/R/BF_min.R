#' Minimum Bayes Factor
#'
#' Calculate the minimum Bayes Factor as derived in Spiegelhalter (2004)
#' equation 4.4
#'
#'
#'
#' @param z Test Statistic
#'
#' @return Returns the minimum Bayes Factor.
#'
#' @examples
#' BF_min(z=1.96)
#'
#' @author Seraphina Kissling
#'
#' @export
BF_min <- function(z) {
  return(exp((-z^2)/2))
}
