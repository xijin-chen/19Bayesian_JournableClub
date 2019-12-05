#' Bayes Factor based on the Type I and II error rates
#'
#' Calculate the Bayes Factor based on the Type I and II error rates as derived in Spiegelhalter (2004)
#' equation 4.1
#'
#' @param a Type I error alpha
#' @param b Type II error beta
#'
#' @return Returns the Bayes Factor based on the Type I and II error rates.
#'
#' @examples
#' BF_alpha_beta(a=0.05, b=0.1)
#'
#' @author Seraphina Kissling
#'
#' @export
BF_alpha_beta <- function(a,b) {
  if( a<0 || a>1 || b<0 || b>1) stop("a and b have to be in [0,1]")
  return(a/(1-b))
}
