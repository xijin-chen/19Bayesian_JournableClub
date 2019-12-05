#' Bayes Factor for composite hypotheses for normal distributions
#'
#' Calculate the Bayes Factor for composite hypotheses for normal distributions as derived in Spiegelhalter (2004)
#' equation 4.5
#'
#'
#'
#' @param m Sample size
#' @param n Sample size of the prior
#' @param z Test Statistic
#'
#' @return Returns the Bayes Factor for composite hypotheses for normal distributions.
#'
#' @examples
#' BF_normal_composite(m=10, n=1, z=1.96)
#'
#' @author Seraphina Kissling
#'
#' @export
BF_normal_composite <- function(m,n,z) {
  if(m<0 || n<0) stop("m and n have to be positive")
  return(sqrt(1 + (m/n)) * exp(-(z^2)/(2*(1 + (n/m)))))
}
