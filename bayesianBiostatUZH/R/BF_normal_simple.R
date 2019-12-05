#' Bayes Factor for simple hypotheses for normal distributions
#'
#' Calculate the Bayes Factor for simple hypotheses for normal distributions as derived in Spiegelhalter (2004)
#' equation 4.2. Theta is zero under the null hypothesis.
#'
#'
#'
#' @param m Sample size
#' @param t Mean under the alternative hypothesis
#' @param v Variance
#' @param y Sample mean
#'
#' @return Returns the Bayes Factor for simple hypotheses for normal distributions.
#'
#' @examples
#' BF_normal_simple(m=31, t=2.3, v=2, y=2.1)
#'
#' @author Seraphina Kissling
#'
#' @export
BF_normal_simple <- function(m,t,v,y) {
  if(m<0 || v<0) stop("m and v have to be positive")
  return(exp(-(m*t)/v * (y - (t/2))))
}
