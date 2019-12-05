#' variance of a sceptical prior
#' The function is used to calculate the variance of a  sceptical prior if we know value of parameter
#' for an alternative hypotheis. Sceptical prior is assumed to be centered around 0 (no effect)
#'
#'
#'@param theta_a <value of parameter of interest for alternative hypothesis>
#'@param z < quantile of expected value of probability equal or above alternative hypothesis>
#'@return <variance for sceptical prior (sigma^2/n0)>
#'
#' @examples
#' # Full example is described in Spiegelhalter (2004), chapter 5, example 5.3
#' # value of parameter under alternative hypothesis is -0.31
#' # z = 1.65 means prior will show 5% chance of being less that value of parameter under alternative hypothesis
#' var_scept_prior(-0.31, 1.65)
#' @author Natalia Popova
#' @export
var_scept_prior <- function (theta_a, z) {
  out <- (theta_a ^ 2) / (z ^ 2)
  return(out)
}
