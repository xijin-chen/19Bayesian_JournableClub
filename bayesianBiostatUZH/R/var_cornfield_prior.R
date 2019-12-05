#' variance of a cornfield prior
#' Prior for alternative hypothesis which "smears" along all the possible values of parameter
#' It is a "smear" part of "lump and smear" approach for defining a prior for null and alternative hypothesis together.
#' In this approach point mass probability for the value of null hupothesis is assigned (p) and continious probability
#' for all the other values of a parameter of interest is integrated to 1-p.
#'
#'Cornfiend suggesta a "default" prior under the alternative as a normal distribution centred on the null
#'hypothesis and with expectatioon (conditional on the effect of being positive) equal to alternative hypothesis
#'
#'@param theta_a <value of parameter of interest for alternative hypothesis>
#'@return <variance for cornfield prior>
#'
#' @examples
#' # Full example is described in Spiegelhalter (2004), chapter 5, example 5.4
#' # If we take 8 as a value of theta of alternative hupothesis then:
#' var_cornfield_prior(8)
#' @author Natalia Popova
#' @export
var_cornfield_prior <- function (theta_a) {
  out <- (theta_a ^ 2) * pi / (2)
  return(out)
}
