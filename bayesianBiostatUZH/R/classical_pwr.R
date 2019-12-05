#' Classical power
#'
#' Conditional power curve for normally distributed outcomes
#' and one sided hypotheses as derived in Spiegelhalter (2004)
#' equation 6.3
#'
#' @importFrom stats pnorm qnorm
#'
#' @param theta Parameter values supplied as a numeric vector
#' @param n Sample size
#' @param sd Standard deviation
#' @param eps Half of the significance level alpha as specified in Spiegelhalter(2004)
#'
#' @author Lucas Kook
#'
#' @examples
#' thetas <- seq(-1, 1, length.out = 10)
#' classical_pwr(theta = thetas, n = 100, sd = 2, eps = 0.025)
#'
#' @export
classical_pwr <- function(theta, n, sd, eps) {
  stopifnot((sd > 0) & (n > 0) & (eps > 0 & eps < 1))
  zeps <- qnorm(eps)
  pwr <- sapply(theta, function(t) {
    pnorm(t * sqrt(n) / sd + zeps)
  })
  return(pwr)
}
