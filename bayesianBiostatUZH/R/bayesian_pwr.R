#' Bayesian Power
#'
#' Implements the Bayesian power curve as derived in Spiegelhalter (2004), equation 6.6
#'
#' @importFrom stats pnorm qnorm
#'
#' @param theta parameter of interest supplied as a numeric vector
#' @param n sample size
#' @param sd standard deviation
#' @param eps half the type I error as specified in Spiegelhalter (2004)
#' @param mu prior mean
#' @param n0 prior sample size
#'
#' @author Lucas Kook
#'
#' @examples
#' thetas <- seq(-1, 1, length.out = 10)
#' bayesian_pwr(theta = thetas, n = 100, sd = 2, eps = 0.025, mu = 0.56, n0 = 34.5)
#'
#' @export
bayesian_pwr <- function(theta, n, sd, eps, mu, n0) {
  stopifnot((sd > 0) & (n > 0) & (eps > 0 & eps < 1))
  zeps <- qnorm(eps)
  pwr <- sapply(theta, function(t) {
    pnorm(t * sqrt(n) / sd + mu * n0 / (sd * sqrt(n)) + sqrt((n0 + n) / n) *
            zeps)
  })
  return(pwr)
}
