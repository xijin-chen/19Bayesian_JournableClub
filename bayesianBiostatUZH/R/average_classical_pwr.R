#' Average classical (hybrid) power
#'
#' Calculates the expected classical power based on a normal prior
#' as presented in equation 6.4 (Spiegelhalter 2004)
#'
#' @importFrom stats pnorm qnorm
#'
#' @param mu prior mean
#' @param n0 prior sample size
#' @param n sample size
#' @param sd standard deviation
#' @param eps half of the significance level alpha for a two-sided test
#'
#' @author Lucas Kook
#'
#' @examples
#' average_classical_pwr(mu = 0.5, n0 = 34, n = 100, sd = 2, eps = 0.025)
#'
#' @export
average_classical_pwr <- function(mu, n0, n, sd, eps) {
  stopifnot((sd > 0) & (n > 0) & (n0 > 0) & (eps > 0 & eps < 1))
  zeps <- qnorm(eps)
  qtl <- sqrt(n0 / (n0 + n)) * ((mu * sqrt(n)) / sd + zeps)
  return(pnorm(qtl))
}
