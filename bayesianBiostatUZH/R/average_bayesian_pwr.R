#' Average Bayesian Power
#'
#' Bayesian power curve averaged w.r.t. to the supplied prior in a normal-normal model,
#' as derived in Spiegelhalter (2004)
#'
#' @importFrom stats pnorm qnorm
#'
#' @param mu prior mean
#' @param n0 prior sample size
#' @param n sample size
#' @param sd standard deviation
#' @param eps half the type I error, as specified in Spiegelhalter (2004)
#'
#' @author Lucas Kook
#'
#' @examples
#' average_bayesian_pwr(mu = 0.56, n0 = 34, n = 100, sd = 2, eps = 0.025)
#'
#' @export
average_bayesian_pwr <- function(mu, n0, n, sd, eps) {
  stopifnot((sd > 0) & (n > 0) & (n0 > 0) & (eps > 0 & eps < 1))
  zeps <- qnorm(eps)
  qtl <-
    (mu * sqrt(n0 + n) * sqrt(n0)) / (sd * sqrt(n)) + sqrt(n0 / n) *
    zeps
  return(pnorm(qtl))
}
