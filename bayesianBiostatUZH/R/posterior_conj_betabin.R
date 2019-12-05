#' Parameters of the posterior beta distribution, given a beta prior and a binomial likelihood
#'
#' @param a First parameter of the beta prior
#' @param b Second parameter of the beta prior
#' @param r Number of successes observed
#' @param n Number of cases observed
#' @return Returns the parameters a, b of the posterior beta distribution
#' @examples posterior_conj_betabin(a=9.2, b=13.8, r=15, n=20)
#' @author Chiara Vanetta
#' @export
posterior_conj_betabin <- function(a, b, r, n) {
  stopifnot((a > 0) & (b > 0) & (r >= 0) & (n > 0))
  newa <- a + r
  newb <- b + n - r
  return(c(newa, newb))
}
