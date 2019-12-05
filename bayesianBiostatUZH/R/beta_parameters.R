#' Parameters of a beta distribution
#'
#' @param m Mean of the distribution
#' @param s Standard deviation of the distribution
#' @return Returns the parameters a, b of the beta distribution with mean m and standard deviation s
#' @examples
#' beta_parameters(m=0.4, s=0.1)
#' @author Chiara Vanetta
#' @export
beta_parameters <- function(m, s) {
  stopifnot((m > 0) & (s > 0))
  a <- m ^ 2 * (1 - m) / s ^ 2 - m
  b <- a * (1 - m) / m
  return(c(a, b))
}
