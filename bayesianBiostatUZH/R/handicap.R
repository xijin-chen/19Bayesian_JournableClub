#' handicap
#' The function is used to calculate n0/n when using a sceptical prior
#'
#'@param alpha <size of a trial>
#'@param beta < power is equal to 1 - beta>
#'@param gamma < quantile of expected value of probability equal or above alternative hypothesis>
#'@return <handicap>
#'
#' @examples
#' # In a trial designed with 5% size and 90% power, such a sceptical prior corresponds to adding a
#' # "handicap" equivalent to already having run a "pseudotrial" with no observed treatment difference,
#' # and which contains around 26% of the porposed sample size/
#' handicap(0.05, 0.1, 0.05)
#' @author Natalia Popova
#' @export
handicap <- function (alpha, beta, gamma) {
  out <- ( qnorm(1-gamma)/(qnorm(1-alpha/2)+qnorm(1-beta)) )^2
  return(out)
}

