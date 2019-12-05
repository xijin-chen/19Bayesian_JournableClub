#' Mean, median and mode of a beta distribution
#'
#' @param a First parameter of the beta distribution
#' @param b Second parameter of the beta distribution
#' @return Returns mean, median and mode of the beta distribution with parameters a and b
#' @examples
#' beta_mean_median_mode(a=2.5, b=1.5)
#' @author Chiara Vanetta
#' @export
beta_mean_median_mode <- function(a, b) {
    mean <- a / (a + b)
    median <- qbeta(0.5, a, b)
    mode <- (a - 1) / (a + b - 2)
    return(c(mean, median, mode))
}
