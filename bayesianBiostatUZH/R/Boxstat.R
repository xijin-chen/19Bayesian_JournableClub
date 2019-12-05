#' Box statistics is used to measure the conflict between prior and data
#' Prior for alternative hypothesis which "smears" along all the possible values of paramete
#'
#' @param
#' obs_theta   observed value of a parameter from the experiment
#' mu_pred   is a mean of predictive normal distribution
#' sd_pred   is a standard deviation of predictive normal distribution
#'
#' @return <Box statistics>
#'
#' @author Natalia Popova
#' 
#' @examples
#' # Full example is described in Spiegelhalter (2004), chapter 5, example 5.5
#' Boxstat (-0.74, -0.26, 0.385)
#'
#' @export
Boxstat <- function (obs_theta, mu_pred, sd_pred) {
  stopifnot((sd_pred > 0))
  out <-
    2 * min(pnorm((obs_theta - mu_pred) / sd_pred), 1 - pnorm((obs_theta + mu_pred) /
                                                                sd_pred))
  return(out)
}
