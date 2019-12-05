#' Odds to probability
#' @param odds An odds, which is a positive double precision numeric.
#' @return Returns the equivalent probability
#' @author Leonhard Held
#' @examples
#' prob(1)
#'
#' @export
prob <- function(odds) {
  stopifnot(odds>0)
  return(odds/(1+odds))
}
