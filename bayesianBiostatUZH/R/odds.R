#' Probability to odds
#'
#' @param prob Requires a probability, which is a double precision numeric between 0 and 1, inclusive.
#' @author Leonhard Held
#' @return Returns an odds
#'
#' @examples
#' odds(0.5)
#' # Returns 1:1 even odds
#' @export
odds <- function(prob) {
  if( (prob <= 1) && (0 <= prob) ) {
    return(prob/(1-prob))
  } else {
    stop("Error: please enter a probability between 0 and 1")
  }
}
