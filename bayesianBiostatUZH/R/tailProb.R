#' tailProb
#'
#' @param threshold threshold for tail probability
#' @param fn the name of a cumulative distribution function
#' @param lower.tail logical indicator whether the lower or upper tail probability is computed
#'
#' @author Leonhard Held
#' @return the tail probability
#'
#' @description  function to compute a tail probability based on a cumulative probability function
#' @examples
#' tailProb(threshold=-2.0, fn=pnorm)
#' tailProb(threshold=-2.0, fn=pnorm, lower.tail=FALSE)
#'
#' @export
tailProb <- function(threshold, fn, lower.tail=TRUE, ...){
  fn1 <- function(par)
  fn(par, lower.tail=lower.tail, ...)
  res <- fn1(threshold)
  names(res) <- "Tail probability"
  return(res)
}
