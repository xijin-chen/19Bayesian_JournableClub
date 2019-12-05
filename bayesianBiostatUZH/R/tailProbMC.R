#' tailProbMC
#'
#' @param threshold threshold for tail probability
#' @param samples a numerical vector of samples from a dsitribution
#' @param lower.tail logical indicator whether the lower or upper tail probability is computed
#'
#' @author Leonhard Held
#' @return A vector with the empirical tail probability and its Monte Carlo standard error
#'
#' @description  function to compute a tail probability based on Monte Carlo samples
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(1000)
#' tailProbMC(threshold = -2.0, samples = x)
#' tailProbMC(threshold = -2.0, samples = x, lower.tail=FALSE)
#'
#' @export
tailProbMC <- function(threshold, samples, lower.tail=TRUE){
  below <- (samples <= threshold)
  p <- mean(below)
  if(lower.tail==FALSE)
    p <- 1-p
  nsim <- length(samples)
  ## Note: Monte Carlo standard error is the same
  ## whether or not lower.tail==TRUE
  p.se <- sqrt(var(below)/nsim)
  res <- c(p, p.se)
  names(res) <- c("Tail probability", "Monte Carlo standard error")
  return(res)
}
