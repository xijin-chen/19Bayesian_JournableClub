#' @description  Computes Wald confidence interval based on estimate and standard error
#' @export
#' @author Mark James Thompson

confidenceInterval <- function(mean, sd, level=0.95){
  z <- qnorm((1+level)/2)
  lower <- mean - z*sd
  upper <- mean + z*sd
  res <- c(lower, upper)
  names(res) <- c("Lower limit", "Upper limit")
  return(res)
}
