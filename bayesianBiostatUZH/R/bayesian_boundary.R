#' Bayesian monitoring boundary
#' 
#' The Bayesian monitoring boundary used in interim analysis for RCTs 
#' is a function of the proportion m/n of the trial completed.
#' 
#' @param n planned total sample size
#' @param m data sample size
#' @param handicap handicap as described in Spiegelhalter (2004)
#' @param conf.level confidence level
#' 
#' @return Returns the boundary value. If the actual test statistic calculated 
#' from collected data is larger than the boundary value, we might consider 
#' stopping the trial early.
#' 
#' @references 
#' Spiegelhalter, D., Abrams, K. and Myles, J. (2011). 
#' Bayesian approaches to clinical trials and health-care evaluation. 
#' Chichester: John Wiley & Sons. \cr
#' Chapter 6, equation 6.11.
#' 
#' @author Katrin Petermann
#' @examples 
#' n <- 1000
#' m <- seq(0, 1000, by = 1)
#' b <- lapply(m, FUN = bayesian_boundary, n = n)
#' plot(x = m/n, y = b, type = "l",  ylim = c(0, 5),
#' xlab = "Proportion of trial completed",
#' ylab = "Classical standardised test statistic Z")
#' 
#' 
#' @export
bayesian_boundary <- function(n, m, handicap = 0.26, conf.level = 0.05){
  stopifnot((n > 0) & (n%%1 == 0) 
            & (m >= 0) & (m%%1 == 0) 
            & (handicap > 0) 
            & (conf.level > 0) & (conf.level < 1))
  
  ze <- qnorm(conf.level/2)
  
  critical_value <- -ze * sqrt(1 + handicap * n / m)
  
  return(critical_value)
}