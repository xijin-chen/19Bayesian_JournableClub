#' Reversal of hypothesis
#' 
#' This function calculates the probability for the reversal of hypothesis given a
#' function that calculates the predictive probability. It has been implemented to 
#' work with bayesian-, hybrid- and classical-predictive-probability from this package.
#' 
#' @param prob_f function used to calculate the probability
#' @param eps half the type I error as described in Spiegelhalter (2004)
#' @param ... other variables going into prob_f
#' 
#' @return Returns a vector with the results
#' \itemize{
#' \item{superior }{probability for treatment superior}
#' \item{equivocal }{probability for treatments equivocal}
#' \item{inferior }{probability for treatment inferior}
#' }
#' 
#' @references 
#' Spiegelhalter, D., Abrams, K. and Myles, J. (2011). 
#' Bayesian approaches to clinical trials and health-care evaluation. 
#' Chichester: John Wiley & Sons. \cr
#' Chapter 6
#' 
#' @author Katrin Petermann
#' @examples 
#' hypothesis_reversal(classical_predictive_probability, fraction = FALSE, m = 30, ym = 0.6, n = 20, sd = 2, eps = 0.025)
#' 
#' @export
hypothesis_reversal <- function(prob_f, eps, ...){
  stopifnot((eps > 0) & (eps < 1))
  
  s <- prob_f(..., eps = eps)
  i <- 1 - prob_f(..., eps = 1-eps)
  e <- 1 - s - i
  
  return(c(superior = s, equivocal = e, inferior = i))
}
