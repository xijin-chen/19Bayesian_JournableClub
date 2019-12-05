#' Hybrid predictive probability
#'
#' The hybrid predictive probability
#' \ifelse{html}{\out{p(S<sub>&epsilon;</sub><sup>C</sup> | y<sub>m</sub>, prior)}}{\eqn{p(S_\epsilon^C | y_m, prior)}}
#' as in Spiegelhalter (2004), describes the probability of getting a significant result
#' in a future classical analysis, using a prior and current data.
#'
#' @param n0 prior sample size
#' @param mu prior mean
#' @param ym data mean
#' @param m data sample size
#' @param n future data sample size
#' @param sd common standard deviation
#' @param eps half the type I error as described in Spiegelhalter (2004)
#'
#' @return Returns the hybrid predictive probability
#' \ifelse{html}{\out{p(S<sub>&epsilon;</sub><sup>C</sup> | y<sub>m</sub>, prior)}}{\eqn{p(S_\epsilon^C | y_m, prior)}}.
#'
#' @references
#' Spiegelhalter, D., Abrams, K. and Myles, J. (2011).
#' Bayesian approaches to clinical trials and health-care evaluation.
#' Chichester: John Wiley & Sons. \cr
#' Chapter 6, equation 6.15.
#'
#' @author Katrin Petermann
#' @examples
#' superior <- hybrid_predictive_probability(n0 = 10, mu = 0, m = 30, ym = 0.6, n = 20, sd = 2, eps = 0.025)
#' inferior <- 1 - hybrid_predictive_probability(n0 = 10, mu = 0, m = 30, ym = 0.6, n = 20, sd = 2, eps = 0.975)
#' equivocal <- 1 - superior - inferior
#' print(rbind(superior, equivocal, inferior))
#'
#' @export
hybrid_predictive_probability <-
  function(n0,
           mu,
           m,
           ym,
           n,
           sd = 2,
           eps = 0.025) {
    stopifnot((n0 >= 0)
              & (n >= 0) & (n %% 1 == 0)
              & (m >= 0) & (m %% 1 == 0)
              & (sd > 0)
              & (eps > 0) & (eps < 1))
    ze <- qnorm(eps)
    x <-
      sqrt(n0 * n / ((n0 + m) * (n0 + m + n))) * sqrt(n0) * mu / sd +
      sqrt(m * (n0 + m + n) / (n * (n0 + m))) * sqrt(m) * ym / sd +
      sqrt((m + n) * (n0 + m) / (n * (n0 + m + n))) * ze
    pred_prob <- pnorm(x)
    return(pred_prob)
  }
