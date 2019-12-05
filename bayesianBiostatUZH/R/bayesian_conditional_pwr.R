#' Bayesian conditional power
#'
#' The Bayesian conditional power
#' \ifelse{html}{\out{P(S<sub>&epsilon;</sub><sup>B</sup> | y<sub>m</sub>, &theta;)}}{\eqn{P(S_\epsilon^B | y_m, \theta)}}
#' as in Spiegelhalter (2004), calculates the power, conditional on current data and \ifelse{html}{\out{&theta;}}{\eqn{\theta}}.
#'
#' @param n0 prior sample size
#' @param mu prior mean
#' @param ym data mean
#' @param m data sample size
#' @param n future data sample size
#' @param theta parameter of interest supplied as a numeric vector
#' @param sd common standard deviation
#' @param eps half the type I error as described in Spiegelhalter (2004)
#'
#' @return Returns the Bayesian conditional power
#' \ifelse{html}{\out{P(S<sub>&epsilon;</sub><sup>B</sup> | y<sub>m</sub>, &theta;)}}{\eqn{P(S_\epsilon^B | y_m, \theta)}}.
#'
#' The implemented formula can be thought of as a general form of other power curves:
#' \itemize{
#' \item{if \ifelse{html}{\out{n<sub>0</sub> = 0}}{\eqn{n_0 = 0}} we have no prior input and we obtian the classical conditional power curve.}{}
#' \item{if \ifelse{html}{\out{m = 0}}{\eqn{m = 0}} we obtain the Bayesian power curve.}{}
#' \item{if \ifelse{html}{\out{n<sub>0</sub> = 0}}{\eqn{n_0 = 0}} and \ifelse{html}{\out{m = 0}}{\eqn{m = 0}} we obtian the standard power curve.}{}
#' }
#'
#' @references
#' Spiegelhalter, D., Abrams, K. and Myles, J. (2011).
#' Bayesian approaches to clinical trials and health-care evaluation.
#' Chichester: John Wiley & Sons. \cr
#' Chapter 6, equation 6.17.
#'
#' @author Katrin Petermann
#' @examples
#' theta <- seq(-1, 3, len = 1000)
#' cond_pwr <- sapply(theta, FUN = bayesian_conditional_pwr,
#' n0 = 10, mu = -0.2, m = 30, ym = 0, n = 20, eps = 0.025)
#' plot(theta, cond_pwr, type = "l")
#'
#'
#' @export
bayesian_conditional_pwr <-
  function(n0,
           mu,
           m,
           ym,
           n,
           theta,
           sd = 2,
           eps = 0.025) {
    stopifnot((n0 >= 0)
              & (n >= 0) & (n %% 1 == 0)
              & (m >= 0) & (m %% 1 == 0)
              & (sd > 0)
              & (eps > 0) & (eps < 1))

    ze <- qnorm(eps)
    x <-
      sqrt(n) * theta / sd + m * ym / (sd * sqrt(n)) +
        n0 * mu / (sd * sqrt(n)) + sqrt((n0 + m + n) / n) * ze
    cond_pwr <- pnorm(x)
    return(cond_pwr)
  }
