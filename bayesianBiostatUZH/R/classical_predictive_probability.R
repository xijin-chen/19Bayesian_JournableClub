#' Classical predictive probability
#'
#' The classical predictive probability
#' \ifelse{html}{\out{p(S<sub>&epsilon;</sub><sup>C</sup> | y<sub>m</sub>)}}{\eqn{p(S_\epsilon^C | y_m)}}
#' as in Spiegelhalter (2004), describes the probability of getting a significant result
#' in a future classical analysis, using only current data.
#'
#' @param fraction if FALSE, the classical predictive probability is calculated using data
#' mean, sample size and standard deviation. If TRUE, only the fraction of the trial completed
#' and the current test statistic have to be provided.
#' @param f fraction of the study completed
#' @param zm current test statistic
#' @param ym data mean
#' @param m data sample size
#' @param n future data sample size
#' @param sd common standard deviation
#' @param eps half the type I error as described in Spiegelhalter (2004)
#'
#' @return Returns the classical predictive probability
#' \ifelse{html}{\out{p(S<sub>&epsilon;</sub><sup>C</sup> | y<sub>m</sub>)}}{\eqn{p(S_\epsilon^C | y_m)}}
#'
#' @references
#' Spiegelhalter, D., Abrams, K. and Myles, J. (2011).
#' Bayesian approaches to clinical trials and health-care evaluation.
#' Chichester: John Wiley & Sons. \cr
#' Chapter 6, equations 6.19 and 6.20.
#'
#' @author Katrin Petermann
#' @examples
#' # using the function with fraction = TRUE
#' zm <- seq(-1, 3, len = 1000)
#' p10 <- lapply(zm, FUN = classical_predictive_probability, fraction = TRUE, f = 0.1, eps = 0.01)
#' plot(x = zm, y = p10, type = "l", ylim = c(0, 1),
#' xlab = "observed z statistic", ylab = "classical predictive probability")
#'
#' # using the function with fraction = FALSE
#' superior <- classical_predictive_probability(fraction = FALSE, m = 30, ym = 0.6, n = 20, sd = 2, eps = 0.025)
#' inferior <- 1 - classical_predictive_probability(fraction = FALSE, m = 30, ym = 0.6, n = 20, sd = 2, eps = 0.975)
#' equivocal <- 1 - superior - inferior
#' print(rbind(superior, equivocal, inferior))
#'
#'
#' @export
classical_predictive_probability <-
  function(fraction = FALSE,
           f,
           zm,
           ym,
           m,
           n,
           sd = 2,
           eps = 0.025) {
    stopifnot((eps > 0) & (eps < 1))

    ze <- qnorm(eps)

    if (fraction) {
      stopifnot((f >= 0) & (f < 1))

      x <- (zm + sqrt(f) * ze) / sqrt(1 - f)
      pred_prob <- pnorm(x)

    } else {
      stopifnot((n >= 0) & (n %% 1 == 0)
                & (m >= 0) & (m %% 1 == 0)
                & (sd > 0))

      x <-
        sqrt(m + n) / sqrt(n) * sqrt(m) * ym / sd + sqrt(m / n) * ze
      pred_prob <- pnorm(x)
    }
    return(pred_prob)
  }
