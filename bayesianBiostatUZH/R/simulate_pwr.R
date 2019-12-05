#' Power and Sample Size Simulation
#'
#' Simulation function for the normal-normal setup as discussed in section 6.5
#' (Spiegelhalter, 2004). Based on desired power and sample size, sample size and power
#' (respectively) are treated as a random variable and estimated accordingly.
#'
#' @importFrom stats rnorm pnorm qnorm dnorm
#'
#' @param nsim number of simulations
#' @param prior_mu_theta prior mean for theta
#' @param prior_sd_theta prior standard deviation for theta
#' @param prior_mu_sigma prior mean for sigma
#' @param prior_sd_sigma prior standard deviation on sigma
#' @param n planned sample size
#' @param alpha Type I error probability
#' @param pwr desired power
#'
#' @return Returns a \code{data.frame} with four columns:
#'
#' \itemize{
#'   \item \code{theta} Random draw for theta
#'   \item \code{sigma} Random draw for sigma
#'   \item \code{power} Simulated power at theta, sigma and the fixed sample size
#'   \item \code{n} Simulated sample size at theta, sigma and the fixed power
#' }
#'
#' @author Lucas Kook
#'
#' @examples
#' res <- simulate_pwr(nsim = 10000, prior_mu_theta = 0.5,
#'                     prior_sd_theta = 0.1, prior_mu_sigma = 1,
#'                     prior_sd_sigma = 0.3, n = 63, alpha = 0.05,
#'                     pwr = 0.8)
#'
#' @export
simulate_pwr <-
  function(nsim,
           # Number of simulations
           prior_mu_theta,
           # Prior mean for theta
           prior_sd_theta,
           # Prior sd for theta
           prior_mu_sigma,
           # Prior mean for sigma
           prior_sd_sigma,
           # Prior sd for sigma
           n,
           # planned sample size
           alpha,
           # Type I error
           pwr) {
    # Planned power

    stopifnot((prior_mu_sigma > 0) & (prior_sd_sigma > 0))
    stopifnot((n > 0) &
                (0 < alpha) & (alpha < 1) & (pwr > 0) & (pwr < 1))

    cn <-
      function(par) {
        # Local function to be vectorized in the simulation
        2 * par[2] ^ 2 * (qnorm(pwr) - qnorm(alpha / 2)) ^ 2 / par[1] ^ 2
      }

    # Draw nsim values for theta and sigma
    tt <-
      rnorm(n = nsim, mean = prior_mu_theta, sd = prior_sd_theta)
    ss <-
      abs(rnorm(n = nsim, mean = prior_mu_sigma, sd = prior_sd_sigma))

    # Calculate the power and n based on the planned values supplied to the function
    pp <- pnorm(sqrt(n * tt ^ 2 / (2 * ss ^ 2)) + qnorm(alpha / 2))
    nn <- apply(cbind(tt, ss), 1, cn)

    return(data.frame(
      # Output data.frame
      theta = tt,
      # random thetas
      sigma = ss,
      # random sigmas
      power = pp,
      # simulated power
      n = nn # simulated necessary sample size
    ))
  }
