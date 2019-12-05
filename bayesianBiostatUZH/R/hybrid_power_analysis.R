#' Full power analysis
#'
#' Full analysis for the normal-normal setup as described in Spiegelhalter (2004)
#' Calculates classical (conditional) power, expected classical, Bayesian and
#' expected Bayesian power, as well as Type I error under the classical and Bayesian
#' power curve. In addition the function also allows for the specification of different
#' alternative hypotheses and contrasts. Note that per default an enthusiastic prior is used.
#'
#' @importFrom stats dnorm
#'
#' @param mu prior mean
#' @param n0 prior sample size
#' @param n sample size
#' @param sd common standard deviation
#' @param eps half the type I error as described in Spiegelhalter (2004)
#' @param conf.level confidence level
#' @param par_range numerical range of the parameters of interest
#' @param theta0 value under the null hypothesis in a one.sample situation (default = 0)
#' @param alternative Either one of \code{"greater.than"} or \code{"less.than"} indicating
#'                    the type of alternative
#' @param type contrast type, either one of \code{"one.sample"} or \code{"two.sample"}
#'
#' @return Returns a list containing all results
#' \itemize{
#'    \item \code{par_vals} Values of theta in range \code{par_range}
#'    \item \code{c.pwr} classical power curve
#'    \item \code{c.pwr_avg} average classical
#'    \item \code{c.Ierror} Type I error to double check correct specification
#'    \item \code{b.pwr} Bayesian power curve
#'    \item \code{b.pwr_avg} average Bayesian power
#'    \item \code{b.Ierror} Bayesian power at theta0
#'    \item \code{prior.dens} Prior density
#' }
#'
#'  @author Lucas Kook
#'
#' @examples
#' res <- hybrid_power_analysis(mu = 0.56, n0 = 34, n = 100,
#'                               sd = 2, eps = 0.025, conf.level = 0.95,
#'                               par_range = c(-1,1.5), theta0 = 0)
#'
#' plot(res$par_vals, res$c.pwr, type = "l")
#' lines(res$par_vals, res$b.pwr)
#' plot(res$par_vals, res$prior.dens, type = "l")
#' res$c.pwr_avg
#' res$b.pwr_avg
#' res$c.Ierror
#' res$b.Ierror
#'
#' @export
hybrid_power_analysis <-
  function(mu,
           n0,
           n,
           sd,
           eps,
           conf.level,
           par_range,
           theta0 = 0,
           alternative = c("greater.than", "less.than"),
           type = c("one.sample", "two.sample")) {
    alt <- match.arg(alternative)
    typ <- match.arg(type)

    if (alt == "less.than")
      eps <- 1 - eps

    if (typ == "two.sample")
      sd <- sqrt(2) * sd

    thetas <- seq(par_range[1], par_range[2], length.out = 1000)

    pwr_curve <-
      classical_pwr(
        theta = thetas - theta0,
        n = n,
        sd = sd,
        eps = eps
      )

    avg_pwr <-
      average_classical_pwr(
        mu = mu - theta0,
        n0 = n0,
        n = n,
        sd = sd,
        eps = eps
      )

    type_I_classical <-
      classical_pwr(
        theta = 0,
        n = n,
        sd = sd,
        eps = eps
      )

    bayesian_pwr_curve <-
      bayesian_pwr(
        theta = thetas - theta0,
        n = n,
        sd = sd,
        eps = eps,
        mu = mu - theta0,
        n0 = n0
      )

    avg_b_pwr <-
      average_bayesian_pwr(
        mu = mu - theta0,
        n0 = n0,
        n = n,
        sd = sd,
        eps = eps
      )

    type_I_bayes <-
      bayesian_pwr(
        theta = 0,
        n = n,
        sd = sd,
        eps = eps,
        mu = mu - theta0,
        n0 = n0
      )

    prior_dens <-
      sapply(thetas - theta0,
             dnorm,
             mean = mu - theta0,
             sd = sd / sqrt(n0))

    cpwr_at_mu <-
      classical_pwr(
        theta = mu - theta0,
        n = n,
        sd = sd,
        eps = eps
      )

    bpwr_at_mu <-
      bayesian_pwr(
        theta = mu - theta0,
        n = n,
        sd = sd,
        eps = eps,
        mu = mu - theta0,
        n0 = n0
      )

    if (alt == "less.than") {
      # Adapt all results to 'less than' alternative
      pwr_curve <- 1 - pwr_curve
      avg_pwr <- 1 - avg_pwr
      type_I_classical <- 1 - type_I_classical
      bayesian_pwr_curve <- 1 - bayesian_pwr_curve
      avg_b_pwr <- 1 - avg_b_pwr
      type_I_bayes <- 1 - type_I_bayes
      cpwr_at_mu <- 1 - cpwr_at_mu
      bpwr_at_mu <- 1 - bpwr_at_mu
    }

    return(
      list(
        par_vals = thetas,
        c.pwr = pwr_curve,
        c.pwr_avg = avg_pwr,
        c.Ierror = type_I_classical,
        b.pwr = bayesian_pwr_curve,
        b.pwr_avg = avg_b_pwr,
        b.Ierror = type_I_bayes,
        prior.dens = prior_dens,
        c.pwr_at_mu = cpwr_at_mu,
        b.pwr_at_mu = bpwr_at_mu
      )
    )
  }
