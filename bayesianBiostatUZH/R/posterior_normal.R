#' Posterior moments in a normal-conjugate model
#' @description Computes the posterior moments of a normal conjugate. If the likelihood
#' is biased this can be modelled with a normal distribution of the bias
#' @param prior.mean : Mean of prior
#' @param prior.sd : Error/sd of the prior
#' @param estimate.mean : Estimate of the prior from the data
#' @param estimate.se : Estimate of the standard error from the data
#' @param bias.mean : Mean of the bias
#' @param bias.sd : Standard deviation of the bias
#'
#' @author Mark James Thompson
#'
#' @export
posterior_normal <-
  function(prior.mean = 0,
           prior.sd = 1,
           estimate.mean = 0,
           estimate.se = 1,
           bias.mean = 0,
           bias.sd = 0) {
    prior.var <- prior.sd ^ 2
    prior.mean <- prior.mean
    estimate.var <- estimate.se ^ 2 + bias.sd ^ 2
    estimate.mean <- estimate.mean - bias.mean
    post.var <- 1 / (1 / prior.var + 1 / estimate.var)
    post.mean <-
      (prior.mean / prior.var + estimate.mean / estimate.var) * post.var
    res <- c(post.mean, sqrt(post.var))
    names(res) <- c("Posterior mean", "Posterior SD")
    return(res)
  }
