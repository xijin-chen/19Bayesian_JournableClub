#' Pooled posterior SD from a normal distribution
#' @param s_k Estimated SD
#'
#' @return The pooled posterior SD from a normal distribution.
#'
#' @examples
#' pooled_posterior_SD(s_k = c(1.06, 0.41, 0.74))
#'
#'
#' @author Manja Deforth
#'
#' @export
pooled_posterior_sd <- function(s_k) {
	pooled.posterior.variance <- 1/sum(1/s_k^2)
	pooled.posterior.sd <- sqrt(pooled.posterior.variance)
	names(pooled.posterior.sd) <- "pooled posterior SD"
	return(pooled.posterior.sd)
}

