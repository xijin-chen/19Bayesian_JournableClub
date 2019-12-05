#' Pooled posterior mean from a normal distribution
#' @param y_k Estimated log(odds ratio)
#' @param s_k Estimated SD
#'
#' @return The pooled posterior mean from a normal distribution.
#'
#' @examples
#' pooled_posterior_mean(y_k = c(-0.65, -1.02, -1.12), s_k = c(1.06, 0.41, 0.74))
#' 
#'
#' @author Manja Deforth
#' 
#' @export
pooled_posterior_mean <- function(y_k, s_k) {
	pooled.posterior.mean <- sum(y_k/s_k^2)/sum(1/s_k^2)
	names(pooled.posterior.mean) <- "pooled posterior mean"
	return(pooled.posterior.mean)
}
