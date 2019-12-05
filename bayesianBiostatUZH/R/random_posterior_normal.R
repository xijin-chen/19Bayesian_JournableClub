#' Random posterior normal distribution
#' @param y_k Estimated log odds ratio
#' @param s_k Estimated SD
#' @param pooled_posterior_mean see function pooled_posterior_distribution
#' @param K Number of studies included in the meta-analysis
#' @param n_k ffective number of events (2/estimated SD)
#'
#' @return Random posterior normal distribution (mean and variance) for each study included in the meta-analysis
#'
#' @examples random_posterior_distribution(pooled_posterior_mean = -1.0032295,
#' y_k = c(-0.65, -1.02, -1.12), s_k = c(1.06, 0.41, 0.74), K = 3, n_k = c(3.6, 24.3, 7.4))
#'
#' @author Manja Deforth
#' 
#' @export
random_posterior_distribution <-
  function(pooled_posterior_mean, y_k, s_k, K, n_k) {
    Q <-
      sum((y_k - pooled_posterior_mean) ^ 2 / (s_k ^ 2)) # Q = Test for heterogeneity
    N <- sum(n_k)
    tau <- sqrt(Q - (K - 1) / (N - sum(n_k ^ 2) / N))
    B_k <-
      s_k ^ 2 / (s_k ^ 2 + tau ^ 2) # B_k controls the "shrinkage" of the estimate towards the mean and the reduction in the width of the CI-interval
    random_posterior_mean <- B_k * pooled_posterior_mean + (1 - B_k) * y_k
    random_posterior_variance <- (1 - B_k) * s_k ^ 2
    return(c(random_posterior_mean, random_posterior_variance))
  }
