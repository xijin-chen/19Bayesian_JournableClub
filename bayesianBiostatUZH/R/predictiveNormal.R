#' @description function predictive computes mean and standard deviation of predictive distribution
#' in normal-normal model
#' @author Mark James Thompson
#' @export
predictiveNormal <- function(prior.mean, prior.sd, data.sd){
  prior.var <- prior.sd^2
  data.var <- data.sd^2
  pred.mean <- prior.mean
  pred.var <- prior.var+data.var
  res <- c(pred.mean, sqrt(pred.var))
  names(res) <- c("Predictive mean", "Predictive SD")
  return(res)
}
