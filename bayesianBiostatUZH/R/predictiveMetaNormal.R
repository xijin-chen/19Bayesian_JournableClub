#' @description function pred computes mean and std of predictive distribution x and s: means and stds of trial(s)
#' tau: heterogeneity of trial(s)
#' @author Mark James Thompson
#' data.sd: predictive standard deviation
#' @export
predictiveMetaNormal <- function(x, s, tau, data.sd=tau){
w <- 1/(s^2+tau^2)
mean <- sum(w*x)/sum(w)
std <- sqrt(1/sum(w)+data.sd^2)
res <- c(mean, std)
names(res) <- c("Predictive Mean", "Predictive SD")
return(res)
}
