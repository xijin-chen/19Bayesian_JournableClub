library(tidyverse)
library(bayesianBiostatUZH)

# Function Section --------------------------------------------------------

calc_n0 <- function(mu, sd, conf.level) {
  stopifnot((sd > 0) & (conf.level > 0 & conf.level < 1))
  n0 <- qnorm(conf.level) ^ 2 * sd ^ 2 / mu ^ 2
  return(n0)
}

calc_n <- function(theta, sd, alpha, pwr) {
  n <- 2 * sd ^ 2 * (qnorm(pwr) - qnorm(alpha / 2)) ^ 2 / theta ^ 2
  return(n)
}

## ----fig6_5, echo=FALSE, warning=FALSE, error=FALSE, message=FALSE, fig.height=4, fig.width=7----
# Example 6.5, p. 200 (Spiegelhalter 2006)
# Author: Lucas Kook
# Date: 2019/03/09

# Parameter specification -------------------------------------------------

theta <- 0.5
sd <- 1
csd <- sqrt(2) * sd
alpha <- 0.05
pwr <- 0.8
n <- calc_n(
  theta = theta,
  sd = sd,
  alpha = alpha,
  pwr = pwr
)

prior_mu_theta <- 0.5
prior_sd_theta <- 0.1

prior_mu_sigma <- 1
prior_sd_sigma <- 0.3

nsim <- 10000

res <-
  simulate_pwr(
    nsim = nsim,
    prior_mu_theta = prior_mu_theta,
    prior_sd_theta = prior_sd_theta,
    prior_mu_sigma = prior_mu_sigma,
    prior_sd_sigma = prior_sd_sigma,
    n = n,
    alpha = alpha,
    pwr = pwr
  )

par(
  mfrow = c(1, 2),
  bty = "n",
  las = 1,
  mar = c(4.1, 5.1, 2.1, 1.1)
)
hist(
  res$power,
  breaks = 50,
  xlim = c(0, 1),
  main = "",
  xlab = expression(Pr(italic(S)[epsilon] ^ italic(C) ~ "|" ~ theta)),
  col = "cornflowerblue",
  probability = TRUE
)
hist(
  res$n,
  breaks = 50,
  main = "",
  xlab = expression(italic(n)),
  col = "cornflowerblue",
  probability = TRUE
)
points(res$n[res$n > 10 * n], rep(0, length(res$n[res$n > 10 * n])), col = "red", pch = "|")
