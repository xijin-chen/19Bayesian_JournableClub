library(tidyverse)
library(bayesianBiostatUZH)

# Function Section --------------------------------------------------------

calc_n0 <- function(mu, sd, conf.level) {
  stopifnot((sd > 0) & (conf.level > 0 & conf.level < 1))
  n0 <- qnorm(conf.level) ^ 2 * sd ^ 2 / mu ^ 2
  return(n0)
}

calc_n <- function(theta, sd, alpha, pwr) {
  n <- 2*sd^2*(qnorm(pwr)-qnorm(alpha/2))^2/theta^2
  return(n)
}

## ----fig6_3, echo=FALSE, warning=FALSE, error=FALSE, message=FALSE-------
# Code for Chapter 6 Part 1 of Spiegelhalter 2006
# Bayesian Sample Size Estimation
# Author: Lucas Kook
# Date: 2019/03/09

# Specify Parameters ------------------------------------------------------

mu <- 0.56
sd <- 2
conf.level <- 0.95
eps <- 0.025
n <- 100
n0 <- calc_n0(mu = mu, sd = sd, conf.level = conf.level)
thetas <- seq(-1, 2, length.out = 1000)


# Run ---------------------------------------------------------------------

complete_analysis <-
  hybrid_power_analysis(
    mu = mu,
    n0 = n0,
    n = n,
    sd = sd,
    eps = eps,
    conf.level = conf.level,
    par_range = c(-0.5, 1),
    alternative = "greater.than", 
    type = "one.sample"
  )

par(mfrow = c(2, 1))

with(complete_analysis, {
  plot(
    par_vals,
    c.pwr,
    type = "l",
    xlim = range(par_vals) * 1.1,
    ylim = c(0, 1),
    ylab = "Power",
    xlab = expression(theta),
    bty = "n",
    las = 1,
    lwd = 1.5
  )
  mtext(
    text = "(a) Power Curves",
    side = 3,
    line = 1,
    adj = 0,
    font = 6
  )
  lines(par_vals, b.pwr, lty = 2)
  abline(v = c(0, mu), col = "grey")
  plot(
    par_vals,
    prior.dens,
    type = "l",
    xlab = expression(theta),
    ylab = expression(p(theta)),
    xlim = range(par_vals) * 1.1,
    bty = "n",
    las = 1,
    lwd = 1.5
  )
  mtext(
    text = "(b) Prior Density",
    side = 3,
    line = 1,
    adj = 0,
    font = 6
  )
  abline(v = c(0, mu), col = "grey")
  
})
