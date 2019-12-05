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

## ----ex6.4, echo=TRUE----------------------------------------------------

# Specify Parameters ------------------------------------------------------

crd <- -0.39 # clinically relevant difference
mu <- -0.12 # clinical prior mean
sd <- 2 # standard deviation
conf.level <- 0.95 # confidence level
eps <- 0.025 # alpha/2
n <- 276 # number of events (from planned sample size)
n0 <- 111 # From clinical prior (since sd/n0=0.19 was observed)

nobs <- 281 # number of observed events (137 under D1, 144 under D2)
theta_hat <- 0.09 # parameter estimate (log HR in this case)
se_theta_hat <- 0.11 # uncertainty in theta_hat

mu_pred <- mu # mean for the predictive distribution
sd_pred <-
  sd * sqrt(1 / n0 + 1 / nobs) # sd for the predictive distribution

# Run ---------------------------------------------------------------------

res <-
  hybrid_power_analysis(
    mu = mu,
    n0 = n0,
    n = n,
    sd = sd,
    eps = eps,
    conf.level = conf.level,
    par_range = -c(-0.5, 1),
    alternative = "less"
  )


## ----fig6_4, echo=FALSE, warning=FALSE, error=FALSE, message=FALSE-------
par(
  mfrow = c(2, 2),
  bty = "n",
  las = 1,
  mar = c(4.1, 5.1, 2.1, 1.1)
)
plot(
  res$par_vals,
  res$prior.dens,
  type = "l",
  xlab = expression(theta),
  ylab = expression(p(theta))
)
abline(v = c(0, crd), col = "grey")
mtext(
  text = "(a) Clinical prior",
  side = 3,
  line = 1,
  adj = 0,
  cex = 0.8,
  font = 6
)
plot(
  res$par_vals,
  res$c.pwr,
  type = "l",
  xlab = expression(theta),
  ylab = expression(Pr(italic(S)[epsilon] ^ italic(C) ~ "|" ~ theta))
)
abline(v = c(0, crd), col = "grey")
mtext(
  text = "(b) Classical power curve",
  side = 3,
  line = 1,
  adj = 0,
  cex = 0.8,
  font = 6
)
plot(
  res$par_vals,
  dnorm(
    x = res$par_vals,
    mean = theta_hat,
    sd = se_theta_hat
  ),
  type = "l",
  xlab = expression(theta),
  ylab = expression(L(theta ~ "|" ~ t[1:n]))
)
abline(v = 0, col = "grey")
mtext(
  text = "(c) Likelihood",
  side = 3,
  line = 1,
  adj = 0,
  cex = 0.8,
  font = 6
)
plot(
  res$par_vals,
  dnorm(
    x = res$par_vals,
    mean = mu_pred,
    sd = sd_pred
  ),
  type = "l",
  xlab = expression(hat(theta)),
  ylab = expression(p(hat(theta)))
)
abline(v = c(0, theta_hat), col = "grey")
mtext(
  text = "(d) Predictive distribution",
  side = 3,
  line = 1,
  adj = 0,
  cex = 0.8,
  font = 6
)
