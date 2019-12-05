#---------------------------------------------------------------------------------------------------------------
# Code for example 6.7 in Spiegelhalter 2006
# Author: Katrin Petermann
# Date: 2019/05/28
#---------------------------------------------------------------------------------------------------------------
######### LIBRARIES
library(bayesianBiostatUZH)

coldef <- c("#048ABF", "#04C4D9", "#F2B705", "#F28705", "#F2380F")

##################################################
## this is the code used to produce figure 6.10 ##
##################################################

x <- seq(log(0.4), log(3), len = 1000)
n0 <- 41.4
sigma <- 2
mu1 <- 0
mu2 <- -0.51

# for a nicely formated plot, uncomment the following line and put a dev.off()
# at the end of the figure.
# pdf(file = "./ch07_fig_6-10.pdf", width = 8, height = 9)

par(mfrow = c(6, 2), mar = c(3, 0, 4, 0))

frame()
legend(
  "top",
  bty = "n",
  legend = c("Optimistic Prior", "Sceptical Prior"),
  lty = c(2, 1),
  cex = 1.5
)

# Prior
plot(
  x = exp(x),
  y = dnorm(x, mean = mu1, sd = sigma / sqrt(n0)),
  log = "x",
  type = "l",
  yaxt = "n",
  xaxt = "n",
  bty = "n",
  ylim = c(0, 3),
  ylab = "",
  xlab = ""
)
lines(
  x = exp(x),
  y = dnorm(x, mean = mu2, sd = sigma / sqrt(n0)),
  lty = 2
)

title("Prior", line = 3)
axis(side = 1, at = seq(0.4, 3, by = 0.1))
abline(v = 1.0)

#likelihood: data from table 6.6
logHR <- c(0.435, 0.567, 0.545, 0.588, 0.519)
selogHR <- c(0.295, 0.244, 0.213, 0.198, 0.172)
years <- c("1993", "1994", "June 1995", "Dec. 1995", "1996")

#posterior
m <- (sigma / selogHR) ^ 2
postmean1 <- (n0 * mu1 + m * logHR) / (n0 + m)
postmean2 <- (n0 * mu2 + m * logHR) / (n0 + m)
postse <- sigma / sqrt(n0 + m)

for (i in 1:length(logHR)) {
  # plot likelihood
  if (i == length(logHR)) {
    plot(
      x = exp(x),
      y = dnorm(x, mean = logHR[i], sd = selogHR[i]),
      log = "x",
      type = "l",
      yaxt = "n",
      xaxt = "n",
      bty = "n",
      ylim = c(0, 3),
      ylab = "",
      xlab = "Tamoxifen superior   <-   Hazard ratio   ->   Control superior",
      mgp = c(2, 1, 1)
    )
    title(paste(years[i], "Likelihood"), line = 2)
  } else {
    plot(
      x = exp(x),
      y = dnorm(x, mean = logHR[i], sd = selogHR[i]),
      log = "x",
      type = "l",
      yaxt = "n",
      xaxt = "n",
      bty = "n",
      ylim = c(0, 3),
      ylab = "",
      xlab = ""
    )
    title(paste(years[i], "Likelihood"), line = 2)
  }
  axis(side = 1, at = seq(0.4, 3, by = 0.1))
  abline(v = 1.0)

  # plot posterior
  if (i == length(logHR)) {
    plot(
      x = exp(x),
      y = dnorm(x, mean = postmean1[i], sd = postse[i]),
      log = "x",
      type = "l",
      yaxt = "n",
      xaxt = "n",
      bty = "n",
      ylim = c(0, 3),
      ylab = "",
      xlab = "Tamoxifen superior   <-   Hazard ratio   ->   Control superior",
      mgp = c(2, 1, 1)
    )
    lines(
      x = exp(x),
      y = dnorm(x, mean = postmean2[i], sd = postse[i]),
      lty = 2
    )
    title(paste(years[i], "Posterior"), line = 2)
  } else {
    plot(
      x = exp(x),
      y = dnorm(x, mean = postmean1[i], sd = postse[i]),
      log = "x",
      type = "l",
      yaxt = "n",
      xaxt = "n",
      bty = "n",
      ylim = c(0, 3),
      ylab = "",
      xlab = ""
    )
    lines(
      x = exp(x),
      y = dnorm(x, mean = postmean2[i], sd = postse[i]),
      lty = 2
    )
    title(paste(years[i], "Posterior"), line = 2)
  }
  axis(side = 1, at = seq(0.4, 3, by = 0.1))
  abline(v = 1.0)
}

# dev.off()


##################################################
## this is the code used to produce figure 6.11 ##
##################################################

# for a nicely formatet plot, uncomment the following line and put a dev.off()
# at the end of the figure.
# pdf(file = "./figure/ch07_fig_6-11.pdf", width = 12, height = 8)
par(mfcol = c(4, 3), mar = c(4, 3, 2, 1))

# HR for reference, sceptical and optimistic posterior
n0 <- 41.4
sigma <- 2
mu1 <- 0
mu2 <- -0.51
n <- 69
m <- 46
lHR_ref <- 0.435
selHR_ref <- 0.295

lHR_scept <- (n0 * mu1 + m * lHR_ref) / (n0 + m)
lHR_opt <- (n0 * mu2 + m * lHR_ref) / (n0 + m)
postse <- sigma / sqrt(n0 + m)

logHR <- c(lHR_ref, lHR_scept, lHR_opt)
selogHR <- c(selHR_ref, postse, postse)

theta <- seq(log(0.4), log(3), len = 1000)

n_prior <- c(0, 41.4, 41.4)
mu_prior <- c(0, 0,-0.51)

title <-
  c(
    "Reference analysis: posterior",
    "Sceptical analysis: posterior",
    "Optimisitc analysis: posterior"
  )
c <- c(1, coldef[1], coldef[5])

for (i in 1:3) {
  Csup <- sapply(
    theta,
    FUN = bayesian_conditional_pwr,
    n0 = n_prior[i],
    mu = mu_prior[i],
    m = m,
    ym = lHR_ref,
    n = n,
    eps = 0.025
  )
  Tsup <- 1 - sapply(
    theta,
    FUN = bayesian_conditional_pwr,
    n0 = n_prior[i],
    mu = mu_prior[i],
    m = m,
    ym = lHR_ref,
    n = n,
    eps = 0.975
  )
  Equi <- 1 - Csup - Tsup
  plot(
    x = exp(theta),
    y = dnorm(theta, mean = logHR[i], sd = selogHR[i]),
    log = "x",
    type = "l",
    lwd = 2,
    yaxt = "n",
    xaxt = "n",
    bty = "n",
    col = c[i],
    ylim = c(0, 3),
    ylab = "",
    xlab = ""
  )
  axis(side = 1,
       at = seq(0.4, 3, by = 0.2),
       cex.axis = 0.8)
  abline(v = 1.0)
  title(title[i], cex.main = 1.2)

  plot(
    exp(theta),
    Tsup,
    type = "l",
    log = "x",
    lwd = 2,
    ylim = c(0, 1),
    xlim = c(0.4, 3),
    col = c[i],
    xaxt = "n",
    bty = "n",
    las = 1,
    cex.axis = 0.8,
    ylab = "",
    xlab = ""
  )
  axis(side = 1,
       at = seq(0.4, 3, by = 0.2),
       cex.axis = 0.8)
  abline(v = 1.0)
  title(
    "Probability of concluding 'Tamoxifen superior'",
    cex.main = 1,
    font.main = 1
  )

  plot(
    exp(theta),
    Equi,
    type = "l",
    log = "x",
    lwd = 2,
    ylim = c(0, 1),
    xlim = c(0.4, 3),
    col = c[i],
    xaxt = "n",
    bty = "n",
    las = 1,
    cex.axis = 0.8,
    ylab = "",
    xlab = ""
  )
  axis(side = 1,
       at = seq(0.4, 3, by = 0.2),
       cex.axis = 0.8)
  abline(v = 1.0)
  title(
    "Probability of concluding 'Equivocal'",
    cex.main = 1,
    font.main = 1
  )

  plot(
    exp(theta),
    Csup,
    type = "l",
    log = "x",
    lwd = 2,
    ylim = c(0, 1),
    xlim = c(0.4, 3),
    col = c[i],
    xaxt = "n",
    bty = "n",
    las = 1,
    cex.axis = 0.8,
    ylab = "",
    xlab = "Tamoxifen superior   <-   Hazard ratio   ->   Control superior"
  )
  axis(side = 1,
       at = seq(0.4, 3, by = 0.2),
       cex.axis = 0.8)
  abline(v = 1.0)
  title(
    "Probability of concluding 'Control superior'",
    cex.main = 1,
    font.main = 1
  )
}

# dev.off()


###############################################################
## this is the code used to produce the results in table 6.7 ##
###############################################################


tab.6.7 <-
  data.frame("Final conclusion" = c("Control superior", "Equivocal", "Tamoxifen superior"))

n0 <- 41.4
mu_opt <- -0.51
mu_scept <- 0
m <- 46
ym <- 0.435
n <- 69
sd <- 2
eps <- 0.025

# classical predictive probability
tab.6.7$reference <-
  hypothesis_reversal(
    classical_predictive_probability,
    ym = ym,
    m = m,
    n = n,
    sd = sd,
    eps = eps
  )

# bayesian predictive probability: sceptcal prior
tab.6.7$bayesian_scept <-
  hypothesis_reversal(
    bayesian_predictive_probability,
    n0 = n0,
    mu = mu_scept,
    ym = ym,
    m = m,
    n = n,
    sd = sd,
    eps = eps
  )

# bayesian predictive probability: optimistic prior
tab.6.7$bayesian_opt <-
  hypothesis_reversal(
    bayesian_predictive_probability,
    n0 = n0,
    mu = mu_opt,
    ym = ym,
    m = m,
    n = n,
    sd = sd,
    eps = eps
  )

# hybrid predictive probability: sceptcal prior
tab.6.7$hybrid_scept <-
  hypothesis_reversal(
    hybrid_predictive_probability,
    n0 = n0,
    mu = mu_scept,
    ym = ym,
    m = m,
    n = n,
    sd = sd,
    eps = eps
  )

# bayesian predictive probability: optimistic prior
tab.6.7$hybrid_opt <-
  hypothesis_reversal(
    hybrid_predictive_probability,
    n0 = n0,
    mu = mu_opt,
    ym = ym,
    m = m,
    n = n,
    sd = sd,
    eps = eps
  )

tab.6.7
