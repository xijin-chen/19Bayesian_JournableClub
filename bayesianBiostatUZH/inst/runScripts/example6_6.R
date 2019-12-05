#---------------------------------------------------------------------------------------------------------------
# Code for example 6.6 in Spiegelhalter 2006
# Author: Katrin Petermann
# Date: 2019/05/28
#---------------------------------------------------------------------------------------------------------------
######### LIBRARIES
library(bayesianBiostatUZH)

#################################################
## this is the code used to produce figure 6.7 ##
#################################################

x <- seq(log(0.4), log(1.3), len = 1000)
n0 <- 110
sigma <- 2
mu <- 0

# for a nicely formatet plot, uncomment the following line and put a dev.off()
# at the end of the figure.
# pdf(file = "./ch07_fig_6-07.pdf", width = 8, height = 9)

par(mfrow = c(6, 2), mar = c(3, 0, 4, 0))

frame()

# Sceptical prior
plot(
  x = exp(x),
  y = dnorm(x, mean = mu, sd = sigma / sqrt(n0)),
  log = "x",
  type = "l",
  yaxt = "n",
  xaxt = "n",
  bty = "n",
  ylim = c(0, 6),
  ylab = "",
  xlab = ""
)
title("Sceptical prior", line = 3)

psup <- pnorm(log(0.8), mean = mu, sd = sigma / sqrt(n0))
pequi <-
  pnorm(log(1), mean = mu, sd = sigma / sqrt(n0)) - pnorm(log(0.8), mean = mu, sd = sigma / sqrt(n0))
pinf <- 1 - psup - pequi

legend(
  x = 0.38,
  y = 9,
  bty = "n",
  cex = 1,
  legend = c("CHART superior", "Equivalent", "Control superior"),
  xpd = TRUE
)
legend(
  x = 0.51,
  y = 9,
  bty = "n",
  cex = 1,
  legend = c(paste(round(psup, 3)), paste(round(pequi, 3)), paste(round(pinf, 3))),
  xpd = TRUE
)

axis(side = 1, at = seq(0.4, 1.3, by = 0.1))

abline(v = 0.8)

abline(v = 1.0)

# estimates and CI from table 6.3
cilower <- log(c(0.35, 0.47, 0.55, 0.61, 0.63))
ciupper <- log(c(0.86, 0.83, 0.90, 0.93, 0.90))
loghr <- log(c(0.55, 0.63, 0.70, 0.75, 0.76))
selh <- (cilower - ciupper) / (2 * qnorm(0.025))

# calculation of posterior

m <- (sigma / selh) ^ 2
postmean <- (n0 * mu + m * loghr) / (n0 + m)
postse <- sigma / sqrt(n0 + m)

for (i in 1:length(loghr)) {
  # plot likelihood
  if (i == length(loghr)) {
    plot(
      x = exp(x),
      y = dnorm(x, mean = loghr[i], sd = selh[i]),
      log = "x",
      type = "l",
      yaxt = "n",
      xaxt = "n",
      bty = "n",
      ylim = c(0, 6),
      ylab = "",
      xlab = "favours CHART   <-   Hazard ratio   ->   favours control",
      mgp = c(2, 1, 1)
    )
    title(paste(1991 + i, "Likelihood"), line = 3)
  } else {
    plot(
      x = exp(x),
      y = dnorm(x, mean = loghr[i], sd = selh[i]),
      log = "x",
      type = "l",
      yaxt = "n",
      xaxt = "n",
      bty = "n",
      ylim = c(0, 6),
      ylab = "",
      xlab = ""
    )
    title(paste(1991 + i, "Likelihood"), line = 3)
  }

  axis(side = 1, at = seq(0.4, 1.3, by = 0.1))
  abline(v = 0.8)
  abline(v = 1.0)
  psup <- pnorm(log(0.8), mean = loghr[i], sd = selh[i])
  pequi <-
    pnorm(log(1), mean = loghr[i], sd = selh[i]) - pnorm(log(0.8), mean = loghr[i], sd = selh[i])

  pinf <- 1 - psup - pequi

  legend(
    x = 0.38,
    y = 9,
    bty = "n",
    cex = 1,
    legend = c("CHART superior", "Equivalent", "Control superior"),
    xpd = TRUE
  )
  legend(
    x = 0.51,
    y = 9,
    bty = "n",
    cex = 1,
    legend = c(paste(round(psup, 3)),
               paste(round(pequi, 3)),
               paste(round(pinf, 3))),
    xpd = TRUE
  )

  # plot posterior
  if (i == length(loghr)) {
    plot(
      x = exp(x),
      y = dnorm(x, mean = postmean[i], sd = postse[i]),
      log = "x",
      type = "l",
      yaxt = "n",
      xaxt = "n",
      bty = "n",
      ylim = c(0, 6),
      ylab = "",
      xlab = "favours CHART   <-   Hazard ratio   ->   favours control",
      mgp = c(2, 1, 1)
    )
    title(paste(1991 + i, "Posterior"), line = 3)
  } else {
    plot(
      x = exp(x),
      y = dnorm(x, mean = postmean[i], sd = postse[i]),
      log = "x",
      type = "l",
      yaxt = "n",
      xaxt = "n",
      bty = "n",
      ylim = c(0, 6),
      ylab = "",
      xlab = ""
    )
    title(paste(1991 + i, "Posterior"), line = 3)
  }

  axis(side = 1, at = seq(0.4, 1.3, by = 0.1))
  abline(v = 0.8)
  abline(v = 1.0)
  psup <- pnorm(log(0.8), mean = postmean[i], sd = postse[i])
  pequi <-
    pnorm(log(1), mean = postmean[i], sd = postse[i]) - pnorm(log(0.8), mean = postmean[i], sd = postse[i])
  pinf <- 1 - psup - pequi
  legend(
    x = 0.38,
    y = 9,
    bty = "n",
    cex = 1,
    legend = c("CHART superior", "Equivalent", "Control superior"),
    xpd = TRUE
  )
  legend(
    x = 0.51,
    y = 9,
    bty = "n",
    cex = 1,
    legend = c(paste(round(psup, 3)),
               paste(round(pequi, 3)),
               paste(round(pinf, 3))),
    xpd = TRUE
  )
}

# dev.off()
