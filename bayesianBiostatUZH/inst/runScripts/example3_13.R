#---------------------------------------------------------------------------------------------------------------
# Code for example 3.13 in Spiegelhalter 2006
# Date: 2019/05/27
#---------------------------------------------------------------------------------------------------------------
######### LIBRARIES
library(gplots)
#---------------------------------------------------------------------------------------------------------------


## Data of the meta-analysis
#----------------------------------------------
Trial <-
  c(
    "Morton",
    "Rasmussen",
    "Smith",
    "Abraham",
    "Feldstedt",
    "Shechter",
    "Ceremuzynski",
    "LIMIT-2"
  )

Magnesium.group.deaths <- c(1, 9, 2, 1, 10, 1, 1, 90)
Magnesium.group.patients <- c(40, 135, 200, 48, 150, 59, 25, 1159)
Control.group.deaths <- c(2, 23, 7, 1, 8, 9, 3, 118)
Control.group.patients <- c(36, 135, 200, 46, 148, 56, 23, 1157)

# Create a data frame
data <- data.frame(
  Trial,
  Magnesium.group.deaths,
  Magnesium.group.patients,
  Control.group.deaths,
  Control.group.patients
)

# Build a 2 by 2 table
a <- Magnesium.group.deaths
c <- Magnesium.group.patients - Magnesium.group.deaths
b <- Control.group.deaths
d <- Control.group.patients - Control.group.deaths

# Calculate the estimated log odds ratio
estim.log.odds.ratio <-
  log((a + 0.5) * (d + 0.5) / (b + 0.5) / (c + 0.5))
estim.odds.ratio <- exp(estim.log.odds.ratio)

# Calculate the estimated standard deviation of the estimated log odds ratio
estim.variance <-
  1 / (a + 0.5) + 1 / (b + 0.5) + 1 / (c + 0.5) + 1 / (d + 0.5)
estim.sd <- sqrt(estim.variance)

# Calculation of the 95 % CI for the OR
CI95_upper <- exp(estim.log.odds.ratio + 1.96 * estim.sd)
CI95_lower <- exp(estim.log.odds.ratio - 1.96 * estim.sd)

# Calculate the effective number of events
effective.no.events <- 4 / estim.variance



## True treatment effect for identical parameters
## -----------------------------------------------
# Pooled posterior mean
mu.pooled <-
  sum(estim.log.odds.ratio / estim.sd ^ 2) / sum(1 / estim.sd ^ 2)
OR.pooled <- exp(mu.pooled)

# Pooled posterior variance
variance.pooled <- 1 / sum(1 / estim.sd ^ 2)
sd.pooled <- sqrt(variance.pooled)

# Calculation of the 95 % CI
CI95_upper_pooled <- exp(mu.pooled + 1.96 * sd.pooled)
CI95_lower_pooled <- exp(mu.pooled - 1.96 * sd.pooled)



## Test for heterogeneity
## ----------------------
Q <- sum((estim.log.odds.ratio - mu.pooled) ^ 2 / estim.sd ^ 2)

# P-value
p_value <- 1 - pchisq(Q, 7)


## Exchangeable parameters (random effects)
## ----------------------------------------
# Calculate the Shrinkage coefficiant
Shrinkage <- estim.variance / (estim.variance + 0.29 ^ 2)

# Calculation of the mean:
estim.log.odds_exchangeable <-
  Shrinkage * mu.pooled + (1 - Shrinkage) * estim.log.odds.ratio
estim.odds_exchangeable <- exp(estim.log.odds_exchangeable)

# Calculation of the sd:
estim.var_exchangeable <- (1 - Shrinkage) * estim.variance
estim.sd_exchangeable <- sqrt(estim.var_exchangeable)

# 95 % CI
CI95_upper_ex <-
  exp(estim.log.odds_exchangeable + 1.96 * estim.sd_exchangeable)
CI95_lower_ex <-
  exp(estim.log.odds_exchangeable - 1.96 * estim.sd_exchangeable)



## Typical population OR for exchangeable parameters
## --------------------------------------------------
# Pooled posterior mean
mu.exchangeable <-
  sum(estim.log.odds.ratio / estim.sd_exchangeable ^ 2) / sum(1 / estim.sd_exchangeable ^ 2)
OR.exchangeable <- exp(mu.exchangeable)

# Pooled posterior variance
variance.exchangeable <- 1 / sum(1 / estim.sd_exchangeable ^ 2)
sd.exchangeable <- sqrt(variance.exchangeable)

# 95 % CI OR
CI95_mean_upper_ex <- exp(mu.exchangeable + 1.96 * sd.exchangeable)
CI95_mean_lower_ex <- exp(mu.exchangeable - 1.96 * sd.exchangeable)



## Plot
##-----
par(mar = c(4.1, 7, 4.1, 2.1), las = 1)
gplots::plotCI(
  y = 1:8,
  x = estim.odds.ratio,
  li = CI95_lower,
  ui = CI95_upper,
  err = "x",
  ylab = "",
  xlab = "Mortality odds ratios",
  pch = 19,
  xlim = c(0, 2),
  ylim = c(10, 1),
  yaxt = 'n'
)

par(new = T)
plotCI(
  y = (1:8) + 0.3,
  x = estim.odds_exchangeable,
  li = CI95_lower_ex,
  ui = CI95_upper_ex,
  err = "x",
  ylab = "",
  xlab = "Mortality odds ratios",
  pch = 19,
  xlim = c(0, 2),
  ylim = c(10, 1),
  yaxt = 'n',
  col = "black",
  lty = 3
)

par(new = T)
gplots::plotCI(
  y = 9,
  x = OR.pooled,
  li = CI95_lower_pooled,
  ui = CI95_upper_pooled,
  err = "x",
  ylab = "",
  xlab = "Mortality odds ratios",
  pch = 19,
  xlim = c(0, 2),
  ylim = c(10, 1),
  yaxt = 'n'
)

par(new = T)
plotCI(
  y = 9 + 0.3,
  x = OR.exchangeable,
  li = CI95_mean_lower_ex,
  ui = CI95_mean_upper_ex,
  err = "x",
  ylab = "",
  xlab = "Mortality odds ratios",
  pch = 19,
  xlim = c(0, 2),
  ylim = c(10, 1),
  yaxt = 'n',
  col = "black",
  lty = 3
)

abline(v = 1, lty = 2, col = "black")
op <- par(cex = 0.7)
axis(
  2,
  1:9,
  c(
    "Morton",
    "Rasmussen",
    "Smith",
    "Abraham",
    "Feldstedt",
    "Shechter",
    "Ceremuzynski",
    "LIMIT-2",
    "'Typical' \n Population"
  )
)
legend(
  "bottomright",
  legend = c(
    "Independent parameters (fixed effects)",
    "Exchangeable parameters (random effects)"
  ),
  col = c("black", "black"),
  lty = c(1, 3),
  cex = 1
)
