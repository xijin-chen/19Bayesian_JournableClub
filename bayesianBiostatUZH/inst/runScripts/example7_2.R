#---------------------------------------------------------------------------------------------------------------
# Code for example 7.2 in Spiegelhalter 2006
# Date: 2019/05/15
#---------------------------------------------------------------------------------------------------------------

#---------------------------------------------------------------------------------------------------------------
######### LIBRARIES

library(ggplot2)
library(dplyr)
# install.packages("TruncatedDistributions", repos="http://R-Forge.R-project.org")
library(truncnorm)
### IMPORTANT: to make rjags work you need to have JAGS installed
### Install from here: https://sourceforge.net/projects/mcmc-jags/
library(rjags)
library(coda)

#---------------------------------------------------------------------------------------------------------------
######### DATA

set.seed(12)

data("clinic", package = "bayesianBiostatUZH")
dd7.2 <- clinic
## p = live birth rates
## n = number of started trials
## Clinic = name of the clinics

dd7.2$r <- dd7.2$p * dd7.2$n / 100 # number of successful live births
dd7.2$y <- log((dd7.2$r + 0.5) / (dd7.2$n - dd7.2$r + 0.5)) # log odds
dd7.2$s <-
  sqrt(1 / (dd7.2$r + 0.5) + 1 / (dd7.2$n - dd7.2$r + 0.5)) # respective sd
dd7.2$s2 <-
  1 / (dd7.2$r + 0.5) + 1 / (dd7.2$n - dd7.2$r + 0.5) # variance

#---------------------------------------------------------------------------------------------------------------
######### MODELS

niter <- 10000 # number of iterations
n <- nrow(dd7.2) # number of clinics

#########################################
## independence model = fixed model
#########################################

fix <- apply(dd7.2, 1, function(x) {
  yk <- as.numeric(x[5])
  sk <- as.numeric(x[6])
  fix <- rtruncnorm(
    n = niter,
    mean = yk,
    sd = sk,
    a = -10,
    b = 10
  )
  fix
})

sims <- apply(fix, 2, function(x) {
  odds.fix <- exp(quantile(x, c(0.025, 0.5, 0.975)))
  c(odds.fix / c(1 + odds.fix))
})

fix.ranks <-
  apply(fix, 1, rank) # matrix with 52 rows and nsim columns. values are the ranks
fix.ranks <- apply(fix.ranks, 1, function(x) {
  quantile(x, c(0.025, 0.5, 0.975))
})

#########################################
## Exchangeable model = random model
#########################################

#### MCMC sims

an_data <- list(y = dd7.2$y, n = n, s2 = dd7.2$s2)

modelString <-  " # open quote for modelString
model{
## likelihood
for (k in 1:n){
y[k] ~ dnorm(theta[k], 1/s2[k])
theta[k] ~ dnorm(mutheta, 1/tautheta^2)
}
## population parameters for theta
mutheta ~ dunif(-10, 10)
tautheta ~ dunif(0, 10)
}
" # close quote for modelString

writeLines(modelString, con = "exmodel.txt") # write to a file

# model initialisation
model.jags <- jags.model(
  file = "exmodel.txt",
  data = an_data,
  n.chains = 1,
  n.adapt = 4000
)


update(model.jags, n.iter = 4000)

# sampling/monitoring
fit.jags.coda <- coda.samples(
  model = model.jags,
  variable.names = c("theta"),
  n.iter = 50000,
  thin = 1
)

sum.res.ex <- summary(fit.jags.coda)
ex.median <-
  exp(sum.res.ex$quantiles[, "50%"]) / (exp(sum.res.ex$quantiles[, "50%"]) +
                                          1)
ex.lower <-
  exp(sum.res.ex$quantiles[, "2.5%"]) / (exp(sum.res.ex$quantiles[, "2.5%"]) +
                                           1)
ex.upper <-
  exp(sum.res.ex$quantiles[, "97.5%"]) / (exp(sum.res.ex$quantiles[, "97.5%"]) +
                                            1)

rand.ranks <-
  apply(as.matrix(fit.jags.coda), 1, rank) # matrix with 52 rows and nsim columns. values are the ranks
rand.ranks <- apply(rand.ranks, 1, function(x) {
  quantile(x, c(0.025, 0.5, 0.975))
})

#########################################
## plots
#########################################

dd7.2.plot <- rbind(dd7.2, dd7.2)
dd7.2.plot$type <-
  rep(c("Independence\nmodel", "Exchangeable\nmodel"), each = 52)
dd7.2.plot$lower <- c(sims[1,], ex.lower)
dd7.2.plot$median <- c(sims[2,], ex.median)
dd7.2.plot$upper <- c(sims[3,], ex.upper)
dd7.2.plot$lower.rank <- c(fix.ranks[1,], rand.ranks[1,])
dd7.2.plot$median.rank <- c(fix.ranks[2,], rand.ranks[2,])
dd7.2.plot$upper.rank <- c(fix.ranks[3,], rand.ranks[3,])

theme <-  theme_bw() +
  theme(
    legend.position =  c(.8, .9),
    legend.title = element_blank(),
    axis.title.y = element_blank(),
    axis.title = element_text(size = 13),
    axis.text.y = element_text(size = 7),
    axis.text.x = element_text(size = 10),
    plot.title = element_text(size = 14),
    legend.text = element_text(size = 11),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )

fig7.2 <-
  ggplot(dd7.2.plot, aes(
    y = median * 100,
    x = Clinic,
    col = type,
    fill = type
  )) +
  coord_flip() +
  geom_hline(yintercept = 14, linetype = 2) +
  geom_errorbar(
    aes(ymin = lower * 100, ymax = upper * 100),
    position = position_dodge(width = 0.6),
    width = 0.5
  ) +
  geom_point(pch = 21,
             col = "black",
             position = position_dodge(width = 0.6)) +
  labs(y = "Adjusted live birth rate (%)", fill = "", col = "") +
  theme

fig7.3 <-
  ggplot(dd7.2.plot, aes(
    y = median.rank,
    x = Clinic,
    col = type,
    fill = type
  )) +
  coord_flip() +
  geom_hline(yintercept = 13.5, linetype = 2) +
  geom_hline(yintercept = 26.5, linetype = 2) +
  geom_hline(yintercept = 39.5, linetype = 2) +
  geom_errorbar(
    aes(ymin = lower.rank, ymax = upper.rank),
    position = position_dodge(width = 0.6),
    width = 0.5
  ) +
  geom_point(pch = 21,
             col = "black",
             position = position_dodge(width = 0.6)) +
  labs(y = "Rank", fill = "", col = "") +
  theme

fig7.2
fig7.3
