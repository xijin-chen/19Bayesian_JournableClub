#---------------------------------------------------------------------------------------------------------------
# Code for example 3.4 in Spiegelhalter 2006
# Date: 2019/05/25
#---------------------------------------------------------------------------------------------------------------
######### LIBRARIES
library(bayesianBiostatUZH)

#---------------------------------------------------------------------------------------------------------------

## We know that SBP is measured with a standard deviation of 5
sigma <- 5

## We have a normal prior with mean 120 and standard deviation 10
mu <- 120
sd <- 10
n_0 <- (sigma/sd)^2 # prior sample size

## We then take 2 measurements that have mean 130
m <- 2 # number of data
ym <- 130
se <- sigma/sqrt(m)

## We find the parameters of the normal posterior
pos <- posterior_normal(mu, sd, ym, se)
newmu <- pos[1]
newsd <- pos[2]

## We plot the prior N[mu,sd], the likelihood N[ym,se] and the posterior N[newmu,newsd]
par(mfrow=c(3,1))

curve(dnorm(x, mu , sd), xlim=c(100, 140),
      ylim=c(0,0.1), yaxt="n", ylab=" ",
      bty="n", xlab=" ", main="(a) Prior")

curve(dnorm(x, ym , se), xlim=c(100, 140),
      yaxt="n", ylab=" ", bty="n",
      xlab=" ", main="(b) Likelihood")

curve(dnorm(x, newmu, newsd), xlim=c(100, 140),
      yaxt="n", ylab=" ", , bty="n", xlab=" ",
      main="(c) Posterior")

