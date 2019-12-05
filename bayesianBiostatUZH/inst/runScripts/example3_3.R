#---------------------------------------------------------------------------------------------------------------
# Code for example 3.3 in Spiegelhalter 2006
# Date: 2019/05/25
#---------------------------------------------------------------------------------------------------------------
######### LIBRARIES
library(bayesianBiostatUZH)

#---------------------------------------------------------------------------------------------------------------

## We want to find the parameters of a beta distribution with mean m = 0.4 and standard deviation s = 0.1
m <- 0.4
s <- 0.1
par <- beta_parameters(m, s)
a <- par[1]
b <- par[2]

## We then observe r = 15 success out of n = 20 trials, so our prior Be[a,b] becomes a posterior Be[newa, newb]
r <- 15
n <- 20
pos <- posterior_conj_betabin(a,b,n,r)
newa <- pos[1]
newb <- pos[2]

## We finally plot the prior Be[a,b], the likelihood Bin[n,r] and the posterior Be[newa,newb]
par(mfrow=c(3,1))

curve(dbeta(x,a,b), ylim=c(0,5),
      yaxt="n", ylab=" ", bty="n",
      xlab=" ", main="(a) Prior")

likelihood <- function(par) {return(par^15*(1-par)^5)}
theta <- seq(0,1,0.01)
plot(theta, sapply(theta,
                   FUN=likelihood), type="l",
     yaxt="n", ylab=" ", bty="n",
     xlab=" ", main="(b) Likelihood")

curve(dbeta(x,newa,newb), yaxt="n",
      ylab=" ", , bty="n", xlab=" ",
      main="(c) Posterior")
