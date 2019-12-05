#---------------------------------------------------------------------------------------------------------------
# Code for example 5.4 in Spiegelhalter 2006
# Date: 2019/05/24
#---------------------------------------------------------------------------------------------------------------
######### LIBRARIES
library(bayesianBiostatUZH)
par(mfrow = c (3,1))
x<-seq(-5,10, by = 0.01)

# Prior
plot(x, dnorm(x, 0, sqrt(var_cornfield_prior(8))),
     cex = 0.1, ylim = c (-0.05,0.8),
     ylab = "",main = "(a) Prior distributions",
     col = "brown", type = "l", lwd = 2,
     xlab = "Improvement with urokinase in absolute resolution in 24-hour scan")
points( c(0,0.5)~c(0,0), type = "l", lwd = 5)
text(0, 0.6, "prior for Ho (p)", cex = 0.8)
text(2, 0.09, "prior for Ha (1-p)", col = "brown", cex = 0.8)

#Likelihood
# from the data we have: difference in mean response  = 3.61 with SE 1.11
plot(x, dnorm(x, 3.61, 1.11),col="blue",
     ylim = c (-0.05,1),
     type = "l", lwd = 2, ylab = "",
     main = "(b) Likelihood",
     xlab = "Improvement with urokinase in absolute resolution in 24-hour scan")
abline(v = 0, lwd = 1)

#Posterior
#For posterior estmation we use the function "posterior_normal"
mean_post1<-posterior_normal(prior.mean = 0,
                             prior.sd = sqrt(var_cornfield_prior(8)),
                             estimate.mean = 3.61,
                             estimate.se = 1.11)[1]
sd_post1<-posterior_normal(prior.mean = 0,
                           prior.sd = sqrt(var_cornfield_prior(8)),
                             estimate.mean = 3.61,
                           estimate.se = 1.11)[2]

# and the change in the point p of the null hypothesis is calculated using bayes factor
# firts, we need to calculate test stat
test_stat_zm<-3.61/1.11
# then, we using formula from the package we can calculate bayes factor
bf1<-BF_normal_composite(71,0.87,test_stat_zm)
# then, setting p = 0.5 we can calculate posterior on the point H0
post_Ho<-bf1/(1+bf1)
plot(x, dnorm(x, mean_post1, sd_post1),col="blue",
     ylim = c (-0.05,1),
     type = "l", lwd = 2, ylab = "", yaxt='n',
     main = "(c) Posterior distribution",
     xlab = "Improvement with urokinase in absolute
     resolution in 24-hour scan")
points( c(0,post_Ho)~c(0,0), type = "l", lwd = 10)
text(0, 0.55, "0.047", cex = 2)
