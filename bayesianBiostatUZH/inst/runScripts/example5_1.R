#---------------------------------------------------------------------------------------------------------------
# Code for example 5.1 in Spiegelhalter 2006
# Date: 2019/05/24
#---------------------------------------------------------------------------------------------------------------
######### LIBRARIES
library(bayesianBiostatUZH)

#---------------------------------------------------------------------------------------------------------------
## Here we introduce the data from the example already as a frequency distribution (histogram)
## Here we use bin mean value as "introduced" in the example and experts opinion (points out of 100) as frequency
pooled<-c(rep(-7.5, 3), rep(-2.5, 7),
          rep(2.5,20),rep(7.5,21),
          rep(12.5,25), rep(17.5,18),
           rep(22.5,5), 27.5)

par(mfrow = c (1,2))
#First, we plot experts histogram of experts opinions "as is"
hist(pooled, col = "grey", main = "A)",
     las = 2, xlab = "Pchart - Pstandard,%")
## Further, we need to "rearrange" the hist of their opinions to logOR scale
m1<-mean(surv_diff_to_logHR(pooled,15))
# mean on a new scale is:
sd1<-sd(surv_diff_to_logHR(pooled,15))
# sd on a new scale is:
cat_levels<-c(-10,-5,0,5,10,15,20,25,30)
#these are "limits" of bins of the "introduced" histogram from above

hist(surv_diff_to_logHR(pooled,15),
     col = "grey", main = "B)",
     las = 2,xlab = "logHR", prob = T,
     breaks = surv_diff_to_logHR(cat_levels,15) )
#then we plot fitted normal distribution which can behave a "prior" for further analysis
points(seq(-1, 0.4, 0.01),
       dnorm(seq(-1, 0.4, 0.01),
             mean=m1, sd=sd1),
       col="darkblue", lwd=2, type="l")





