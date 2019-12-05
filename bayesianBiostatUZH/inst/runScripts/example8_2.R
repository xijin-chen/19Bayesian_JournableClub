## ----'preamble10',include=FALSE------------------------------------------
library(knitr)
opts_chunk$set(
    fig.path='figure/ch10_fig',
    self.contained=FALSE,
    cache=TRUE
)


## ----EFM_pre,echo=FALSE, warning=FALSE,error = FALSE, message=FALSE,results='hide'----
ID <- 1:9
treatment.death <- c(1,2,0,3,1,0,14,17,2)
treatment.patients <- c(175,242,253,463,445,
                        485,6530,122,746)
control.death <- c(1,1,1,0,0,1,14,18,9)
control.patients <- c(175,241,251,232,482,493,
                      6554,124,682)

dat <- data.frame(ID,treatment.death,
                  treatment.patients,control.death,
                  control.patients)

a <- treatment.death
c <- treatment.patients - treatment.death
b <- control.death
d <- control.patients - control.death


## ----EFM_pre2,echo=FALSE, warning=FALSE,error = FALSE, message=FALSE,results='hide'----
estim.log.odds.ratio <-
  log((a+0.5)*(d+0.5) / (b+0.5)/(c+0.5))
estim.variance <-
  1/(a+0.5) + 1/(b+0.5) + 1/(c+0.5) + 1/(d+0.5)
estim.sd <- sqrt(estim.variance)


## ----EFM1,echo=FALSE, warning=FALSE,error = FALSE, message=FALSE,results='hide'----
#two parts for fixed-effect model
#1st for the individuals, from the fixed-effect model by bayesian
jags_data<-list(y=estim.log.odds.ratio,
                rt=dat$treatment.death,
                nt=dat$treatment.patients,
                rc=dat$control.death,
                nc=dat$control.patients
)
params.jags.noncen <-c( "theta")
EFM1.model <- "model
{
for(j in 1:9){
  y[j]    ~ dnorm(theta[j],sigma2.inv[j])
  theta[j]  ~ dunif(-10,10)
 }
 for(j in 1:9)
 	{
	sigma2[j] <- 1/(rt[j] + 0.5) + 1/(nt[j] - rt[j] + 0.5) +
	1/(rc[j] + 0.5) + 1/(nc[j] - rc[j] + 0.5)
	sigma2.inv[j]  <- 1/sigma2[j]
 	}
}
"
writeLines(EFM1.model, con="EFM1.model.txt")
# Initial values: need to give starting values to all unknowns
inits<-function(){
  list(theta=rep(0,9))
}
# model initiation
fixed_model <- jags.model(
  file = "EFM1.model.txt",
  data = jags_data,
  inits=inits,
  n.chains = 1,
  n.adapt = 4000
)
# burn-in
update(fixed_model, n.iter = 4000)
# sampling/monitoring
fit.model.fixed <- coda.samples(
  model = fixed_model,
  variable.names = params.jags.noncen,
  n.iter = 10000,
  thin = 1
)
theta.fixed.exp <-
  exp(MCMCsummary(fit.model.fixed,
              round = 2,Rhat = FALSE,
              params = c('theta'))[,c("2.5%","50%","97.5%")])
## ------------------------------------------------------------------------
#2nd for the overall, form the pooled-effect model by bayesian
jags_data<-list(y=estim.log.odds.ratio,
                rt=dat$treatment.death,
                nt=dat$treatment.patients,
                rc=dat$control.death,
                nc=dat$control.patients
)
params.jags.noncen <-c( "theta","mu")
EFM2.model <- "model
{

for(j in 1:9){
  y[j]    ~ dnorm(theta[j],sigma2.inv[j])
  theta[j]  <- mu
}
 for(j in 1:9)
 	{
	sigma2[j] <- 1/(rt[j] + 0.5) + 1/(nt[j] - rt[j] + 0.5) +
	1/(rc[j] + 0.5) + 1/(nc[j] - rc[j] + 0.5)
	sigma2.inv[j]  <- 1/sigma2[j]
 	}
  # overall effect
mu  ~ dunif(-10,10)       # uniform on overall mean
}
"
writeLines(EFM2.model, con="EFM2.model.txt")
# Initial values; need to give starting values to all unknowns
inits<-function(){
  list(
    theta=rep(0,9))}
# model initiation
pooled_model <- jags.model(
  file = "EFM2.model.txt",
  data = jags_data,
  inits=inits,
  n.chains = 1,
  n.adapt = 4000
)
# burn-in
update(pooled_model, n.iter = 4000)
# sampling/monitoring
fit.model.pooled <- coda.samples(
  model = pooled_model,
  variable.names = params.jags.noncen,
  n.iter = 10000,
  thin = 1
)
mu.pooled.exp <- exp(MCMCsummary(fit.model.pooled,
                                 round = 2, Rhat = FALSE,
                                 params = c('mu'))[,c("2.5%","50%","97.5%")])
matrix1 <- data.frame(rbind(cbind(theta.fixed.exp[,1],
                                  theta.fixed.exp[,2],
                                  theta.fixed.exp[,3]),
                            c(mu.pooled.exp[2],mu.pooled.
                              exp[1],mu.pooled.exp[3])))


## ----EFM2,echo=FALSE, warning=FALSE,error = FALSE, message=FALSE,results=F----
jags_data<-list(
  y=estim.log.odds.ratio,
  sigma2.inv=1/(estim.sd^2))
params.jags.noncen <-c("mu", "tau", "tau2.inv", "theta")
#random, exact binomial, indep, uniform
EFM2.model <- "model
{
for(j in 1:9){
  y[j]    ~ dnorm(theta[j],sigma2.inv[j])
  theta[j]  ~ dnorm(mu,tau2.inv)
}
mu  ~ dunif(-10,10)       # uniform on overall mean
tau ~ dunif(0,50)      # Prior: Uniform(0,50) on tau
tau2.inv <- 1/(tau * tau)
}
"
writeLines(EFM2.model, con="EFM2.model.txt")
# Initial values; need to give starting values to all unknowns
inits<-function(){
        list(mu=0,tau=.1)
}
# model initiation
normal_model <- jags.model(
        file = "EFM2.model.txt",
        data = jags_data,
        inits=inits,
        n.chains = 1,n.adapt = 4000
)
# burn-in
update(normal_model, n.iter = 4000)
# sampling/monitoring
fit.model.normal <- coda.samples(
        model = normal_model,
        variable.names = params.jags.noncen,
        n.iter = 10000,thin = 1
)
tau3<- (MCMCsummary(fit.model.normal,round = 2,
                    Rhat = FALSE, params = c('tau')))
theta3.exp <- exp(MCMCsummary(fit.model.normal,round = 2,
                              Rhat = FALSE,
                              params = c('theta'))[,c("2.5%","50%","97.5%")])
mu3.exp <- exp(MCMCsummary(fit.model.normal,round = 2,
                           Rhat = FALSE,
                           params = c('mu'))[,c("2.5%","50%","97.5%")])
matrix3 <- data.frame(rbind(theta3.exp,mu3.exp))


## ----EFM3,echo=FALSE, warning=FALSE,error = FALSE, message=FALSE,results=F----
jags_data<-list(rt=dat$treatment.death,
                nt=dat$treatment.patients,
                rc=dat$control.death,
                nc=dat$control.patients)
params.jags.noncen <-c("mu", "tau", "tau2.inv", "theta","pc")
#random, exact binomial, indep uniform
EFM3.model <- "model
{
for(j in 1:9){
                rt[j]  ~ dbin(pt[j],nt[j])
                rc[j]  ~ dbin(pc[j],nc[j])
                logit(pt[j]) <- theta[j] + logit(pc[j])
                pc[j]         ~ dunif(0,1)
                theta[j]  ~ dnorm(mu,tau2.inv)
}
mu  ~ dunif(-10,10)       # uniform on overall mean
tau ~ dunif(0,50)      # Prior: Uniform(0,50) on tau
tau2.inv <- 1/(tau * tau)
}
"
writeLines(EFM3.model, con="EFM3.model.txt")
# Initial values; need to give starting values to all unknowns
inits<-function(){
        list(mu=0,
             tau=.1,
             pc=rep(0.01,9))

}
# model initiation
binom1_model <- jags.model(
        file = "EFM3.model.txt",
        data = jags_data,
        inits=inits,
        n.chains = 1,
        n.adapt = 4000
)
# burn-in
update(binom1_model, n.iter = 4000)

# sampling/monitoring
fit.model.binomial1 <- coda.samples(
        model = binom1_model,
        variable.names = params.jags.noncen,
        n.iter = 10000,
        thin = 1
)
#summ_model2 <- summary(fit.model.binomial1)
tau4 <- MCMCsummary(fit.model.binomial1,round = 2,
                    Rhat = FALSE,params = c('tau'))
theta4.exp <- exp(MCMCsummary(fit.model.binomial1,
                              round = 2, Rhat = FALSE,
                              params = c('theta'))[,c("2.5%","50%","97.5%")])
mu4.exp <- exp(MCMCsummary(fit.model.binomial1,round = 2,
                           Rhat = FALSE,
                           params = c('mu'))[,c("2.5%","50%","97.5%")])
matrix4 <- data.frame(rbind(theta4.exp,mu4.exp))


## ----EFMtau,echo=FALSE, warning=FALSE,error = FALSE, message=FALSE,results=F----
jags_data<-list(rt=dat$treatment.death,
                nt=dat$treatment.patients,
                rc=dat$control.death,
                nc=dat$control.patients)
params.jags.noncen <-c("mu", "tau", "tau2.inv", "theta","pc")
#random, exact binomial, indep uniform
EFM.model.sens <- "model
{
for(j in 1:9){
                rt[j]  ~ dbin(pt[j],nt[j])
                rc[j]  ~ dbin(pc[j],nc[j])
                logit(pt[j]) <- theta[j] + logit(pc[j])
                pc[j]         ~ dunif(0,1)
                theta[j]  ~ dnorm(mu,tau2.inv)
}
mu  ~ dunif(-10,10)       # uniform on overall mean
tau ~ dunif(0,2)      # Prior: Uniform(0,50) on tau
tau2.inv <- 1/(tau * tau)
}
"
writeLines(EFM.model.sens, con="EFM.model.sens.txt")
# Initial values; need to give starting values to all unknowns
inits<-function(){
        list(mu=0,
             tau=.1,
            # phi.mean=-4,
             #phi.sd=.1,
            # phi5=-4,
             pc=rep(0.01,9)
        )
}
# model initiation
binom1_model_sens <- jags.model(
        file = "EFM.model.sens.txt",
        data = jags_data,
        inits=inits,
        n.chains = 1,
        n.adapt = 4000
)
# burn-in
update(binom1_model_sens, n.iter = 4000)

# sampling/monitoring
fit.model.binomial1.sens <- coda.samples(
        model = binom1_model_sens,
        variable.names = params.jags.noncen,
        n.iter = 10000,
        thin = 1
)
tau5 <- MCMCsummary(fit.model.binomial1.sens,round = 2, Rhat = FALSE,
                    params = c('tau'))
theta5.exp <- exp(MCMCsummary(fit.model.binomial1.sens,round = 2,
                              Rhat = FALSE,params = c('theta'))[,c("2.5%","50%","97.5%")])
mu5.exp <- exp(MCMCsummary(fit.model.binomial1.sens,round = 2,
                           Rhat = FALSE,params = c('mu'))[,c("2.5%","50%","97.5%")])
matrix5 <- data.frame(rbind(theta5.exp,mu5.exp))


## ----EFM4,echo=FALSE, warning=FALSE,error = FALSE, message=FALSE,results=F----
jags_data<-list(rt=dat$treatment.death,
                nt=dat$treatment.patients,
                rc=dat$control.death,
                nc=dat$control.patients)
params.jags.noncen <-c("mu", "tau", "tau2.inv", "theta","phi")
EFM4.model <- "model
{
for(j in 1:9){
                 rt[j]  ~ dbin(pt[j],nt[j])
                rc[j]  ~ dbin(pc[j],nc[j])
                logit(pt[j]) <- theta[j] + phi[j]
		logit(pc[j]) <- phi[j]
		phi[j]     ~ dunif(-10,10)
                theta[j]  ~ dnorm(mu,tau2.inv)
}

mu  ~ dunif(-10,10)       # uniform on overall mean
tau ~ dunif(0,50)      # Prior: Uniform(0,50) on tau
tau2.inv <- 1/(tau * tau)
}
"
writeLines(EFM4.model, con="EFM4.model.txt")
# Initial values; need to give starting values to all unknowns
inits<-function(){
        list(mu=0,
             tau=.1,
             phi=rep(-4,9)
        )
}

# model initiation
binom2_model <- jags.model(
        file = "EFM4.model.txt",
        data = jags_data,
        inits=inits,
        n.chains = 1,
        n.adapt = 4000
)
# burn-in
update(binom2_model, n.iter = 4000)
# sampling/monitoring
fit.model.binomial2 <- coda.samples(
        model = binom2_model,
        variable.names = params.jags.noncen,
        n.iter = 10000,
        thin = 1
)
tau6 <- MCMCsummary(fit.model.binomial2,round = 2,
                    Rhat = FALSE,params = c('tau'))
theta6.exp <- exp(MCMCsummary(fit.model.binomial2,
                              round = 2, Rhat = FALSE,
                              params = c('theta'))[,c("2.5%","50%","97.5%")])
mu6.exp <- exp(MCMCsummary(fit.model.binomial2,round = 2,Rhat = FALSE,
                           params = c('mu'))[,c("2.5%","50%","97.5%")])
matrix6 <- data.frame(rbind(theta6.exp,mu6.exp))


## ----EFMall,echo=FALSE, warning=FALSE,error = FALSE, message=F,results=F----
name_col <- c(paste("Trial",1:9),"Overall")
matrix1$Trial <- name_col
name_model <- rep("Model1",10)
matrix1$Model <- name_model
colnames(matrix1) <- c("OR_L","OR","OR_U","Trial","Model")

matrix3$Trial <- name_col
name_model <- rep("Model3",10)
matrix3$Model <- name_model
colnames(matrix3) <- c("OR_L","OR","OR_U","Trial","Model")

matrix4$Trial <- name_col
name_model <- rep("Model4",10)
matrix4$Model <- name_model
colnames(matrix4) <- c("OR_L","OR","OR_U","Trial","Model")

matrix5$Trial <- name_col
name_model <- rep("Model5",10)
matrix5$Model <- name_model
colnames(matrix5) <- c("OR_L","OR","OR_U","Trial","Model")

matrix6$Trial <- name_col
name_model <- rep("Model6",10)
matrix6$Model <- name_model
colnames(matrix6) <- c("OR_L","OR","OR_U","Trial","Model")

matrix_full <- (rbind(matrix1,matrix3,matrix4,matrix5,matrix6))


## ----fig.height=5,echo=FALSE,message=FALSE,warning=FALSE-----------------
par(las=1)
gplots::plotCI(y=10:1, x = matrix6$OR, li= matrix6$OR_L,
               ui = matrix6$OR_U, err="x",
               xlab="Favours EFM  <-   Odds ratio   ->  Favours Control",
							 ylab="", pch=19, xlim=c(0.01,70),
							 ylim = c(0.5,10.5), yaxt='n',log='x',col="green")
par(new=T)
gplots::plotCI(y=(10:1)+0.3,  x = matrix1$OR,
               li= matrix1$OR_L, ui = matrix1$OR_U, err="x",
               pch=19, xlim=c(0.01,70), ylim = c(0.5,10.5), yaxt='n',
               col = "black",log='x' ,lty=1,xlab="",ylab="")

par(new=T)
gplots::plotCI(y=(10:1)+0.2,x = matrix3$OR,
               li= matrix3$OR_L, ui = matrix3$OR_U, err="x",
               pch=19, xlim=c(0.01,70), ylim = c(0.5,10.5), yaxt='n',
               col = "blue",log='x' ,lty=1,xlab=" ",ylab="")

par(new=T)
gplots::plotCI(y=(10:1)+0.1,  x = matrix4$OR,
               li= matrix4$OR_L, ui = matrix4$OR_U, err="x",
               pch=19, xlim=c(0.01,70), ylim = c(0.5,10.5),
               yaxt='n', col = "red",log='x' ,lty=1,xlab=" ",ylab="")
abline(v=1, lty=1, col = "black")


op <- par(cex = 0.7)
axis(2,at = 1:10, labels=c("Overall",9:1), par(las=1))
legend("bottomright", legend=c("fixed", "normal random",
                               "Binomial,uniform risks",
                               "Binomial,uniform logits"),
			 col=c("black","blue", "red","green"), lty=1, bty = "n",cex = 1)



