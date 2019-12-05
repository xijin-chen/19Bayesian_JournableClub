## ----Hyper_pre, echo=FALSE, warning=FALSE,error = FALSE, message=FALSE,results=F----
r.t <- c(10,2,54,47,53,10,25,47,43,25,157,92)
n.t <- c(595.2, 762.0, 5635.0, 5135.0, 3760.0,
         2233.0, 7056.1, 8099.0, 5810.0, 5397.0,
         22162.7, 20885.0)
rate.t <- (r.t+0.5)/n.t*1000


r.c <- c(21,0,70,63,62,9,35,31,39,45,182,72)
n.c <- c(640.2, 756, 5600.0, 4960.0, 4210.0,
         2084.5, 6824.0, 8267.0, 5922.0,
         5173.0, 22172.5, 20645.0)
rate.c <- (r.c+0.5)/n.c*1000


log.RaR <- log(rate.t/rate.c)
var.log.RaR <- 1/(r.c+0.5)+1/(r.t+0.5)
ind <- 1:length(r.c)


## ----Hyper_a,fig.height=4.5,echo=FALSE, warning=FALSE,error = FALSE, message=FALSE,results=F----
plot(rate.c,exp(log.RaR),type="n",xlim=c(0.5,35.0),
     ylim=c(0.5,5.0),main="(a) Observed data",axes=FALSE,
     xlab="Control group rate per 1000 patient years",
     ylab="Rate ratio",log="xy")
text(rate.c,exp(log.RaR),label=ind,cex=0.8)
abline(h=1,lty=2)

axis(2, at=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0),
     labels=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0),
     las=2, cex.axis=0.7, tck=-.01)
axis(1, at=c(0.5,1.0,2.0,5.0,10.0,15.0,25.0,35.0),
     labels=c(0.5,1.0,2.0,5.0,10.0,15.0,25.0,35.0),
     cex.axis=0.7, tck=-.01)


## ----Hyper_b_pre, echo=FALSE, warning=FALSE,error = FALSE, message=FALSE,results=F----
#===================================
#(b)data for uniform prior(independent)
#===================================
dat <- as.data.frame(cbind(rt=r.t,rc=r.c,nt=n.t ,nc=n.c))

model.jag.independent <- "
model
{
for(i in 1:12){
rt[i]~dpois(mt[i])
rc[i]~dpois(mc[i])
log(mc[i]) <-log(nc[i]/1000) + phi[i]
log(mt[i]) <-log(nt[i]/1000) + phi[i] + theta[i]
theta[i] <- theta.adj[i] + beta*(phi[i]-mean(phi[1:12]))
theta.adj[i]  ~ dnorm(mu,tau2.inv)


phi[i]   ~ dunif(-10,10)   #   independence prior 1
log(rr[i])<-  theta[i]
log(rbase[i])<-phi[i]
}

#prior for baseline risk phi
#baseline analysis, mu and tau are given uniforms
mu  ~ dunif(-10,10)
tau2.inv <- 1/(tau*tau)
tau ~ dunif(0,10)

beta  ~ dunif(-10,10)
phi0 <- -mu/beta + mean(phi[1:12])
break<-exp(phi0)
#            predict theta for phi = -1,-.5,0,...., 3.5
for(j in 1:10){
theta.adj.pred[j]  ~ dnorm(mu,tau2.inv)
theta.pred[j] <- theta.adj.pred[j] + beta*( (j-3)/2  -mean(phi[1:12]))
}

}
"
writeLines(model.jag.independent, con="jag_model_independent.txt")

jags_data<-list(nt=dat$nt,nc=dat$nc,rc=dat$rc,rt=dat$rt)
params.jags.noncen <-c("beta","mu","tau","phi0",
                       "phi","theta","tau2.inv",
                       " theta.pred","theta.adj",
                       "rr","rbase","theta.adj.pred")

# Initial values; need to give starting values to all unknowns
inits<-function(){
  list(mu=0,tau=1,beta=0.5)  #  independent baselines
}

rjags.mod.noncen <- jags.model(
  file = "jag_model_independent.txt",
  data = jags_data,
  inits=inits,
  n.chains = 1,
  n.adapt = 4000
)

# burn-in
update(rjags.mod.noncen, n.iter = 4000)

# sampling/monitoring
fit.independent <- coda.samples(
  model = rjags.mod.noncen,
  variable.names = params.jags.noncen,
  n.iter = 10000,
  thin = 1
)
rr.independent <- MCMCsummary(fit.independent,round = 2,
                              Rhat = FALSE, params = c('rr'))
rbase.independent <- MCMCsummary(fit.independent,round = 2,
                                 Rhat = FALSE,params = c('rbase'))
phi.independent <- MCMCsummary(fit.independent,round = 2,
                               Rhat = FALSE,params = c('phi'))
theta.independent <- MCMCsummary(fit.independent,round = 2,
                                 Rhat = FALSE, params = c('theta'))
beta.independent <- MCMCsummary(fit.independent,round = 2,
                                Rhat = FALSE,params = c('beta'))
phi0.independent <- MCMCsummary(fit.independent,round = 2,
                                Rhat = FALSE,params = c('phi0'))
tau.independent <- MCMCsummary(fit.independent,round = 2,
                               Rhat = FALSE,params = c('tau'))


## ----Hyper_b,echo=F,fig.height=3.3, warning=FALSE,error = FALSE, message=FALSE,results=F----
x.independent <- rbase.independent[,"50%"]
y.independent <- rr.independent[,"50%"]

plot(x.independent,y.independent,
     type="n",axes=FALSE,
     xlim=c(0.36,35.0),ylim=c(0.5,5.0),
     ylab="Rate ratio",
     main="(b) Fitted data, independent baselines",
     xlab="Control group rate per 1000 patient years",
     log="xy")

text(x.independent,y.independent,label=ind,cex=0.8)
abline(h=1,lty=2)

axis(2, at=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0),
     labels=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0),
     las=2, cex.axis=0.7, tck=-.01)
axis(1, at=c(0.5,1.0,2.0,5.0,10.0,15.0,25.0,35.0),
     labels=c(0.5,1.0,2.0,5.0,10.0,15.0,25.0,35.0),
     cex.axis=0.7, tck=-.01)

phi_pred <- seq(from=-1, to=3.5, by=0.5)
pred_indep <- MCMCsummary(fit.independent,round = 2, Rhat = FALSE,
                          params = c('theta.pred'))

lines(exp(phi_pred), exp(pred_indep[ ,"50%"]))
lines(exp(phi_pred), exp(pred_indep[ ,"97.5%"]), lty="dashed")
lines(exp(phi_pred), exp(pred_indep[ ,"2.5%"]), lty="dashed")


## ----Hyper_c_pre,  warning=FALSE,error = FALSE, message=FALSE,results=F----
#Model with the assamption of exchangeable \phi_i with a normal distribution

#model with likelihood and prior
model.jag.exchange <- "
model
{
for(i in 1:12){
rt[i]~dpois(mt[i])
rc[i]~dpois(mc[i])
log(mc[i]) <-log(nc[i]/1000) + phi[i]
log(mt[i]) <-log(nt[i]/1000) + phi[i] + theta[i]
theta[i] <- theta.adj[i] + beta*(phi[i]-mean(phi[1:12]))
theta.adj[i]  ~ dnorm(mu,tau2.inv)

#prior for baseline risk phi
phi[i] ~ dnorm(mu.phi, tau2.phi.inv)
log(rr[i])<-  theta[i]
log(rbase[i])<-phi[i]
}

#baseline analysis, mu and tau are given uniforms
mu  ~ dunif(-10,10)
tau2.inv <- 1/(tau*tau)
tau ~ dunif(0,10)

mu.phi  ~ dunif(-10,10)
tau2.phi.inv <- 1/(tau.phi*tau.phi)
tau.phi ~ dunif(0,10)


beta  ~ dunif(-10,10)
phi0 <- -mu/beta + mean(phi[1:12])
break<-exp(phi0)

#predict theta for phi = -1,-.5,0,...., 3.5
for(j in 1:10){
theta.adj.pred[j]  ~ dnorm(mu,tau2.inv)
theta.pred[j] <- theta.adj.pred[j] + beta*( (j-3)/2  -mean(phi[1:12]))
}
}
"
writeLines(model.jag.exchange, con="exchange_model.txt")

#data that goes into the model
jags_data<-list(nt=dat$nt,nc=dat$nc,rc=dat$rc,rt=dat$rt)
params.jags.noncen <-c("beta","mu","tau","phi0",
                       "phi","theta","tau2.inv",
                       " theta.pred","theta.adj",
                       "mu.phi","tau2.phi.inv","tau.phi",
                       "rr","rbase","theta.adj.pred")


# Initial values; need to give starting values to all unknowns
inits<-function(){
  list(mu=0,tau=1,beta=0.5,mu.phi=1,tau.phi=1)  # exchangeable balines
}

# model initiation
rjags.exchange <- jags.model(
  file = "exchange_model.txt",
  data = jags_data,
  inits=inits,
  n.chains = 1,
  n.adapt = 4000
)

# burn-in
update(rjags.exchange, n.iter = 4000)

# sampling/monitoring
fit.rjags.exchange <- coda.samples(
  model = rjags.exchange,
  variable.names = params.jags.noncen,
  n.iter = 10000,
  thin = 1
)

rr.exchange <- MCMCsummary(fit.rjags.exchange,round = 2,
                           Rhat = FALSE,params = c('rr'))
rbase.exchange <- MCMCsummary(fit.rjags.exchange,round = 2,
                              Rhat = FALSE,params = c('rbase'))
phi.exchange <- MCMCsummary(fit.rjags.exchange,round = 2,
                            Rhat = FALSE,params = c('phi'))
theta.exchange <- MCMCsummary(fit.rjags.exchange,round = 2,
                              Rhat = FALSE,params = c('theta'))
beta.exchange <- MCMCsummary(fit.rjags.exchange,round = 2,
                             Rhat = FALSE,params = c('beta'))
tau.exchange <- MCMCsummary(fit.rjags.exchange,round = 2,
                            Rhat = FALSE,params = c('tau'))
phi0.exchange <- MCMCsummary(fit.rjags.exchange,round = 2,
                             Rhat = FALSE,
                             params = c('phi0'))


## ----Hyper_c,fig.height=3.3,echo=F,warning=FALSE, message=FALSE,results=F----

x.exchange <- rbase.exchange[,"50%"]
y.exchange <- rr.exchange[,"50%"]
plot(x.exchange,y.exchange,
     type="n",axes=FALSE,
     xlim=c(0.36,35.0),ylim=c(0.5,5.0),
     ylab="Rate ratio",
     main="(c) Fitted data, exchangeable baselines",
     xlab="Control group rate per 1000 patient years",
     log="xy")

text(x.exchange,y.exchange,label=ind,cex=0.8)
abline(h=1,lty=2)

axis(2, at=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0),
     labels=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0),
     las=2, cex.axis=0.7, tck=-.01)
axis(1, at=c(0.5,1.0,2.0,5.0,10.0,15.0,25.0,35.0),
     labels=c(0.5,1.0,2.0,5.0,10.0,15.0,25.0,35.0),
     cex.axis=0.7, tck=-.01)

phi_pred <- seq(from=-1, to=3.5, by=0.5)
pred_exchange<- MCMCsummary(fit.rjags.exchange,round = 2, Rhat = FALSE,
                            params = c('theta.pred'))

lines(exp(phi_pred), exp(pred_exchange[ ,"50%"]))
lines(exp(phi_pred), exp(pred_exchange[ ,"97.5%"]), lty="dashed")
lines(exp(phi_pred), exp(pred_exchange[ ,"2.5%"]), lty="dashed")


## ----fig.height=3.3,echo=F,warning=FALSE,error = FALSE, message=FALSE----
plot(rate.c,exp(log.RaR),type="n",xlim=c(0.35,35.0),
     ylim=c(0.5,5.0),main="Comparison",axes=FALSE,
     xlab="Control group rate per 1000 patient years",
     ylab="Rate ratio",log="xy")
#text(rate.c,exp(log.RaR),label="a",cex=0.8)
points(rate.c,exp(log.RaR),cex=1.5,pch=1,col="black")
points(x.independent,y.independent,cex=1.5,pch=3,col="blue")
points(x.exchange,y.exchange,cex=1.5,pch=18,col="red")
lines(exp(phi_pred), exp(pred_indep[ ,"50%"]),col="blue")
lines(exp(phi_pred), exp(pred_exchange[ ,"50%"]),col="red")
abline(h=1,lty=2)
axis(2, at=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0),
     labels=c(0.5,1.0,1.5,2.0,2.5,3.0,3.5,4.0,4.5,5.0),
     las=2, cex.axis=0.7, tck=-.01)

axis(1, at=c(0.5,1.0,2.0,5.0,10.0,15.0,25.0,35.0),
     labels=c(0.5,1.0,2.0,5.0,10.0,15.0,25.0,35.0),
     cex.axis=0.7, tck=-.01)
legend("topright",col=c("black","blue","red"),
       pch=c(1,3,18),legend=c("observed","independent baseline",
                              "exchangeable baseline"),cex = 0.75,
       bty = "n")


## ----echo=F,warning=FALSE,error = FALSE, message=FALSE-------------------
break.independent <- round(exp(phi0.independent[,c("2.5%",
                                                   "50%","97.5%")]),digits = 2)
break.exchange <- round(exp(phi0.exchange[,c("2.5%",
                                             "50%","97.5%")]),digits = 2)
