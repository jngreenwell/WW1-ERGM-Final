#model3 is the base model to test the exogenous covariate 'MaoPI' 
model3 <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('MaoPI')+nodeifactor('MaoPI',base=1)+nodeofactor('MaoPI',base=1)
results3 <- ergm(model3,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results3)
mcmc.diagnostics(results3,vars.per.page=6)
dev.off()

fit3=gof(results3~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit3)
plot(fit3)

#############################################
#removes the base feature from the nodei/ofactor parameters
model3.1 <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('MaoPI')+nodeifactor('MaoPI')+nodeofactor('MaoPI')
results3.1 <- ergm(model3.1,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results3.1)
mcmc.diagnostics(results3.1,vars.per.page=6)
dev.off()

fit3.1=gof(results3.1~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit3.1)
plot(fit3.1)


#############################################
#removes the fixed term from the gwi/gwodegree parameters and replaces the base terms to the nodei/ofactor paramters 
model3.2 <- mat1~edges+mutual+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('MaoPI')+nodeifactor('MaoPI',base=1)+nodeofactor('MaoPI',base=1)
results3.2 <- ergm(model3.2,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results3.2)
mcmc.diagnostics(results3.2,vars.per.page=6)
dev.off()

fit3.2=gof(results3.2~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit3.2)
plot(fit3.2)


##############################################

#updates coeficients from 3.1 


results3.3 <- ergm(model3.1,theta0=results3.1$coef,burnin=500000,MCMCsamplesize=30000,parallel=4,constraints=~bd(maxout=10))
summary(results3.3)
mcmc.diagnostics(results3.3,vars.per.page=6)
dev.off()

fit3.3=gof(results3.3~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit3.3)
plot(fit3.3)

#########################################

#removes the fixed term from the gwi/gwodegree parameters like model3.2 and replaces the base terms to the nodei/ofactor paramters but increases the burnin and sample size from 3.2
model3.4 <- mat1~edges+mutual+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('MaoPI')+nodeifactor('MaoPI',base=1)+nodeofactor('MaoPI',base=1)
results3.4 <- ergm(model3.4,burnin=500000,MCMCsamplesize=30000,parallel=4,constraints=~bd(maxout=10))
summary(results3.4)
mcmc.diagnostics(results3.4,vars.per.page=6)
dev.off()

fit3.4=gof(results3.4~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit3.4)
plot(fit3.4)


