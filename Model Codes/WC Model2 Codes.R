#model2 is the base model to test the exogenous covariate 'Gender' 
model2 <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender',base=1)+nodeofactor('Gender',base=1)
results2 <- ergm(model2,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results2)
mcmc.diagnostics(results2,vars.per.page=6)
dev.off()

fit2=gof(results2~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit2)
plot(fit2)

#############################################

results2.1 <- ergm(model2,burnin=500000,MCMCsamplesize=30000,parallel=4,constraints=~bd(maxout=10))
summary(results2.1)
mcmc.diagnostics(results2.1,vars.per.page=6)
dev.off()

fit2.1=gof(results2.1~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))

summary(fit2.1)

plot(fit2.1)

################################################################

results2.2 <- ergm(model2,theta0=results2$coef,burnin=500000,MCMCsamplesize=30000,parallel=4,constraints=~bd(maxout=10))
summary(results2.2)
mcmc.diagnostics(results2.1,vars.per.page=6)
dev.off()

fit2.2=gof(results2.2~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))

summary(fit2.2)

plot(fit2.2)
