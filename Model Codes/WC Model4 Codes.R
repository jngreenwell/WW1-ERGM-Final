model4 <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Ethnic')+nodeifactor('Ethnic',base=1)+nodeofactor('Ethnic',base=1)
results4 <- ergm(model4,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results4)
mcmc.diagnostics(results4,vars.per.page=6)
dev.off()

fit4=gof(results4~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit4)
plot(fit4)

##########################################################
#Model4.1 is same as the base model but removes the base terms from the nodefactor parameters

model4.1 <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Ethnic')+nodeifactor('Ethnic')+nodeofactor('Ethnic')
results4.1 <- ergm(model4.1,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results4.1)
mcmc.diagnostics(results4.1,vars.per.page=6)
dev.off()

fit4.1=gof(results4.1~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit4.1)
plot(fit4.1)

############################################

#Model4.2 keeps the base the same except it changes the fixed feature in gwi/gwodree to F
model4.2 <- mat1~edges+mutual+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Ethnic')+nodeifactor('Ethnic',base=1)+nodeofactor('Ethnic',base=1)
results4.2 <- ergm(model4.2,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results4.2)
mcmc.diagnostics(results4.2,vars.per.page=6)
dev.off()

fit4.2=gof(results4.2~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit4.2)
plot(fit4.2)