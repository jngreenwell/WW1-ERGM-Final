model5 <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('CigsOk')+nodeifactor('CigsOk',base=1)+nodeofactor('CigsOk',base=1)
results5 <- ergm(model5,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results5)

mcmc.diagnostics(results5,vars.per.page=6)
dev.off()

fit5=gof(results5~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit5)
plot(fit5)

################################

model5.1 <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('CigsOk')+nodeifactor('CigsOk',base=1)+nodeofactor('CigsOk',base=1)
results5.1 <- ergm(model5.1,burnin=500000,MCMCsamplesize=30000,parallel=4,constraints=~bd(maxout=10))
summary(results5.1)

mcmc.diagnostics(results5.1,vars.per.page=6)
dev.off()

fit5.1=gof(results5.1~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit5.1)
plot(fit5.1)