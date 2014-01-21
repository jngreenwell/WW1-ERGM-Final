model6 <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('AlcOk')+nodeifactor('AlcOk',base=1)+nodeofactor('AlcOk',base=1)
results6 <- ergm(model6,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results6)

mcmc.diagnostics(results6,vars.per.page=6)
dev.off()

fit6=gof(results6~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit6)
plot(fit6)