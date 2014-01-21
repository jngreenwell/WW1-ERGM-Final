model7 <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)
results7 <- ergm(model7,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results7)

mcmc.diagnostics(results7,vars.per.page=6)
dev.off()

fit7=gof(results7~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit7)
plot(fit7)

#####################################################

model7.1 <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)
results7.1 <- ergm(model7.1,burnin=500000,MCMCsamplesize=50000,parallel=4,constraints=~bd(maxout=10))
summary(results7.1)

mcmc.diagnostics(results7.1,vars.per.page=6)
dev.off()

fit7.1=gof(results7.1~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit7.1)
plot(fit7.1)