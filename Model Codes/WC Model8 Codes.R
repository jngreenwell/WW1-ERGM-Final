model8 <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)
results8 <- ergm(model8,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results8)

mcmc.diagnostics(results8,vars.per.page=6)
dev.off()

fit8=gof(results8~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit8)
plot(fit8)

#####################################################

model8.1 <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('PersonSmoke')+nodematch('PersonDrink')+nodematch('Gender')+nodematch('Ethnic')
results8.1 <- ergm(model8.1,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results8.1)

mcmc.diagnostics(results8,vars.per.page=6)
dev.off()

fit8=gof(results8~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit8)
plot(fit8)
