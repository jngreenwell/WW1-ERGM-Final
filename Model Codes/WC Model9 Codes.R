# This model 9 is for the attribute smoking and will be modelled with other variables as an approach to the final smoking model
##################################
#Model9 is the base model for the smoking attribute 

model9 <- mat1~edges+mutual('PersonSmoke')+asymmetric('PersonSmoke')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('MaoPI')+nodeofactor('MaoPI')+nodeifactor('MaoPI')+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)
results9 <- ergm(model9,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))

pdf('summaryresults9.pdf')
summary(results9)

pdf('ResultsModel9.pdf')
mcmc.diagnostics(results9,vars.per.page=6)
dev.off()

fit9=gof(results9~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit9)
pdf('fit9.pdf')
plot(fit9)



##############################################################
#########################################
#adds an additional unattributed mutual term to the base model
model9.1 <- mat1~edges+mutual+mutual('PersonSmoke')+asymmetric('PersonSmoke')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('MaoPI')+nodeofactor('MaoPI')+nodeifactor('MaoPI')+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)
results9.1 <- ergm(model9.1,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results9.1)

mcmc.diagnostics(results9.1,vars.per.page=6)
dev.off()

fit9.1=gof(results9.1~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit9.1)
plot(fit9.1)


#############################

#adds the ctriple parameter to 9.1

model9.2 <- mat1~edges+ctriple+mutual+mutual('PersonSmoke')+asymmetric('PersonSmoke')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('MaoPI')+nodeofactor('MaoPI')+nodeifactor('MaoPI')+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)
results9.2 <- ergm(model9.2,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results9.2)

mcmc.diagnostics(results9.2,vars.per.page=6)
dev.off()

fit9.2=gof(results9.2~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit9.2)
plot(fit9.2)

fit9.2.5=gof(results9.2)
plot(fit9.2.5)

#######################################

#adds the twopath parameter

model9.3 <- mat1~edges+twopath+ctriple+mutual+mutual('PersonSmoke')+asymmetric('PersonSmoke')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('MaoPI')+nodeofactor('MaoPI')+nodeifactor('MaoPI')+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)
results9.3 <- ergm(model9.3,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results9.3)

mcmc.diagnostics(results9.3,vars.per.page=6)
dev.off()

fit9.3=gof(results9.3~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit9.3)
plot(fit9.3)


##########################################

#replaces base model with ethnic rather than MaoPI like base model otherwise

model9.4 <- mat1~edges+mutual('PersonSmoke')+asymmetric('PersonSmoke')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeofactor('Ethnic')+nodeifactor('Ethnic')+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)
results9.4 <- ergm(model9.4,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results9.4)

mcmc.diagnostics(results9.4,vars.per.page=6)
dev.off()

fit9.4=gof(results9.4~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit9.4)
plot(fit9.4)

#############################################
##adds an additional unattributed mutual term to the base model also with Ethnic like 9.1 otherwise
model9.5 <- mat1~edges+mutual+mutual('PersonSmoke')+asymmetric('PersonSmoke')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeofactor('Ethnic')+nodeifactor('Ethnic')+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)
results9.5 <- ergm(model9.5,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results9.5)

mcmc.diagnostics(results9.5,vars.per.page=6)
dev.off()

fit9.5=gof(results9.5~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit9.5)
plot(fit9.5)
#####################################################
#adds the ctriple parameter to 9.1 like 9.2 and 9.3 otherwise and uses Ethnic 

model9.6 <- mat1~edges+ctriple+mutual+mutual('PersonSmoke')+asymmetric('PersonSmoke')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeofactor('Ethnic')+nodeifactor('Ethnic')+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)
results9.6 <- ergm(model9.6,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results9.6)

mcmc.diagnostics(results9.6,vars.per.page=6)
dev.off()

fit9.6=gof(results9.6~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit9.6)
plot(fit9.6)
###################

c(results9$mle.lik,results9.1$mle.lik,results9.2$mle.lik,results9.3$mle.lik,results9.4$mle.lik,results9.5$mle.lik,results9.6$mle.lik)

############################################
#adds the twopath to the previous mdoel
model9.7 <- mat1~edges+twopath+ctriple+mutual+mutual('PersonSmoke')+asymmetric('PersonSmoke')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeofactor('Ethnic')+nodeifactor('Ethnic')+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)
results9.7 <- ergm(model9.7,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results9.7)

mcmc.diagnostics(results9.7,vars.per.page=6)
dev.off()

fit9.7=gof(results9.7~distance+espartners,triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit9.7)
plot(fit9.7)
###################

c(results9$mle.lik,results9.1$mle.lik,results9.2$mle.lik,results9.3$mle.lik,results9.4$mle.lik,results9.5$mle.lik,results9.6$mle.lik)

##############################
#same as 9.7 but adds a differential homophily for PersonSmoke nodematch term
model9.8 <- mat1~edges+twopath+ctriple+mutual+mutual('PersonSmoke')+asymmetric('PersonSmoke')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeofactor('Ethnic')+nodeifactor('Ethnic')+nodematch('PersonSmoke',diff=T)+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)
results9.8 <- ergm(model9.8,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results9.8)

mcmc.diagnostics(results9.8,vars.per.page=6)
dev.off()

fit9.8=gof(results9.8~distance+espartners,triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit9.8)
  z   fghjlot(fit9.8)