# This model 10 is for the attribute drinking and will be modelled with other variables as an approach to the final smoking model
##################################
#Model10 is the base model for the smoking attribute 

model10 <- mat1~edges+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('MaoPI')+nodeofactor('MaoPI')+nodeifactor('MaoPI')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)
results10 <- ergm(model10,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results10)

###############
pdf('modelresults10.pdf')
mcmc.diagnostics(results10)
##################

mcmc.diagnostics(results10,vars.per.page=11)
dev.off()

fit10=gof(results10~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit10)
plot(fit10)

pdf('GOFModel10.pdf')
plot(fit10)


##############################################################
#########################################
#adds an additional unattributed mutual and ctriple term to the base model

model10.1 <- mat1~edges+mutual+ctriple+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('MaoPI')+nodeofactor('MaoPI')+nodeifactor('MaoPI')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)
results10.1 <- ergm(model10.1,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results10.1)

mcmc.diagnostics(results10.1,vars.per.page=6)
dev.off()

fit10.1=gof(results10.1~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit10.1)
plot(fit10.1)


#######################################

#adds the twopath parameter
model10.2 <- mat1~edges+mutual+ctriple+twopath+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('MaoPI')+nodeofactor('MaoPI')+nodeifactor('MaoPI')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)
results10.2 <- ergm(model10.2,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results10.2)

mcmc.diagnostics(results10.2,vars.per.page=6)
dev.off()

fit10.2=gof(results10.2~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit10.2)
plot(fit10.2)


##########################################

#replaces base model with ethnic rather than MaoPI like base model otherwise

model10.3 <- mat1~edges+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeofactor('Ethnic')+nodeifactor('Ethnic')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)
results10.3 <- ergm(model10.3,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results10.3)

mcmc.diagnostics(results10.3,vars.per.page=6)
dev.off()

fit10.3=gof(results10.3~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit10.3)
plot(fit10.3)



#############################################
##adds an additional unattributed mutual term to the base model also with Ethnic like 9.1 otherwise
model10.4 <- mat1~edges+mutual+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeofactor('Ethnic')+nodeifactor('Ethnic')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)
results10.4 <- ergm(model10.4,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results10.4)

mcmc.diagnostics(results10.4,vars.per.page=6)
dev.off()

fit10.4=gof(results10.4~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit10.4)
plot(fit10.4)




#####################################################
#adds the ctriple and twopath parameters to 9.1 like 9.2 and 9.3 otherwise
model10.5 <- mat1~edges+ctriple+twopath+mutual+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeofactor('Ethnic')+nodeifactor('Ethnic')+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)
results10.5 <- ergm(model10.5,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results10.5)

mcmc.diagnostics(results10.5,vars.per.page=6)
dev.off()

fit10.5=gof(results10.5~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit10.5)
plot(fit10.5)

#############################
#Adds differential homophily to the PersonDrink parameter otherwise like 10.5

#adds the ctriple and twopath parameters to 9.1 like 9.2 and 9.3 otherwise
model10.6 <- mat1~edges+ctriple+twopath+mutual+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeofactor('Ethnic')+nodeifactor('Ethnic')+nodematch('PersonDrink',diff=T)+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)
results10.6 <- ergm(model10.6,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results10.6)

mcmc.diagnostics(results10.6,vars.per.page=6)
dev.off()

fit10.6=gof(results10.6~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit10.6)
plot(fit10.6)


