###################
#This model11 combines drinking and smoking prameters into one model - with maximum parameters from Models 9 and 10 (nodematch parameters are not differential) Also, mutual and asymmetric are PersonSmoke - the next model should be PersonDrink

model11 <- mat1~edges+twopath+ctriple+mutual+mutual('PersonSmoke')+asymmetric('PersonSmoke')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeofactor('Ethnic')+nodeifactor('Ethnic')+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)
results11<- ergm(model11,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results11)

mcmc.diagnostics(results11,vars.per.page=6)
dev.off()

fit11=gof(results11~distance+espartners,triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit11)
plot(fit11)

#######################################
model11.1 <- mat1~edges+twopath+ctriple+mutual+mutual('PersonDrink')+asymmetric('PersonDrink')+gwidegree(fixed=F)+gwodegree(fixed=F)+gwesp(.25,fixed=T)+gwdsp(.25,fixed=TRUE)+nodematch('Gender')+nodeifactor('Gender')+nodeofactor('Gender')+nodematch('Ethnic')+nodeofactor('Ethnic')+nodeifactor('Ethnic')+nodematch('PersonSmoke')+nodeifactor('PersonSmoke',base=1)+nodeofactor('PersonSmoke',base=1)+nodematch('PersonDrink')+nodeifactor('PersonDrink',base=1)+nodeofactor('PersonDrink',base=1)
results11.1<- ergm(model11.1,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results11.1)

mcmc.diagnostics(results11.1,vars.per.page=6)
dev.off()

fit11.1=gof(results11.1~distance+espartners,triadcensus,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit11.1)
plot(fit11.1)

###############################33

pdf("8.3_lab8_mcmc_m2.pdf")
mcmc.diagnostics(m2)