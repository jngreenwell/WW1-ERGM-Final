#model1.1 is the base strucural model 
model1.1 <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.5,fixed=T)+gwdsp(.5,fixed=TRUE)
results1.1 <- ergm(model1.1,burnin=200000,MCMCsamplesize=30000,parallel=4,constraints=~bd(maxout=10))
summary(results1.1)
mcmc.diagnostics(results1.1,vars.per.page=6)
dev.off()

#lowers sample size
model1.1a <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.5,fixed=T)+gwdsp(.5,fixed=TRUE)
results1.1a <- ergm(model1.1a,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results1.1a)
mcmc.diagnostics(results1.1a,vars.per.page=6)
dev.off()

#removes .5 from gwesp and gwdsp
model1.1b <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(fixed=T)+gwdsp(fixed=TRUE)
results1.1b <- ergm(model1.1b,burnin=200000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results1.1b)
mcmc.diagnostics(results1.1b,vars.per.page=6)
dev.off()

fit1.1b=gof(results1.1b)
summary(fit1.1b)
plot(fit1.1b)

fit1.1b.5=gof(results1.1b~distance+espartners,nsim=40,burnin=200000)
dev.off()
par(mfrow=c(2,2))
summary(fit1.1b.5)
plot(fit1.1b.5)


#model1.2 updates the parameters from 1.1b and then increases burnin
results1.2 <- ergm(model1.1b,theta0=results1.1b$coef,burnin=500000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results1.2)
mcmc.diagnostics(results1.2,vars.per.page=6)



#same as model1.1a but updates those coefficients and then increases the burnin
results1.2a <- ergm(model1.1a,theta0=results1.1a$coef,burnin=500000,MCMCsamplesize=10000,parallel=4,constraints=~bd(maxout=10))
summary(results1.2a)
mcmc.diagnostics(results1.2,vars.per.page=6)

#same as 1.2a but increases sample size to 30000
results1.2b <- ergm(model1.1a,theta0=results1.1a$coef,burnin=500000,MCMCsamplesize=30000,parallel=4,constraints=~bd(maxout=10))
summary(results1.2b)
mcmc.diagnostics(results1.2,vars.per.page=6)

c(results1.2a$mle.lik,results1.2b$mle.lik,results1.2c$mle.lik,results1.2c$mle.lik)


#updates coefficients from 1.1a but updates the MCMC to use the TNT feature
results1.2c <- ergm(model1.1a,theta0=results1.1a$coef,burnin=500000,MCMCsamplesize=10000,parallel=4,MCMC.interval=1000,constraints=~bd(maxout=10),control=control.ergm(prop.weights='TNT'))
summary(results1.2c)
mcmc.diagnostics(results1.2,vars.per.page=6)


#updates coefficients from 1.1a and includes same terms from 1.2c but increases interval of 5000
results1.2d <- ergm(model1.1a,theta0=results1.1a$coef,burnin=500000,MCMCsamplesize=10000,parallel=4,MCMC.interval=5000,constraints=~bd(maxout=10),control=control.ergm(prop.weights='TNT'))
summary(results1.2d)
mcmc.diagnostics(results1.2,vars.per.page=6)


#same as 1.2c but raises the burn in to 1000000 and sample size to 50000
results1.2e <- ergm(model1.1a,theta0=results1.1a$coef,burnin=1000000,MCMCsamplesize=50000,parallel=4,MCMC.interval=1000,constraints=~bd(maxout=10),control=control.ergm(prop.weights='TNT'))
summary(results1.2e)
mcmc.diagnostics(results1.2e,vars.per.page=6)

c(results1.1a$mle.lik,results1.1b$mle.lik,results1.2a$mle.lik,results1.2b$mle.lik,results1.2c$mle.lik,results1.2d$mle.lik,results1.2e$mle.lik)

model1.1 <- mat1~edges+mutual+gwidegree(fixed=T)+gwodegree(fixed=T)+gwesp(.5,fixed=T)+gwdsp(.5,fixed=TRUE)
results1.1 <- ergm(model1.1,burnin=200000,MCMCsamplesize=30000,parallel=4,constraints=~bd(maxout=10))
summary(results1.1)
mcmc.diagnostics(results1.1,vars.per.page=4)


c(results1.1$mle.lik,results1.1a$mle.lik,results1.1b$mle.lik,results1.2$mle.lik,results1.2a$mle.lik,results1.2b$mle.lik,results1.2c$mle.lik,results1.2d$mle.lik,results1.2e$mle.lik)
