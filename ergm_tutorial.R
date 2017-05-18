## ----setup, include=FALSE------------------------------------------------
library(ergm)
library(knitr)
knitr::opts_chunk$set(cache=F, comment=NA)

## ----eval=FALSE----------------------------------------------------------
## install.packages('statnet')
## library(statnet)

## ----eval=FALSE----------------------------------------------------------
## install.packages('ergm') # will install the network package
## install.packages('sna')

## ----eval=FALSE----------------------------------------------------------
## update.packages('name.of.package')

## ----eval=FALSE----------------------------------------------------------
## install.packages('coda')

## ----results='hide', message=FALSE---------------------------------------
library(statnet)

## ----results='hide', message=FALSE---------------------------------------
library(ergm)
library(sna)
library(coda)

## ----eval=FALSE----------------------------------------------------------
## # latest versions:  ergm 3.6.0 and network 1.13.0 (as of 4/03/2016)
## sessionInfo()

## ------------------------------------------------------------------------
set.seed(0)

## ------------------------------------------------------------------------
data(package='ergm') # tells us the datasets in our packages

## ------------------------------------------------------------------------
data(florentine) # loads flomarriage and flobusiness data
flomarriage # Let's look at the flomarriage network properties
par(mfrow=c(1,2)) # Setup a 2 panel plot (for later)
plot(flomarriage, main="Florentine Marriage", cex.main=0.8) # Plot the flomarriage network
summary(flomarriage~edges) # Look at the $g(y)$ statistic for this model
flomodel.01 <- ergm(flomarriage~edges) # Estimate the model 
summary(flomodel.01) # The fitted model object

## ------------------------------------------------------------------------
summary(flomarriage~edges+triangle) # Look at the g(y) stats for this model
flomodel.02 <- ergm(flomarriage~edges+triangle) 
summary(flomodel.02)

## ------------------------------------------------------------------------
class(flomodel.02) # this has the class ergm

names(flomodel.02) # the ERGM object contains lots of components.

## ------------------------------------------------------------------------
flomodel.02$coef # you can extract/inspect individual components

## ------------------------------------------------------------------------
wealth <- flomarriage %v% 'wealth' # %v% references vertex attributes
wealth
summary(wealth) # summarize the distribution of wealth
plot(flomarriage, vertex.cex=wealth/25, main="Florentine marriage by wealth", cex.main=0.8) # network plot with vertex size proportional to wealth
summary(flomarriage~edges+nodecov('wealth')) 
# observed statistics for the model
flomodel.03 <- ergm(flomarriage~edges+nodecov('wealth'))
summary(flomodel.03)

## ------------------------------------------------------------------------
data(faux.mesa.high) 
mesa <- faux.mesa.high

## ------------------------------------------------------------------------
mesa
par(mfrow=c(1,1)) # Back to 1-panel plots
plot(mesa, vertex.col='Grade')
legend('bottomleft',fill=7:12,legend=paste('Grade',7:12),cex=0.75)

## ------------------------------------------------------------------------
fauxmodel.01 <- ergm(mesa ~edges + nodematch('Grade',diff=T) + nodematch('Race',diff=T))
summary(fauxmodel.01)

## ------------------------------------------------------------------------
table(mesa %v% 'Race') # Frequencies of race
mixingmatrix(mesa, "Race")

## ------------------------------------------------------------------------
summary(mesa ~edges + nodematch('Grade',diff=T) + nodematch('Race',diff=T))

## ------------------------------------------------------------------------
data(samplk) 
ls() # directed data: Sampson's Monks
samplk3
plot(samplk3)
summary(samplk3~edges+mutual)

## ------------------------------------------------------------------------
sampmodel.01 <- ergm(samplk3~edges+mutual)
summary(sampmodel.01)

## ------------------------------------------------------------------------
missnet <- network.initialize(10,directed=F)
missnet[1,2] <- missnet[2,7] <- missnet[3,6] <- 1
missnet[4,6] <- missnet[4,9] <- missnet[5,6] <- NA
summary(missnet)

# plot missnet with missing edge colored red. 
tempnet <- missnet
tempnet[4,6] <- tempnet[4,9] <- tempnet[5,6] <- 1
missnetmat <- as.matrix(missnet)
missnetmat[is.na(missnetmat)] <- 2
plot(tempnet,label = network.vertex.names(tempnet),edge.col = missnetmat)

summary(missnet~edges)
summary(ergm(missnet~edges))

## ------------------------------------------------------------------------
missnet_bad <- missnet
missnet_bad[4,6] <- missnet_bad[4,9] <- missnet_bad[5,6] <- 0
summary(missnet_bad)
summary(ergm(missnet_bad~edges))

## ----eval=FALSE----------------------------------------------------------
## help('ergm-terms')

## ------------------------------------------------------------------------
flomodel.03.sim <- simulate(flomodel.03,nsim=10)
class(flomodel.03.sim) 
summary(flomodel.03.sim)
length(flomodel.03.sim)
flomodel.03.sim[[1]]
plot(flomodel.03.sim[[1]], label= flomodel.03.sim[[1]] %v% "vertex.names")

## ------------------------------------------------------------------------
flo.03.gof.model <- gof(flomodel.03 ~ model)
flo.03.gof.model
plot(flo.03.gof.model)

## ------------------------------------------------------------------------
flo.03.gof.global <- gof(flomodel.03 ~ degree + esp + distance)
flo.03.gof.global
plot(flo.03.gof.global)

## ------------------------------------------------------------------------
mesamodel.b <- ergm(mesa~edges)
plot(gof(mesamodel.b ~ model, nsim=10))
plot(gof(mesamodel.b ~ degree + esp + distance, nsim=10))

## ------------------------------------------------------------------------
summary(flobusiness ~ edges+degree(1))
fit <- ergm(flobusiness ~ edges+degree(1))
mcmc.diagnostics(fit)

## ---- eval=FALSE---------------------------------------------------------
## ergm(flobusiness ~ edges+degree(1),
##      control=control.ergm(MCMC.interval=1))

## ------------------------------------------------------------------------
data('faux.magnolia.high')
magnolia <- faux.magnolia.high
plot(magnolia, vertex.cex=.5)
summary(magnolia ~ edges+triangle)

## ---- eval=F-------------------------------------------------------------
## ergm(magnolia ~ edges+triangle)

## ----eval=T--------------------------------------------------------------
fit.mag.01 <- ergm(magnolia ~ edges+triangle, control=control.ergm(MCMLE.maxit=2))


## ---- eval=T, results='hide',fig.show='asis'-----------------------------
mcmc.diagnostics(fit.mag.01)

## ------------------------------------------------------------------------
fit.mag.02 <- ergm(magnolia ~ edges+gwesp(0.25,fixed=T))
mcmc.diagnostics(fit.mag.02)

## ------------------------------------------------------------------------
gof.mag.02.model <- gof(fit.mag.02, GOF = ~model)
gof.mag.02.model
plot(gof.mag.02.model)


## ------------------------------------------------------------------------
fit.mag.03 <- ergm(magnolia ~ edges+gwesp(0.25,fixed=T)
                   +nodematch('Grade')+nodematch('Race')+nodematch('Sex'),
               control = control.ergm(MCMC.interval=20000), eval.loglik = F)
summary(fit.mag.03)

## ------------------------------------------------------------------------
mcmc.diagnostics(fit.mag.03)

## ------------------------------------------------------------------------
plot(gof(fit.mag.03, GOF=~model, control=control.gof.ergm(nsim=200)))

## ------------------------------------------------------------------------
plot(gof(fit.mag.03, GOF = ~ degree + esp + distance))

## ------------------------------------------------------------------------
ego.net <- network.initialize(500, directed=F)
ego.net %v% 'sex' <- c(rep(0,250),rep(1,250))

## ------------------------------------------------------------------------
ego.deg <- c(180, 245, 60, 15)    			# node distn
ego.mixmat <- matrix(c(164,44,26,176)/2, nrow=2, byrow=T)	# adjusted tie distn

## ------------------------------------------------------------------------
ego.edges <- sum(ego.mixmat)
ego.sexmatch <- ego.mixmat[1,1]+ego.mixmat[2,2]

## ------------------------------------------------------------------------
ego.target.stats <- c(ego.edges, ego.sexmatch)
ego.target.stats

## ------------------------------------------------------------------------
ego.fit <- ergm(ego.net ~ edges + nodematch('sex'),
                target.stats = ego.target.stats)

## ------------------------------------------------------------------------
summary(ego.fit) 

## ------------------------------------------------------------------------
ego.sim1 <- simulate(ego.fit)
plot(ego.sim1, vertex.cex=.65, vertex.col="sex")

## ------------------------------------------------------------------------
rbind(sim=summary(ego.sim1 ~ degree(c(0:3))), obs=ego.deg)
mixingmatrix(ego.sim1, "sex")
ego.mixmat

## ------------------------------------------------------------------------
ego.sim100 <- simulate(ego.fit, nsim=100)
ego.sim100

## ------------------------------------------------------------------------
sim.stats <- attr(ego.sim100,"stats")
rbind(sim=colMeans(sim.stats), obs=ego.target.stats)

## ------------------------------------------------------------------------
matplot(1:nrow(sim.stats), sim.stats, 
        pch=c("e","m","0","+"), cex=.65, 
        main="100 simulations from ego.fit model", sub="(default settings)",
        xlab="Replicate", ylab="frequency")
abline(h=ego.target.stats, col=c(1:4))

## ------------------------------------------------------------------------
sim.fulldeg <- summary(ego.sim100 ~ degree(c(0:10)))
colnames(sim.fulldeg) <- paste("deg",0:10, sep='')
sim.fulldeg[1:5,]

## ------------------------------------------------------------------------
sim.deg <- cbind(sim.fulldeg[,1:3], apply(sim.fulldeg[,4:11],1,sum))
colnames(sim.deg) <- c(colnames(sim.fulldeg)[1:3],"degree3+")
rbind(sim=colMeans(sim.deg), obs=ego.deg)

## ------------------------------------------------------------------------
matplot(1:nrow(sim.deg), sim.deg, pch=as.character(0:3), cex=.5,
        main="Comparing ego.sims to non-targeted degree frequencies",
        sub = "(only total edges targeted)",
        xlab = "Replicate", ylab = "Frequencies")
abline(h=c(180, 245, 60, 15), col=c(1:4))

## ------------------------------------------------------------------------
ego.isolates <- ego.deg[1]
ego.target.stats <- c(ego.edges, ego.sexmatch, ego.isolates)
ego.fit <- ergm(ego.net ~ edges + nodematch('sex') + degree(0),
                target.stats = ego.target.stats) 
summary(ego.fit)

## ------------------------------------------------------------------------
ego.sim100 <- simulate(ego.fit, nsim=100,
                       control=control.simulate.ergm(MCMC.interval=10000))
sim.stats <- attr(ego.sim100,"stats")
rbind(sim=colMeans(sim.stats), obs=ego.target.stats)

## ------------------------------------------------------------------------
sim.fulldeg <- summary(ego.sim100 ~ degree(c(0:10)))
sim.deg <- cbind(sim.fulldeg[,1:3], apply(sim.fulldeg[,4:11],1,sum))
colnames(sim.deg) <- c(colnames(sim.fulldeg)[1:3],"degree3+")
rbind(sim=colMeans(sim.deg), obs=ego.deg)

## ------------------------------------------------------------------------
matplot(1:nrow(sim.deg), sim.deg, pch=as.character(0:3), cex=.5,
        main="Comparing ego.sims to non-targeted degree frequencies",
        sub = "(only 0, 2+ and total edges targeted)",
        xlab = "Replicate", ylab = "Frequencies")
abline(h=c(180, 245, 60, 15), col=c(1:4))

