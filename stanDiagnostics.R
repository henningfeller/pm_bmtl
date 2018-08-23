
# load("stanOne/stanFitBigChains.RData")
source("mcmc/DBDA2E-utilities.R")
library(rstan)

# Embedded functions of rstan
# openGraph()
# traceplot(stanFit,pars=c("alpha"))
# saveGraph(file=paste0(fileNameRoot,"StanTrace"),type="eps")
# openGraph()
# plot(stanFit,pars=c("alpha"))
# saveGraph(file=paste0(fileNameRoot,"StanPlot"),type="eps")

# Make graphs:
# For consistency with JAGS-oriented functions in DBDA2E collection,
# convert stan format to coda format. This excludes warmup and thinned steps.

mcmcSamples = mcmc.list( lapply( 1:ncol(stanFit) ,
															function(x) { mcmc(as.array(stanFit)[,x,]) } ) )

# # random (only one!)
# idxParam <- round(runif(1, 0.5, (length(parametersOut)+0.5)))
# var <- parametersOut[idxParam]
var <- "alpha[1]"

diagMCMC(mcmcSamples, var,
				 saveName = fileNameRoot, saveType = "eps" )

varValues <- c()
for (i in 1:length(mcmcSamples)) { # for each chain
	varValues <- c(varValues, mcmcSamples[[i]][,var])
}

plotPost(varValues, cenTend = "median", xlab = var, col= leuphanaPalette$fillQualitative$red1)
