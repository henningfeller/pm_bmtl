# Remove current plots
graphics.off()

source("mcmc/DBDA2E-utilities.R")

allFilesPrefix = 'mcmcOut/'

if (is.null(finalDataset)) {
	source("jointSimulation.R")
}

y <- as.matrix(finalDataset[,c("STK", "AMI", "ARF")])



x <- finalDataset[,2:(ncol(finalDataset)-3)]
x <- standardize_mean0_var1(x)
x <- one_hot_encoding(x)
x <- as.matrix(x)

mcmcInputData = list(
	y = y,
	x = x,
	Ny = ncol(y),
	Nx = ncol(x),
	Ntotal = nrow(x)
)


########################################################################
# THE MODEL
########################################################################
modelString <- "
model {
	########################################################################	
	# Variables:
	# k:  				An adverse health event
	# j: 					A predictor
	# i:					A patient
	# Nx:					Number of predictors
	# Ny: 				Number of adverse health events
	# Ntotal:			Number of patients
	# y[i,k]: 	 	Patient i had adverse health event k
	# x[i,j];			Predictor value j of patient i
	# alpha[k]:  	Bias-Regression-Coefficient of adverse health event k
	# beta[j,k]: 	Regression Coefficient for predictor j for adverse health event k
	########################################################################



	# for ( k in 1:(NOutcome*Ntotal) ) {
	# 	y[k] = dbern(theta[k])
	# }
	

	########################################################################
	# Defining the likelihood
	for ( i in 1:Ntotal ) { # for each patient
		for ( k in 1:Ny ) { # for each event
			y[i,k] ~ dbern( theta[i, k] )
		
			# Probability based on logistic regression
			theta[i,k] <- ilogit( alpha[k] + sum( beta[1:Nx,k] * x[i, 1:Nx] ) )	
		}
	}
	########################################################################

	

	########################################################################
	# Defining the priors for the alphas:
	# Cauchy distribution is the same as a T distr with k=1 degrees of freedom
	# https://en.wikipedia.org/wiki/Cauchy_distribution#Related_distributions
	
	for (k in 1:Ny){ # for each event
		alpha[k] ~ dt(0, 10, 1) 
	}
	########################################################################
	

	
	########################################################################
	# Defining the priors for the betas
	for (j in 1:Nx){ # for each predictor
		### TODO beta must be drawn from MV-Norm
		# for (n in 1:Ndim){
		# 	beta[m,n] ~ dmnorm(0,pow(r[n],2)*SIGMA[n])
		# }

		### as SIGMA is not yet defined, a boring normal
		for (k in 1:Ny) { # for each outcome
			beta[j,k] ~ dnorm(0, (1/2)^2)
		}
	}
	########################################################################
	


	########################################################################
	# Defining the formulae for 'r' and it's priors
	for (j in 1:Nx){
		r[j] = tau[j] * psi
		
		# Half cauchy are truncated cauchy dist
		# https://stackoverflow.com/questions/34935606/cauchy-prior-in-jags/34951349
		tau[j] ~ dt(0, 1, 1)T(0,) 
	}
	
	psi ~ dt(0, 1, 1)T(0,)
	########################################################################

	### TODO:
	### SIGMA = diag(sigma[j]) %*% OMEGA[j] %*% diag(sigma[j])
	### sigma ~ dt(0, 2.5, 1)
	### OMEGA ~ lkj
}"

writeLines( modelString , con="TEMPmodel.txt" )

########################################################################
# Sampling
########################################################################
parametersIn <- c("alpha", "beta", "psi", "tau", "r")

runJagsOut  <- run.jags(data = mcmcInputData,
												model = "TEMPmodel.txt",
												monitor = parametersIn,
											  method = "parallel",
												n.chains = 3,
												adapt = 1000,
												burnin = 4000,
												sample = 10000, # per chain
												thin = 2 # save only each third step
												)


########################################################################
# DIAGNOSIS
########################################################################
mcmcSamples <- as.mcmc.list(runJagsOut)

# Giving proper names
theColnames <- colnames(mcmcSamples[[1]])
for (i in 1:length(theColnames)) {
	if (substr(theColnames[i], 1, 5) == "alpha") {
		indexStr <- substr(theColnames[i], 7, (nchar(theColnames[i])-1))
		indexK <- as.integer(indexStr)
		theColnames[i] <- paste("alpha[", targetVariables[indexK], "]", sep="")
	} else if (substr(theColnames[i],1,4) == "beta") {
		indexStr <- substr(theColnames[i], 6, (nchar(theColnames[i])-1))
		indexStr <- strsplit(indexStr, split = ",")
		indexJ <- as.integer(indexStr[[1]][1])
		indexK <- as.integer(indexStr[[1]][2])
		theColnames[i] <- paste("beta[", inputVariables[indexJ], ",", targetVariables[indexK], "]", sep="")
	} else if (substr(theColnames[i],1,2) == "r[") {
		indexStr <- substr(theColnames[i], 3, (nchar(theColnames[i])-1))
		indexJ <- as.integer(indexStr)
		theColnames[i] <- paste("r[", inputVariables[indexJ], "]", sep="")
	} else if	(substr(theColnames[i],1,3) == "tau") {
		indexStr <- substr(theColnames[i], 5, (nchar(theColnames[i])-1))
		indexJ <- as.integer(indexStr)
		theColnames[i] <- paste("tau[", inputVariables[indexJ], "]", sep="")
	}
}

for (i in 1:length(mcmcSamples)) { # for all chains
	colnames(mcmcSamples[[i]]) <- theColnames
}

parametersOut <- varnames(mcmcSamples)

# all (many!)
# for (var in parametersOut) {
# 	diagMCMC(mcmcSamples, var,
# 					 saveName = allFilesPrefix, saveType = "eps")
# }

# random (only one!)
idxParam <- round(runif(1, 0.5, (length(parametersOut)+0.5)))
var <- parametersOut[idxParam]
diagMCMC(mcmcSamples, var,
				 saveName = allFilesPrefix, saveType = "eps")

varValues <- c()
for (i in 1:length(mcmcSamples)) { # for each chain
	varValues <- c(varValues, mcmcSamples[[i]][,var])
}

plotPost(varValues, cenTend = "median", xlab = var)
	
