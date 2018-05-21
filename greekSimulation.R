# simulating alphas and betas for multiple logistic regression


# install.packages("extraDistr")
# install.packages("matrixcalc")
library(extraDistr)
library(MASS)
library(matrixcalc)

# define model parameters
simulate_model_parameters <- function(data,
																			inputVariables,
																			targetVariables,
																			alpha.location = 0,
																			alpha.scale = 10,
																			tau.scale = 1,
																			psi.scale = 1,
																			sigma.scale = 2.5,
																			omega.scale = 1) {
	
	targetVariables.n = length(targetVariables) # k
	inputVariables.n = length(inputVariables) # j
	
	########################################################################
	### ALPHA simulation is easy
	alpha <- rcauchy(targetVariables.n,
									 alpha.location,
									 alpha.scale) #length k
	names(alpha) <- targetVariables
	
	########################################################################
	### BETA
	# Shrinkage Scalar r (via psi and tau)
	psi <- rhcauchy(1, psi.scale)
	tau <- rhcauchy(inputVariables.n, tau.scale)
	r <- tau * psi # length j
	
	# standard deviation
	sigma.sd <- matrix(rhcauchy(inputVariables.n * targetVariables.n, sigma.scale),
										 nrow = inputVariables.n,
										 ncol = targetVariables.n) # j x k matrix
	
	# now draw correlation matrix from LKJ distribution
	# no function for LKJ distribution found
	# so I implemented it myself
	source("LKJdistribution.R")
	
	omega <- list()
	for (j in 1:inputVariables.n) {
		omega[[j]] <- rlkj(targetVariables.n, omega.scale) #k x k matrix, j times
	}
	
	# create covariance matrix bigSigma
	sigma.cov <- list()
	
	for (j in 1:inputVariables.n) {
		sigma.cov[[j]] <- diag(sigma.sd[j,]) %*% omega[[j]] %*% diag(sigma.sd[j,])
	}
	
	
	# finally beta (j x k matrix)
	beta <- matrix(NA, nrow=inputVariables.n, ncol=targetVariables.n)
	for (j in 1:inputVariables.n) {
		beta[j,] <- mvrnorm(1,
											  rep(0, times=targetVariables.n), 
											  (r[j]^2*sigma.cov[[j]]))
	}
	
	colnames(beta) <- targetVariables
	rownames(beta) <- inputVariables
	
	return(list(alpha = alpha,
							beta = beta,
							psi = psi,
							tau = tau,
							r = r,
							sigma = sigma.sd,
							omega = omega,
							SIGMA = sigma.cov))
}
