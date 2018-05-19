# simulating alphas and betas for multiple logistic regression


# install.packages("extraDistr")
# install.packages("matrixcalc")
library(extraDistr)
library(MASS)
library(matrixcalc)

# define model parameters

targetVariables = c("STK", "AMI", "ARF")
targetVariables.n = length(targetVariables) # k

inputVariables = c("gender", "age", "height", "weight", "bmi")
inputVariables.n = length(inputVariables) # j

########################################################################
### ALPHA simulation is easy
alpha <- rcauchy(targetVariables.n, 0, 10) #length k
names(alpha) <- targetVariables

########################################################################
### BETA
# Shrinkage Scalar r (via psi and tau)
psi <- rhcauchy(1, 1)
tau <- rhcauchy(inputVariables.n, 1)
r <- tau * psi # length j

# standard deviation
sd_sigma <- matrix(rhcauchy(inputVariables.n * targetVariables.n, 2.5),
									 nrow = inputVariables.n,
									 ncol = targetVariables.n) # j x k matrix

# now draw correlation matrix from LKJ distribution
# no function for LKJ distribution found
# so I implemented it myself
source("LKJdistribution.R")

omega <- list()
for (j in 1:inputVariables.n) {
	omega[[j]] <- rlkj(targetVariables.n) #k x k matrix, j times
}

# create covariance matrix bigSigma
cov_Sigma <- list()

for (j in 1:inputVariables.n) {
	cov_Sigma[[j]] <- diag(sd_sigma[j,]) %*% omega[[j]] %*% diag(sd_sigma[j,])
}


# finally beta (j x k matrix)
beta <- matrix(NA, nrow=inputVariables.n, ncol=targetVariables.n)
for (j in 1:inputVariables.n) {
	beta[j,] <- mvrnorm(1,
										  rep(0, times=targetVariables.n), 
										  (r[j]^2*cov_Sigma[[j]]))
}

colnames(beta) <- targetVariables
rownames(beta) <- inputVariables

beta
alpha
