n = 2002

source("greekSimulation.R")
source("inputSimulation.R")

logit.theta <- matrix(NA, nrow = n, ncol = targetVariables.n)
theta <- matrix(NA, nrow = n, ncol = targetVariables.n)
y <- matrix(NA, nrow = n, ncol = targetVariables.n)

for (j in 1:n) { # patient
	for (k in 1:targetVariables.n) { # outcome
		logit.theta[j,k]<- alpha[k] + beta[,k] %*% as.numeric(inputData[j,2:6])
		theta[j,k] <- 1/(1+exp(-logit.theta[j,k]))
		y[j,k] <- rbern(1, theta[j,k])
	}
}



