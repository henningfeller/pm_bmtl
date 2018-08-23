rm(list = ls())
graphics.off()

# install.packages("dplyr")
# install.packages("extraDistr")
# install.packages("matrixcalc")
library(dplyr)

source("inputSimulation.R")
source("standardizationFunctions.R")
source("greekSimulation.R")
source("themeLeuphana.R")


# Generate small data set to ensure right amount of events
N <- 1000
eventMin <- 0.04*N
eventMax <- 0.2*N
inputData <- simulation_of_input_data(N)

########################################################
# Data Standardization
# - Continuous Variables to Mean 0, SD 1/2
# - Categorical Variables to one 0/1 vector per category

stdInputData <- standardize_mean0_var1(inputData)
stdInputData <- one_hot_encoding(stdInputData)


## Applyting the standardized data to the simulated parameters to produce Ys:
inputVariables = colnames(stdInputData)[-1];
targetVariables = c("STK", "AMI", "ARF")
	

y <- matrix(0, nrow = n, ncol = targetVariables.n)


runningIdx <- 0
while( (sum(y[,1]) < eventMin) |
			  (sum(y[,2]) < eventMin) |
				 (sum(y[,3]) < eventMin) |
				  (sum(y[,1]) > eventMax)  |
				   (sum(y[,2]) > eventMax) |
				    (sum(y[,3]) > eventMax) ) {

runningIdx <- runningIdx + 1
	
					    	
	model.parameters <- simulate_model_parameters(inputVariables,
																								targetVariables )
	
	alpha <- model.parameters[["alpha"]]
	beta <- model.parameters[["beta"]]
	
	targetVariables.n <- length(targetVariables) 
	
	logit.theta <- matrix(NA, nrow = n, ncol = targetVariables.n)
	theta <- matrix(NA, nrow = n, ncol = targetVariables.n)
	y <- matrix(0, nrow = n, ncol = targetVariables.n)
	
	for (j in 1:n) { # patient
		for (k in 1:targetVariables.n) { # outcome
			logit.theta[j,k]<- alpha[k] + beta[,k] %*% as.numeric(stdInputData[j,-1])
			theta[j,k] <- 1/(1+exp(-logit.theta[j,k]))
			y[j,k] <- rbern(1, theta[j,k])
		}
	}
	
	colnames(y) <- targetVariables
	
	sums <- apply(y,2,sum)
	print(paste("Iteration: ", runningIdx, "(", paste(sums, collapse=" "), ")",sep=""))
}

# Now right amount of adverse health events would be generated.
# Generate new dataset
N <- 20000
inputData <- simulation_of_input_data(N)

# standardize
stdInputData <- standardize_mean0_var1(inputData)
stdInputData <- one_hot_encoding(stdInputData)

# alpha, beta and co are known
# simulate y
logit.theta <- matrix(NA, nrow = N, ncol = targetVariables.n)
theta <- matrix(NA, nrow = N, ncol = targetVariables.n)
y <- matrix(0, nrow = N, ncol = targetVariables.n)

for (j in 1:N) { # patient
	for (k in 1:targetVariables.n) { # outcome
		logit.theta[j,k]<- alpha[k] + beta[,k] %*% as.numeric(stdInputData[j,-1])
		theta[j,k] <- 1/(1+exp(-logit.theta[j,k]))
		y[j,k] <- rbern(1, theta[j,k])
	}
}

colnames(y) <- targetVariables

# double checking
apply(y,2,sum)

# merge data frames
jointDataset <- cbind(inputData, y)

# and save
write.csv2(jointDataset, file = "patientsData/patientsComplete.csv")
save(model.parameters, file = "patientsData/model_parameters.Rdata")
