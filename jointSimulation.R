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

#Set Number of patients
n = 1000


inputData <- simulation_of_input_data(n)

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
while( (sum(y[,1]) < 40) |
			  (sum(y[,2]) < 40) |
				 (sum(y[,3]) < 40) |
				  (sum(y[,1]) > 200)  |
				   (sum(y[,2]) > 200) |
				    (sum(y[,3]) > 200) ) {

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
	
	sums <- apply(y,2,sum)
	print(paste("Iteration: ", runningIdx, "(", paste(sums, collapse=" "), ")",sep=""))
}

colnames(y) <- targetVariables

# merge data frames
jointDataset <- cbind(inputData, y)

# and save
write.csv2(jointDataset, file = "patientsData/patientsComplete.csv")
save(model.parameters, file = "patientsData/model_parameters.Rdata")
