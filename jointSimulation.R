rm(list = ls())
graphics.off()

# install.packages("dplyr")
# install.packages("extraDistr")
# install.packages("matrixcalc")
library(dplyr)

source("inputSimulation.R")
source("greekSimulation.R")
source("themeLeuphana.R")


n = 2000 #Number of patients

inputData <- simulation_of_input_data(n)
inputData <- inputData %>% mutate(gender = ifelse(gender == 1, 'm','w')) # I needed to change this so the standardization function doesn't treat is as numeric variable



## STANDARDIZING CONTINUOUS VARIABLES AND ONEHOT ENCODING FOR FACTOR VARIABLES:

#Since the one hot encoding function takes non numeric variables and converts them to numeric variables, we want to perform this
#change after standardizing the numeric variables otherwise the standardization function would take the one hot encoded variables and
#standardize them which we do not want

# @Henning, you are free to change the standardization here. I used N(0,1) as that's what most of the literature I read was pointing to

stdInputData <- standardize_mean0_var1(inputData)
stdInputData <- one_hot_encoding(stdInputData)


## Applyting the standardized data to the simulated parameters to produce Ys:
inputVariables = colnames(stdInputData)[-1];
targetVariables = c("STK", "AMI", "ARF")
model.parameters <- simulate_model_parameters(inputVariables,
																							targetVariables )

alpha <- model.parameters[["alpha"]]
beta <- model.parameters[["beta"]]

targetVariables.n <- length(targetVariables) 

logit.theta <- matrix(NA, nrow = n, ncol = targetVariables.n)
theta <- matrix(NA, nrow = n, ncol = targetVariables.n)
y <- matrix(NA, nrow = n, ncol = targetVariables.n)

for (j in 1:n) { # patient
	for (k in 1:targetVariables.n) { # outcome
		logit.theta[j,k]<- alpha[k] + beta[,k] %*% as.numeric(stdInputData[j,-1])
		theta[j,k] <- 1/(1+exp(-logit.theta[j,k]))
		y[j,k] <- rbern(1, theta[j,k])
	}
}

colnames(y) <- targetVariables

# manual check of data
apply(y, 2, sum)

finalDataset <- cbind(inputData, y)
