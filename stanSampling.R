
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

source("standardizationFunctions.R")

source("stanModel.R") # get compiled Stan Model
library(rstan)

options(mc.cores = parallel::detectCores())

setAmount <- 5
setPrefix <- "patientData/patients2000_"
setSuffix <- ".csv"

for (setIdx in 1:setAmount) {
	
	fileNameIn <- paste(setPrefix, setIdx, setSuffix, sep="")
	patientData <- read.csv2(fileNameIn)
	print("###################################################")
	print(paste("Reading File:", fileNameIn))
	
	y <- as.matrix(patientData[,c("STK", "AMI", "ARF")])
	
	x <- patientData[,c("gender", "age", "height", "weight", "bmi", "smoking", "married", "alcohol")]
	x$gender <- as.character(x$gender)
	x$smoking <- as.character(x$smoking)
	x$married <- as.character(x$married)
	x$alcohol <- as.character(x$alcohol)
	
	
	x <- standardize_mean0_var1(x)
	x <- one_hot_encoding(x)
	x <- as.matrix(x)
	
	N <- nrow(x)
	J <- ncol(x)
	K <- ncol(y)
	
	stanInputData <- list(
		N = N,
		K = K,
		J = J,
		y = y,
		x = x )
	
	# Generate posterior sample:
	stanFit <- sampling( object=stanDso , 
											 data = stanInputData,
											 chains = 3 ,
											 iter = 2000 , 
											 warmup = 1000 , 
											 thin = 1 )
	
	
	fileNameOut <- paste("stanOut/stanFit_", setIdx, ".Rdata", sep="")
	save(stanFit, file = fileNameOut)
}
