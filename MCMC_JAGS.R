rm(list = ls())
library(rjags)
library(runjags)

# Load Input Data
Input_Data = read.csv("input_data.csv")

# Drop index and patient ID columns  
Input_Data = subset(Input_Data, select = -c(X,patientID))

# Standardize continuous inputs and onehotencode binary inputs
source('inputSim_functionPool.R')
Input_Data = standardize_mean0_var1(Input_Data)
Input_Data = one_hot_encoding(Input_Data)
Input_Data = data.matrix(Input_Data, rownames.force = NA)

#Load Output Data
Output_Data = read.csv("output_data.csv")
# Drop index and patient ID columns  
Output_Data = subset(Output_Data, select=-c(X,id))

# Get the dimensions of required in the model
Ntotal = dim(Input_Data)[1]
Ndim = dim(Input_Data)[2]
NOutcome = dim(Output_Data)[2]

# Get the SIGMA priors since there's no LKJ function in JAGS
#m = matrix(c(2,-1,0,-1,2,-1,0,-1,2),3,3)
omega = function(NOutcome){
	source("LKJdistribution.R")
	omega <- rlkj(NOutcome, 1)
}

# Function for getting diagonal inside the JAGS model
ls = rep (1, NOutcome)
ls = as.double(ls)
t.diag = function(ls = c(1,1,1)){
	return(diag(ls))
}

# Data List for the JAGS model
outcome = NULL
mu = c(0,0,0)
X = NULL
y = unlist(Output_Data[colnames(Output_Data)], use.names = FALSE)

for (i in 1:NOutcome){
	outcome = c(outcome, rep(i,Ntotal))
	X = rbind(X,Input_Data)
}

outcome = as.double(outcome)
mu = as.double(mu)
y = as.double(y)

d_List = list (y=y, Ntotal=Ntotal, Ndim=Ndim, NOutcome=NOutcome, outcome=outcome, X=X, mu=mu, t.diag=t.diag(ls), omega=omega(NOutcome))

modelString = "
	model {
		# Defining the likelihood
		for (k in 1:(NOutcome*Ntotal)) {
			y[k] ~ dbern(theta[k])
		}

		# Defining the priors for the theta
		for (i in 1: (NOutcome*Ntotal)) {
			theta[i] = 1/(1+exp(-(alpha[outcome[i]] + (beta[,outcome[i]] %*% X[i,]))))
		}
		
		# Defining the priors for the alphas
		for (j in 1:NOutcome){
			alpha[j] ~ dt(0, 10, 1) #Cauchy dist is the same as a T distr with k=1 degrees of freedom 
															#https://en.wikipedia.org/wiki/Cauchy_distribution#Related_distributions
		}
		
		# Defining the priors for the betas
		for (i in 1:Ndim){
			for (j in 1:NOutcome) {
				sigma.sd[j] ~  dt(0, 2.5, 1)T(0,)
			}
			sigma = t.diag(sigma.sd) %*% omega %*% t.diag(sigma.sd)
			beta[i,1:NOutcome] ~ dmnorm(mu,sigma)
		}
		
		# Defining the formulae for 'r' and it's priors
		for (i in 1:Ndim){
			r[i] = tau[i] * psi
			r_sqr[i] = r[i]* r[i]
		}
		
		for (j in 1:Ndim){
			tau[j] ~ dt(0, 1, 1)T(0,) # Half cauchy are truncated cauchy dist 
																# https://stackoverflow.com/questions/34935606/cauchy-prior-in-jags/34951349
		}
		
		psi ~ dt(0, 1, 1)T(0,)
	}"

writeLines (modelString, con  = 'TEMPmodel.txt')

jagsModel = jags.model(file = 'TEMPmodel.txt', data = d_List, n.chains = 10, n.adapt = 1000, quiet = FALSE)

update(jagsModel,n.iter = 5)
codaSamples = coda.samples(jagsModel, variable.names = c('theta','alpha','beta'), n.iter = 5)

codaSamples
