
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

if (!exists("finalDataset")) {
	source("jointSimulation.R")
}

y <- as.matrix(finalDataset[,c("STK", "AMI", "ARF")])
x <- as.matrix(finalDataset[,2:(ncol(finalDataset)-3)])

x <- finalDataset[,2:(ncol(finalDataset)-3)]
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

fileNameRoot="stanOut/"

source("mcmc/DBDA2E-utilities.R")
library(rstan)

# Specify model:
modelString = "
data {
	int<lower=0> N ; // number of patients
	int<lower=0> K ; // number of outcomes
	int<lower=0> J ; // number of predictors
	int y[N,K] ; 
	matrix[N,J] x ;
}

parameters {
	vector[K] beta[J] ;
	vector[K] alpha ;

	// http://stla.github.io/stlapblog/posts/StanLKJprior.html
	corr_matrix[K] Omega[J] ;
	vector<lower=0>[K] sigma[J] ;
	
	vector<lower=0>[J] tau ;
	real<lower=0> psi ;
}

transformed parameters {
	vector[K] zeromean ;
	for (k in 1:K) {
		zeromean[k] = 0 ;
	}
}

model {
	vector[J] r; 							// shrinkage scalar
	matrix[K,K] Sigma[J] ; 	    // covariance matrix
	real utility; 						// utility of ilogit function
	
	// sampling the shrinkage scalar
	tau ~ cauchy(0, 1) ;
	psi ~ cauchy(0, 1) ;
	r = tau * psi ;

	// For each predictor
	for (j in 1:J) {
		// Sampling correlation and standard deviation
		// and calculating covariante
		Omega[j] ~ lkj_corr(1) ; // K is 3 by definition of the matrix
		
		sigma[j] ~ cauchy(0, 2.5) ; // half cauchy due to constraint

		Sigma[j] = quad_form_diag(Omega[j], sigma[j]) ;
		
		beta[j] ~ multi_normal( zeromean, r[j]^2*Sigma[j] ) ;
	}

	for (k in 1:K) {
		alpha[k] ~ cauchy(0, 10) ;
	}
	
	// STAN loves vectorizations
	// So sadistically we are not using it yet
	// Poor Stan :( 
	for (i in 1:N) {
		for (k in 1:K) {
			utility = alpha[k] ;
			for (j in 1:J) {
				utility = utility + x[i,j] * beta[j][k] ;
			}
			
			y[i,k] ~ bernoulli_logit( utility ) ;
		}
	}
}
" # close quote for modelString

# Translate model to C++ and compile to DSO:
stanDso <- stan_model( model_code=modelString ) 

# Generate posterior sample:
stanFit <- sampling( object=stanDso , 
										 data = stanInputData,
										 chains = 3 ,
										 iter = 1000 , 
										 warmup = 200 , 
										 thin = 1 )

openGraph()
traceplot(stanFit,pars=c("theta"))
saveGraph(file=paste0(fileNameRoot,"StanTrace"),type="eps")
openGraph()
plot(stanFit,pars=c("theta"))
saveGraph(file=paste0(fileNameRoot,"StanPlot"),type="eps")

# Make graphs:
# For consistency with JAGS-oriented functions in DBDA2E collection, 
# convert stan format to coda format. This excludes warmup and thinned steps.
mcmcCoda = mcmc.list( lapply( 1:ncol(stanFit) , 
															function(x) { mcmc(as.array(stanFit)[,x,]) } ) )
diagMCMC( mcmcCoda , parName=c("theta") )
saveGraph(file=paste0(fileNameRoot,"Diag"),type="eps")