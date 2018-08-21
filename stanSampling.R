
graphics.off() # This closes all of R's graphics windows.
rm(list=ls())  # Careful! This clears all of R's memory!

fileNameRoot="stanOut/"

source("mcmc/DBDA2E-utilities.R")
library(rstan)

# Specify model:
modelString = "
data {
	int<lower=0> N ; // number of patients
	int<lower=0> K ; // number of outcomes
	int<lower=0> J ; // number of predictors
	matrix[N,K] y ; 
	matrix[N,J] x ;
}

parameters {
	vector[K] beta[J] ;
	vector[K] alpha ;

	// http://stla.github.io/stlapblog/posts/StanLKJprior.html
	corr_matrix[K] Omega[J] ;
	vector<lower=0>[K] sigma[J] ;
	cov_matrix[K] Sigma[J] ;

	vector<lower=0>[J] tau ;
	real<lower=0> psi ;
	vector<lower=0>[J] r ;
}

transformed parameters {
	vector[K] zeromean ;
	for (k in 1:K) {
		zeromean[k] = 0
	}
}

model {
	// Sampling shrinkage scalar
	tau ~ cauchy(0, 1) ;
	psi ~ cauchy(0, 1) ;
	r = tau * psi ;

	// For each predictor
	for (j in 1:J) {
		// Sampling correlation and standard deviation
		// and calculating covariante
		Omega[j] ~ lkj_corr(1) ;
		
		sigma[j] ~ cauchy(0, 2.5) ; // half cauchy due to constraint

		Sigma[j] = quad_form_diag(Omega[j], sigma[j]) ;
		
		beta[j] ~ multi_normal( zeromean, r[j]^2*Sigma[j] ) ;
	}

	for (k in 1:K) {
		alpha[k] ~ cauchy(0, 10) ;
	}
	
	// declaration of utility
	real utility;
	
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

# Specify data:
N = 50 ; z = 10
y = c(rep(1,z),rep(0,N-z))
dataList = list(
	y = y ,
	N = N 
)

# Generate posterior sample:
stanFit <- sampling( object=stanDso , 
										 data = dataList , 
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
