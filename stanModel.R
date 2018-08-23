library(rstan)
rstan_options(auto_write = TRUE)

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
