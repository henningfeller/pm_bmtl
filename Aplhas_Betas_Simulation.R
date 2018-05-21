rm(list = ls())

library(LaplacesDemon) # for Cauchy distribution sampling
library(stats) # for the cauchy distribution sampling
library(MASS) # for MVN sampling

## Defining the length of the input parameters and the number of tasks to be learnt
no_of_input_parameters <- 10
no_tasks <- 3 

## Simulating Psi from Half Cauchy (0,1) distribution
generated_Parameter_Psi <- rhalfcauchy(1000,1)

plot(generated_Parameter_Psi)
boxplot(generated_Parameter_Psi, ylim=c(0,5))

Parameter_Psi <- mean(generated_Parameter_Psi)

## Simulating Tao (a vector of legth j where j is the number of input parameters) from Half Cauchy (0,1)
Parameter_tao <- rhalfcauchy(no_of_input_parameters,1)

plot(Parameter_tao)

## Computing Gamma from Psi and Tao ( Also a vector of length j)
Parameter_scaling_factor <- (Parameter_Psi*Parameter_tao) ^ 2


## Simulating the sigma from half cauchy (0,2.5) distribution. the sigma will be a 'number of tasks to be learnt by number of input parameter matrix)
parameter_sigma <- list()

# The paper states that the standard deviation for the jth coefficient in the kth task, denoted as σj(k) is
# set to have a half-Cauchy prior distribution with center 0 and scale 2.5, as suggested by Gelman et al. (2008).
for (j in 1:no_of_input_parameters) {
  mylist <- list()
  for(i in 1:no_tasks){
    mylist[[i]] <- rhalfcauchy(1,2.5)
  }
  parameter_sigma[[j]] <- mylist
}


## Simulating Parameter Omega from the LKJ distribution:
source("LKJ_Distribution.R")

parameter_omega <- list()

for (j in 1:no_of_input_parameters) {
  
  parameter_omega[[j]] <- lkj_simulator( d = 3,eta=1 )
  
}

## Computing the Covariance Matrix; we compute for each input parameter j

Cov_matrix <- list()
for (j in 1:no_of_input_parameters) {
  Cov_matrix[[j]] <- diag(parameter_sigma[[j]])%*%parameter_omega[[j]]%*%diag(parameter_sigma[[j]])
}

## Computing the Covariance Matrix of betas' MNV distribution
MVN_COV <- list()

for (j in 1: no_of_input_parameters) {
  MVN_COV[[j]] <- Cov_matrix[[j]]*Parameter_scaling_factor[[j]]
}






## Simulating beta from a multivariate Normal Distribution
mu <- as.vector(matrix(0,nrow= no_tasks))
beta <- list()

for (j in 1:no_of_input_parameters) {
  beta[[j]] <- mvrnorm(n = 1, mu, MVN_COV[[j]], tol = 1e-6, empirical = FALSE, EISPACK = FALSE)
}
beta <- do.call(cbind, beta)


## Simulating alphas 
alpha <- list()
for (i in 1:no_tasks) {
  alpha[[i]] <- rcauchy(1, 0, 10)
  
}
