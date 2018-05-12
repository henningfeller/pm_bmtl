# https://www.sciencedirect.com/science/article/pii/S0047259X09000876
# Section 3.2

rlkj <- function(dim, eta = 1) {
	if (dim < 3) {
		stop("Dimension must be equal or bigger than 3")
	}
	
	# Initialization
	beta <- eta + (dim - 2)/2
	
	u <- rbeta(1, beta, beta)
	
	r12 <- 2*u - 1
	r <- matrix(c(1, r12, r12, 1), nrow=2, ncol=2)
	
	for (k in 2:(dim-1)) {
		beta <- beta - 0.5
		
		y <- rbeta(1, k/2, beta)
		
		u <- runif_spere(1,k)
		
		w <- sqrt(y) * u
		
		A <- chol(r)
		
		z <- A %*% as.vector(w)
		
		r <- cbind(r, z)
		r <- rbind(r, c(z,1))
	}
	
	return(r)
}

# https://stats.stackexchange.com/questions/7977/how-to-generate-uniformly-distributed-points-on-the-surface-of-the-3-d-unit-sphe
runif_spere <- function(n, dim) {
	r <- matrix(NA, nrow = n, ncol = dim)
	
	for (i in 1:n) {
		q = 0
		r[i,] <- rnorm(dim)
		
		qsquare <- sum(r[i,]^2)
		q <- sqrt(qsquare)
		r[i,] <- r[i,] / q
	}
	return(r)
}
