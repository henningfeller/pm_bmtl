#distrPlayScript.R
########################################################################
# with eta = 1 (no prior knowledge)
corr.draws <- list()

for (i in 1:10000) {
	corr.draws[[i]] <- rlkj(3)
}

corr12 <- c()
corr13 <- c()
corr23 <- c()

for (i in 1:10000) {
	corr12 <- c(corr12, corr.draws[[i]][1,2])
	corr13 <- c(corr13, corr.draws[[i]][1,3])
	corr23 <- c(corr23, corr.draws[[i]][2,3])
}

mean(corr12) # 0.0006911523
var(corr12)  # 0.2045436
mean(corr13) # 0.0002847395
var(corr13)  # 0.2401485
mean(corr23) # 0.0007797004
var(corr23)  # 0.2028529

########################################################################
# with eta = 2 (assume more no correlation)
corr.draws <- list()

for (i in 1:10000) {
	corr.draws[[i]] <- rlkj(3, 2)
}

corr12 <- c()
corr13 <- c()
corr23 <- c()

for (i in 1:10000) {
	corr12 <- c(corr12, corr.draws[[i]][1,2])
	corr13 <- c(corr13, corr.draws[[i]][1,3])
	corr23 <- c(corr23, corr.draws[[i]][2,3])
}

mean(corr12) # 0.000862283
var(corr12)  # 0.1557322
mean(corr13) # -0.0005114173
var(corr13)  # 0.1794156
mean(corr23) # -0.004736726
var(corr23)  # 0.1456764

########################################################################
# with eta = 10 (assume strong no correlation)
corr.draws <- list()

for (i in 1:10000) {
	corr.draws[[i]] <- rlkj(3, 10)
}

corr12 <- c()
corr13 <- c()
corr23 <- c()

for (i in 1:10000) {
	corr12 <- c(corr12, corr.draws[[i]][1,2])
	corr13 <- c(corr13, corr.draws[[i]][1,3])
	corr23 <- c(corr23, corr.draws[[i]][2,3])
}

mean(corr12) # -0.0009153905
var(corr12)  # 0.04520702
mean(corr13) # -0.0003541457
var(corr13)  # 0.04833202
mean(corr23) # -0.001255599
var(corr23)  # 0.04357352

########################################################################
########################################################################
########################################################################
# Checking something with the horseshoe predictor

r <- c()
for (i in 1:100) {
	psi <- rhcauchy(1, sigma = 1)
	tau <- rhcauchy(100, sigma = 1)
	r <- c(r, psi*tau)
}

hist(r, xlim = c(0,10), breaks = 500000)
