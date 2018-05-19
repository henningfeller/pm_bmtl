	# general demographic data for Germany from
	# https://www.destatis.de/DE/ZahlenFakten/GesellschaftStaat/Bevoelkerung/Bevoelkerung.html 
	bev <- read.csv("bev.csv", sep = ";")
	bev <- bev[bev$Simulationsjahr == "2018",]
	bev <- bev[bev$Variante == "1",]
	
	bev <- subset(bev, select = c(-Variante, -Simulationsjahr, -Median, -Bev))
	
	bev <- cbind(rep("population", times = 2), bev)
	colnames(bev) <- c("indicator", "mw", 0:99)
	bev

	plot(0:99, bev[1,3:102], type = "l", col = "blue")
	points(0:99, bev[2,3:102], type = "l", col = "red")
	
	# some information on diabetes prevalance from
	# https://www.rki.de/SharedDocs/Bilder/GBE/Gesundheitsthemen/Infografik_Diabetes_2016.gif?__blob=normal&v=5
	# (these information are tough to get and nail down, so I'm happy I found something...
	# ... after all we should show algorithm knowledge, so the numbers don't really matter)
	levels(bev$indicator) <- c("population", "diabetesShare", "diabetesTotal")
	bev[3,] <-  c("diabetesShare", "m",
											rep(0.01, times=40),
							 		 		rep(0.02, times=10),
							 				rep(0.075, times=10),
							 				rep(0.17, times=10),
							 				rep(0.22, times=30))
	
	bev[4,] <-  c("diabetesShare", "w",
								rep(0.02, times=20),
								rep(0.035, times=20),
								rep(0.045, times=10),
								rep(0.04, times=10),
								rep(0.11, times=10),
								rep(0.22, times=30))
	
	bev[5,1:2] <- c("diabetesTotal", "m")
	bev[6,1:2] <- c("diabetesTotal", "w")
	
	for (i in 3:102) {
		bev[,i] <- as.numeric(bev[,i])
		bev[5,i] <- bev[1,i]*bev[3,i]	
		bev[6,i] <- bev[2,i]*bev[4,i]	
	}
	
plot(0:99, bev[5,3:102], type = "l", lty = 2, col = "blue", ylim=c(0,200), xlab = "age", ylab = "Diabetes Patients (*1000)")
points(0:99, bev[6,3:102], type = "l", lty = 2, col = "red")

beta1 <- 3
beta2 <- 1.7

db <- dbeta(seq(0, 1, by=0.01), beta1, beta2)
c <- max(bev[5:6,3:102]) * 1.1 / max(db)
db <- db*c

points(0:100, db, type = "l")


# sampling using function 
age.distr <- as.numeric(bev[5,3:102])
names(age.distr) <- 0:99

age.rand <- r_ages(2000, age.distr = age.distr, beta.shape1 = 3, beta.shape2 = 1.7)


#scale original count data
age.raw <- bev[5,3:102]
age.raw.scaled <-age.raw * n / sum(age.raw)

hist(age.rand, breaks = 101)
points(0:99, age.raw.scaled, type = "l", col = "blue", lwd=2)

sum(age.raw)
View(age.raw)


# playing with body mass indexes
beta1 <- 6.7
beta2 <- 26
scale <- 50
min <- 16
bmi.rand <- round(rbeta(1000,beta1,beta2)*scale+min,1)
	
hist(bmi.rand, breaks = 25)
sum(bmi.rand < 18.5) / 1000 # should be 4,4%
sum((bmi.rand >= 18.5) & (bmi.rand < 25)) / 1000 # should be 75,1%
sum((bmi.rand >= 25) & (bmi.rand < 30)) / 1000 # should be 16,4%
sum(bmi.rand >= 30) / 1000 # should be 4,1%

# males
# go from (20, 4,29)
# go to (75, 7,25)


# females
# go from (20, 3.5, 30)
# to (75, 6.7, 26)