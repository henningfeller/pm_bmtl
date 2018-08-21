	# general demographic data for Germany from
	# https://www.destatis.de/DE/ZahlenFakten/GesellschaftStaat/Bevoelkerung/Bevoelkerung.html 
	bev <- read.csv("./_data/bev.csv", sep = ";")
	bev <- bev[bev$Simulationsjahr == "2018",]
	bev <- bev[bev$Variante == "1",]
	
	bev <- subset(bev, select = c(-Variante, -Simulationsjahr, -Median, -Bev))
	
	bev <- cbind(rep("population", times = 2), bev)
	colnames(bev) <- c("indicator", "mw", 0:99)
	bev

	# plot(0:99, bev[1,3:102], type = "l", col = "blue")
	# points(0:99, bev[2,3:102], type = "l", col = "red")
	
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
	
# # plot(0:99, bev[5,3:102], type = "l", lty = 2, col = "blue", ylim=c(0,200), xlab = "age", ylab = "Diabetes Patients (*1000)")
# points(0:99, bev[5,3:102], type = "l", lty = 2, col = "blue", ylim=c(0,200), xlab = "age", ylab = "Diabetes Patients (*1000)")
# points(0:99, bev[6,3:102], type = "l", lty = 2, col = "red")

bevNew <- t(bev)
bevNew <- bevNew[3:nrow(bevNew),]
bevNew <- cbind(0:99, bevNew)
colnames(bevNew) <- c("age","pop.m", "pop.w", "dShare.m", "dShare.w", "dTotal.m", "dTotal.w")

bevNew <- as.data.frame(bevNew)
bevNew$age <- unfactor(bevNew$age)
bevNew$pop.m <- unfactor(bevNew$pop.m)
bevNew$pop.w <- unfactor(bevNew$pop.w)
bevNew$dShare.m <- unfactor(bevNew$dShare.m)
bevNew$dShare.w <- unfactor(bevNew$dShare.w)
bevNew$dTotal.m <- unfactor(bevNew$dTotal.m)
bevNew$dTotal.w <- unfactor(bevNew$dTotal.w)

ggplot(bevNew, aes(x = age)) + 
	geom_line(aes(y=pop.m), col = leuphanaPalette$colQualitative$blue1, size = 1) + 
	geom_line(aes(y=pop.w), col = leuphanaPalette$colQualitative$red1, size = 1) +
	geom_line(aes(y=dTotal.m), col = leuphanaPalette$colQualitative$blue1, lty = 2, size = 1) +
	geom_line(aes(y=dTotal.w), col = leuphanaPalette$colQualitative$red1, lty = 2, size = 1) +
	xlab("Age")+
	ylab("Population (*1000)")+
	theme_leuphana()



# sampling using function 
age.distr <- as.numeric(bev[5,3:102])
names(age.distr) <- 0:99

age.rand <- r_ages(2000, age.distr = age.distr, beta.shape1 = 3, beta.shape2 = 1.7)


#scale original count data
age.raw <- bev[5,3:102]
age.raw.scaled <- age.raw * n / sum(age.raw)


ageOrig <- data.frame(age = 0:99)
ageOrig$cnt <- as.numeric(age.raw.scaled)
	
ageData <- as.data.frame(age.rand)
ageData$age.rand <- unfactor(ageData$age.rand)

colnames(ageData) <- c("randomAge")

ggplot(data = ageData, aes(randomAge)) +
	geom_histogram(fill = leuphanaPalette$fillQualitative$red1,
								 col = leuphanaPalette$theme$grey, binwidth = 1) +
	theme_leuphana() +
	xlab("Age") +
	ylab("Diabetes Patients (*1000)") +
	geom_line(data = ageOrig, aes(x = age, y = cnt), col = leuphanaPalette$colQualitative$blue2,size = 1.5, lty=2)


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


########################################################################
# Smoking
# https://www.rauchfrei-info.de/informieren/verbreitung-des-rauchens/raucherquote-bei-erwachsenen/ 
x <- 0:100
y.m <- c(rep(0, times = 12),
				 rep(NA, times = 6),
				 rep(0.261, times = 3),
				 rep(0.362, times = 4),
				 rep(0.319, times = 5),
				 rep(0.35, times = 10),
				 rep(0.321, times = 10),
				 rep(0.303, times = 10),
				 rep(0.22, times = 5),
				 rep(NA, times = 18),
				 rep(0.1, times = 18))

plot(x, y.m)
			 
y.m.model <- lm(y.m ~ poly(x,2), na.action = na.omit)

for (i in x) {
	y.m.sim[i] <- y.m.model$coefficients[1] +
						 y.m.model$coefficients[2]*i^1 +
						 y.m.model$coefficients[3]*i^2 
						 # y.m.model$coefficients[4]*i^3 +
						 # y.m.model$coefficients[5]*i^4
}
points(x, y.m.sim, type = "l", col = "red")			 
			 