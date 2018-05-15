# install.packages("stringr")
library(stringr)

source("inputSim_functionPool.R")

# input
n <- 2000

# inputVariables = c("Age", "Gender", "Weight", "Height", "BFP", "Pulse", "BP")

# generating inputData data frame and patient IDs (to give the data frame initial n rows)
inputData <- data.frame(patientID = paste("P", str_pad(round(runif(n,0,n*5)), str_length(n*5)+1, pad="0"), sep =""))

# get some demographic and diabetes prevalence data
pop <- build_population_table()

cnt.m.orig <- sum(pop[pop$indicator == "diabetesTotal" & pop$mw == "m",3:102])
cnt.w.orig <- sum(pop[pop$indicator == "diabetesTotal" & pop$mw == "w",3:102]) # w like German "weiblich" --> female

prob.m <- cnt.m.orig / (cnt.m.orig + cnt.w.orig)

inputData$gender <- r_gender(n, prob.m, values = c("m","w"))
cnt.m.sim <- sum(inputData$gender == "m")
cnt.w.sim <- sum(inputData$gender == "w")

age.distr.m <- as.numeric(pop[pop$indicator == "diabetesTotal" & pop$mw == "m",3:102])
age.distr.w <- as.numeric(pop[pop$indicator == "diabetesTotal" & pop$mw == "w",3:102])
names(age.distr.m) <- 0:99
names(age.distr.w) <- 0:99

inputData$age[inputData$gender == "m"] <- r_ages(cnt.m.sim, age.distr.m,
																								beta.shape1 = 3, beta.shape2 = 1.7) # careful with these,
																																										# they just improve runtime

inputData$age[inputData$gender == "w"] <- r_ages(cnt.w.sim, age.distr.w) # I knew the beta stuff for males
																																				 # as I tested and played around with them
																																				 # for females I stick to standard uniform distr

hist(inputData$age[inputData$gender == "m"], breaks = 101,
		 main = "drawn ages (hist) and underlying distribution",
		 xlab = "Age" )
points(0:99, (age.distr.m * cnt.m.sim / sum(age.distr.m)),
			 type = "l", col = "blue", lwd=2)
