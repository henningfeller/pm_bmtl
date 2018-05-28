# install.packages("stringr")
library(stringr)

source("inputSim_functionPool.R")

simulation_of_input_data <- function(n) {
	
	# generating inputData data frame and patient IDs (to give the data frame initial n rows)
	inputData <- data.frame(patientID = paste("P", str_pad(round(runif(n,0,n*5)), str_length(n*5)+1, pad="0"), sep =""))
	
	# get some demographic and diabetes prevalence data
	pop <- build_population_table()
	
	cnt.m.orig <- sum(pop[pop$indicator == "diabetesTotal" & pop$mw == "m",3:102])
	cnt.w.orig <- sum(pop[pop$indicator == "diabetesTotal" & pop$mw == "w",3:102]) # w like German "weiblich" --> female
		
	prob.m <- cnt.m.orig / (cnt.m.orig + cnt.w.orig)
	
	inputData$gender <- r_gender(n, prob.m, values = c(1,-1))
	cnt.m.sim <- sum(inputData$gender == 1)
	cnt.w.sim <- sum(inputData$gender == -1)
	
	age.distr.m <- as.numeric(pop[pop$indicator == "diabetesTotal" & pop$mw == "m",3:102])
	age.distr.w <- as.numeric(pop[pop$indicator == "diabetesTotal" & pop$mw == "w",3:102])
	names(age.distr.m) <- 0:99
	names(age.distr.w) <- 0:99
	
	inputData$age[inputData$gender == 1] <- r_ages(cnt.m.sim, age.distr.m,
																									beta.shape1 = 3, beta.shape2 = 1.7) # careful with these,
																																											# they just improve runtime
	
	inputData$age[inputData$gender == -1] <- r_ages(cnt.w.sim, age.distr.w) # I knew the beta stuff for males
																																					 # as I tested and played around with them
																																					 # for females I stick to standard uniform distr
	
	# plot draw vs. original distribution
	# hist(inputData$age[inputData$gender == 1], breaks = 101,
	# 		 main = "drawn ages (hist) and underlying distribution",
	# 		 xlab = "Age" )
	# points(0:99, (age.distr.m * cnt.m.sim / sum(age.distr.m)),
	# 			 type = "l", col = "blue", lwd=2)
	
	
	# Include additional risk factors
	source("added_risk_factors.R")
	
	
	inputData$height <- NA
	inputData$weight <- NA
	inputData$bmi <- NA
	inputData$marital.status <- NA
	inputData$alcohal.consumption <- NA
	inputData$Glycemic.level <- NA
	inputData$time.Since.Diabetes.Diagnosis <- NA
	inputData$GP.Visits.in3months <- NA
	inputData$Medication.adherence <- NA
	inputData$self.rated.health.status <- NA
	inputData$Mean.HbA1c <- NA
	inputData$Mean.systolic.BP.in.mmHg <- NA
	inputData$Mean.diastolic.BP.in.mmHg <- NA
	inputData$Participation.in.DMP.DM <- NA
	inputData$Cancer <- NA
	inputData$Coronary.heart.disease <- NA
	inputData$Depression <- NA
	inputData$Heart.failure <- NA
	inputData$Hypercholesterolemia <- NA
	inputData$Hypertension <- NA
	inputData$Intermittent.claudication <- NA
	inputData$Nephropathy <- NA
	inputData$Neuropathy <- NA
	inputData$Retinopathy <- NA
	inputData$Stroke <- NA
	

	
	for (i in 1:nrow(inputData)) {
		inputData[i,c("height","weight","bmi")] <- r_body(inputData$gender[i], inputData$age[i])
		inputData[i,c("marital.status","alcohal.consumption")] <- behavioural.risk.factors(inputData$age[i],inputData$gender[i])
		inputData[i,c("Glycemic.level", "time.Since.Diabetes.Diagnosis", "GP.Visits.in3months",
		              "Medication.adherence", "self.rated.health.status")] <- clinical.risk.factors(inputData$gender[i])
		inputData[i,c("Mean.HbA1c","Mean.systolic.BP.in.mmHg", "Mean.diastolic.BP.in.mmHg",
		              "Participation.in.DMP.DM")] <- lab.reults.risk.factors(inputData$gender[i])
		inputData[i,c(	"Cancer","Coronary.heart.disease","Depression","Heart.failure","Hypercholesterolemia","Hypertension",
		               "Intermittent.claudication","Nephropathy","Neuropathy","Retinopathy",
		               "Stroke")] <- related.diagnosis.factors(inputData$gender[i])
	}
	
	return(inputData)
}

