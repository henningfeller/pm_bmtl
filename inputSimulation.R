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
	# as I tested and played around with them
	
	
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
	inputData$smoking <- NA
	inputData$maritalStatus <- NA
	inputData$alcohalConsumption <- NA
	inputData$GlycemicLevel <- NA
	inputData$timeSinceDiabetesDiagnosis <- NA
	inputData$GPVisitsIn3months <- NA
	inputData$MedicationAdherence <- NA
	inputData$selfRatedHealthStatus <- NA
	inputData$Mean_HbA1c <- NA
	inputData$Mean_systolic_BP_in_mmHg <- NA
	inputData$Mean_diastolic_BP_in_mmHg <- NA
	inputData$Participation_in_DMP_DM <- NA
	inputData$Cancer <- NA
	inputData$Cor_heart_dis <- NA
	inputData$Depression <- NA
	inputData$Heart_fail <- NA
	inputData$Hypercholesterolemia <- NA
	inputData$Hypertension <- NA
	inputData$Intermittent_claudication <- NA
	inputData$Nephropathy <- NA
	inputData$Neuropathy <- NA
	inputData$Retinopathy <- NA
	inputData$Stroke <- NA
	

	
	for (i in 1:nrow(inputData)) {
		inputData[i,c("height","weight","bmi")] <- r_body(inputData$gender[i], inputData$age[i])
		inputData[i, "smoking"] <- r_smoking(inputData$gender[i], inputData$age[i])
		inputData[i,c("maritalStatus","alcohalConsumption")] <- behavioural.risk.factors(inputData$age[i],inputData$gender[i])
		inputData[i,c("GlycemicLevel", "timeSinceDiabetesDiagnosis", "GPVisitsIn3months","MedicationAdherence", "selfRatedHealthStatus",
									"Participation_in_DMP_DM")] <- clinical.risk.factors(inputData$gender[i])
		inputData[i,c("Mean_HbA1c","Mean_systolic_BP_in_mmHg", "Mean_diastolic_BP_in_mmHg")] <- lab.reults.risk.factors(inputData$gender[i])
		inputData[i,c(	"Cancer","Cor_heart_dis","Depression","Heart_fail","Hypercholesterolemia","Hypertension","Intermittent_claudication",
									 "Nephropathy","Neuropathy","Retinopathy", "Stroke")] <- related.diagnosis.factors(inputData$gender[i])
	}
	
	return(inputData)
}
