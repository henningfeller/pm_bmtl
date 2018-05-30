# Found some very meaningful statistics from this study on a South west Germany population 
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3526520/
# The challenge is that the factors are gender correlated but not age correlated which may not be realistic so we simulate this for now
# as I continue to seek information by age which we can then use to apply rejection sampling

behavioural.risk.factors <- function(age,gender){
  if (!((gender == "m") | (gender == "w")| (gender == 1)| (gender == -1))) {
    stop("gender must be 'm','w' or '1','-1'")
    }
    
    marr_values = c("Single/ widowed/ divorced","Married")
    
    ifelse (age < 19, 
            maritalStatus <- 'Never Married',
            if ((gender == "m") | (gender == 1)) {
              maritalStatus <- sample(marr_values, 1, replace=T, prob = c(0.181, 0.819))
              
            }
            else {
              maritalStatus <- sample(marr_values, 1, replace=T, prob = c(0.402, 0.598))
            }
    )
    

    alc_values = c('Abstainer',"Consumer")
    
    ifelse (age < 19, 
            alcohalConsumption <- 'Abstainer',
            if ((gender == "m") | (gender == 1)) {
              alcohalConsumption <- sample(alc_values, 1, replace=T, prob = c(0.223, 0.777))
              
            }
            else {
              alcohalConsumption <- sample(alc_values, 1, replace=T, prob = c(0.542, 0.458))
            }
    )
  
    return(c(maritalStatus, alcohalConsumption)) 
}

clinical.risk.factors <- function(gender) {
  
  if (!((gender == "m") | (gender == "w")| (gender == 1)| (gender == -1))) {
    stop("gender must be 'm','w' or '1','-1'")
  }
  
  Glvalues = c("Good","Moderate","Poor")
  tgValues = c("less than 5 years"," six to ten years","Eleven to Fifteen years","Greater than sixteen years")
  GPVvalues = c("one or less","two to three", "four or greater")
  MA_Values = c ("Good", "Moderate", "Poor")
  HSValues = c("very good","Good","Fair","Poor")
  DMPvalue = c('Yes','No')
  
  
          if ((gender == "m") | (gender == 1)) {
             
            GlycemicLevel <- sample(Glvalues, 1, replace=T, prob = c(0.386, 0.383, 0.231))
            timeSinceDiabetesDiagnosis <- sample(tgValues, 1, replace=T, prob = c(0.386,0.297,0.167,0.151))
            GPVisitsIn3months <- sample(GPVvalues, 1, replace=T, prob = c(0.417,0.393,0.191))
            MedicationAdherence <- sample(MA_Values, 1, replace=T, prob = c(0.756,0.219,0.025))
            selfRatedHealthStatus <- sample(HSValues, 1, replace=T, prob = c(0.1,0.587,0.279,0.034))
            Participation_in_DMP_DM <- sample(DMPvalue, 1, replace=T, prob = c(0.794, 0.206))
            
          }
  
          else {
            
            GlycemicLevel <- sample(Glvalues, 1, replace=T, prob = c(0.411,	0.409,0.18))
            timeSinceDiabetesDiagnosis <- sample(tgValues, 1, replace=T, prob = c(0.408,0.28,0.155,0.157))
            GPVisitsIn3months <- sample(GPVvalues, 1, replace=T, prob = c(0.379,0.415,0.206))
            MedicationAdherence <- sample(MA_Values, 1, replace=T, prob = c(0.77,0.204,0.026))
            selfRatedHealthStatus <- sample(HSValues, 1, replace=T, prob = c(0.077,0.558,0.329,0.036))
            Participation_in_DMP_DM <- sample(DMPvalue, 1, replace=T, prob = c(0.817, 0.183))
          }
  
  return(c(GlycemicLevel, timeSinceDiabetesDiagnosis, GPVisitsIn3months,MedicationAdherence, selfRatedHealthStatus,Participation_in_DMP_DM))
}

lab.reults.risk.factors <- function(gender){
  
  if (!((gender == "m") | (gender == "w")| (gender == 1)| (gender == -1))) {
    stop("gender must be 'm','w' or '1','-1'")
  }
  
  
  if ((gender == "m") | (gender == 1)) {
    Mean_HbA1c <- rnorm(1,6.9,1.1)
    Mean_systolic_BP_in_mmHg <- rnorm(1,137.8, 18.1)
    Mean_diastolic_BP_in_mmHg <- rnorm(79.4 ,10.2)
    
  }
  else {
    Mean_HbA1c <- rnorm(1,6.8,1.0)
    Mean_systolic_BP_in_mmHg <-rnorm(1,136.8,19.0)
    Mean_diastolic_BP_in_mmHg <- rnorm(1,79.6, 10.5)
    
  }

  return(c(Mean_HbA1c,Mean_systolic_BP_in_mmHg, Mean_diastolic_BP_in_mmHg ))
}

related.diagnosis.factors <- function(gender){
  if (!((gender == "m") | (gender == "w")| (gender == 1)| (gender == -1))) {
    stop("gender must be 'm','w' or '1','-1'")
  }
  value = c('Yes','No')
  if ((gender == "m") | (gender == 1)) {
    Cancer <- sample(value, 1, replace=T, prob = c(0.101,	0.899))
    Cor_heart_dis <- sample(value, 1, replace=T, prob = c(0.236,	0.764))
    Depression <- sample(value, 1, replace=T, prob = c(0.107,	0.893))
    Heart_fail <- sample(value, 1, replace=T, prob = c(0.12,	0.88))
    Hypercholesterolemia <- sample(value, 1, replace=T, prob = c(0.562,	0.438))
    Hypertension <- sample(value, 1, replace=T, prob = c(0.783,	0.217))
    Intermittent_claudication <- sample(value, 1, replace=T, prob = c(0.154,	0.846))
    Nephropathy <- sample(value, 1, replace=T, prob = c(0.122,	0.878))
    Neuropathy <- sample(value, 1, replace=T, prob = c(0.231,	0.769))
    Retinopathy <- sample(value, 1, replace=T, prob = c(0.082,	0.918))
    Stroke <- sample(value, 1, replace=T, prob = c(0.075,0.925))
  }
  else {
    Cancer <- sample(value, 1, replace=T, prob = c(0.094,	0.906))
    Cor_heart_dis <- sample(value, 1, replace=T, prob = c(0.128,	0.872))
    Depression <- sample(value, 1, replace=T, prob = c(0.186,	0.814))
    Heart_fail <- sample(value, 1, replace=T, prob = c(0.123,	0.877))
    Hypercholesterolemia <- sample(value, 1, replace=T, prob = c(0.578,	0.422))
    Hypertension <- sample(value, 1, replace=T, prob = c(0.781,	0.219))
    Intermittent_claudication <- sample(value, 1, replace=T, prob = c(0.069,	0.931))
    Nephropathy <- sample(value, 1, replace=T, prob = c(0.086, 0.914))
    Neuropathy <- sample(value, 1, replace=T, prob = c(0.2,	0.8))
    Retinopathy <- sample(value, 1, replace=T, prob = c(0.067,	0.933))
    Stroke <- sample(value, 1, replace=T, prob = c(0.042,	0.958))
  }
  
  return(c(Cancer,Cor_heart_dis,Depression,Heart_fail,Hypercholesterolemia,Hypertension,
         Intermittent_claudication,Nephropathy,Neuropathy,Retinopathy,Stroke))
}
