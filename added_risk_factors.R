# Found some very meaningful statistics from this study on a South west Germany population 
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3526520/
# The challenge is that the factors are gender correlated but not age correlated which may not be realistic so we simulate this for now
# as I continue to seek information by age which we can then use to apply rejection sampling

behavioural.risk.factors <- function(age,gender){
  if (!((gender == "m") | (gender == "w")| (gender == 1)| (gender == -1))) {
    stop("gender must be 'm','w' or '1','-1'")
    }
    
    marr.values = c("Single/ widowed/ divorced","Married")
    
    ifelse (age < 19, 
            marital.status <- 'Never Married',
            if ((gender == "m") | (gender == 1)) {
              marital.status <- sample(marr.values, 1, replace=T, prob = c(0.181, 0.819))
              
            }
            else {
              marital.status <- sample(marr.values, 1, replace=T, prob = c(0.402, 0.598))
            }
    )
    

    alc.values = c('Abstainer',"Consumer")
    
    ifelse (age < 19, 
            alcohal.consumption <- 'Abstainer',
            if ((gender == "m") | (gender == 1)) {
              alcohal.consumption <- sample(alc.values, 1, replace=T, prob = c(0.223, 0.777))
              
            }
            else {
              alcohal.consumption <- sample(alc.values, 1, replace=T, prob = c(0.542, 0.458))
            }
    )
    
    return(c(marital.status, alcohal.consumption)) 
}

clinical.risk.factors <- function(gender) {
  
  if (!((gender == "m") | (gender == "w")| (gender == 1)| (gender == -1))) {
    stop("gender must be 'm','w' or '1','-1'")
  }
  
  Gl.values = c('Good (≤6.4%)',"Moderate (6.5 - 7.4%)","Poor (≥7.5%)")
  tg.Values = c("≤5 years","6 – 10 years","11 – 15 years","≥16 years")
  GPV.values = c("≤1","2 – 3", "≥4")
  MA.Values = c ("Good", "Moderate", "Poor")
  HS.Values = c("very good","Good","Fair","Poor")
  
  
  
          if ((gender == "m") | (gender == 1)) {
             
            Glycemic.level <- sample(Gl.values, 1, replace=T, prob = c(0.386, 0.383, 0.231))
            time.Since.Diabetes.Diagnosis <- sample(tg.Values, 1, replace=T, prob = c(0.386,0.297,0.167,0.151))
            GP.Visits.in3months <- sample(GPV.values, 1, replace=T, prob = c(0.417,0.393,0.191))
            Medication.adherence <- sample(MA.Values, 1, replace=T, prob = c(0.756,0.219,0.025))
            self.rated.health.status <- sample(HS.Values, 1, replace=T, prob = c(0.1,0.587,0.279,0.034))
            
          }
  
          else {
            
            Glycemic.level <- sample(Gl.values, 1, replace=T, prob = c(0.411,	0.409,0.18))
            time.Since.Diabetes.Diagnosis <- sample(tg.Values, 1, replace=T, prob = c(0.408,0.28,0.155,0.157))
            GP.Visits.in3months <- sample(GPV.values, 1, replace=T, prob = c(0.379,0.415,0.206))
            Medication.adherence <- sample(MA.Values, 1, replace=T, prob = c(0.77,0.204,0.026))
            self.rated.health.status <- sample(HS.Values, 1, replace=T, prob = c(0.077,0.558,0.329,0.036))
          }
  
  return(c(Glycemic.level, time.Since.Diabetes.Diagnosis, GP.Visits.in3months,Medication.adherence, self.rated.health.status))
}

lab.reults.risk.factors <- function(gender){
  
  if (!((gender == "m") | (gender == "w")| (gender == 1)| (gender == -1))) {
    stop("gender must be 'm','w' or '1','-1'")
  }
  DMP.DM.value = c('Yes','No')
  if ((gender == "m") | (gender == 1)) {
    Mean.HbA1c <- rnorm(1,6.9,1.1)
    Mean.systolic.BP.in.mmHg <- rnorm(1,137.8, 18.1)
    Mean.diastolic.BP.in.mmHg <- rnorm(79.4 ,10.2)
    Participation.in.DMP.DM <- sample(DMP.DM.value, 1, replace=T, prob = c(0.794, 0.206))
  }
  else {
    Mean.HbA1c <- rnorm(1,6.8,1.0)
    Mean.systolic.BP.in.mmHg <-rnorm(1,136.8,19.0)
    Mean.diastolic.BP.in.mmHg <- rnorm(1,79.6, 10.5)
    Participation.in.DMP.DM <- sample(DMP.DM.value, 1, replace=T, prob = c(0.817, 0.183))
  }
  return(c(Mean.HbA1c,Mean.systolic.BP.in.mmHg, Mean.diastolic.BP.in.mmHg,Participation.in.DMP.DM ))
}

related.diagnosis.factors <- function(gender){
  if (!((gender == "m") | (gender == "w")| (gender == 1)| (gender == -1))) {
    stop("gender must be 'm','w' or '1','-1'")
  }
  value = c('Yes','No')
  if ((gender == "m") | (gender == 1)) {
    Cancer <- sample(value, 1, replace=T, prob = c(0.101,	0.899))
    Coronary.heart.disease <- sample(value, 1, replace=T, prob = c(0.236,	0.764))
    Depression <- sample(value, 1, replace=T, prob = c(0.107,	0.893))
    Heart.failure <- sample(value, 1, replace=T, prob = c(0.12,	0.88))
    Hypercholesterolemia <- sample(value, 1, replace=T, prob = c(0.562,	0.438))
    Hypertension <- sample(value, 1, replace=T, prob = c(0.783,	0.217))
    Intermittent.claudication <- sample(value, 1, replace=T, prob = c(0.154,	0.846))
    Nephropathy <- sample(value, 1, replace=T, prob = c(0.122,	0.878))
    Neuropathy <- sample(value, 1, replace=T, prob = c(0.231,	0.769))
    Retinopathy <- sample(value, 1, replace=T, prob = c(0.082,	0.918))
    Stroke <- sample(value, 1, replace=T, prob = c(0.075,0.925))
  }
  else {
    Cancer <- sample(value, 1, replace=T, prob = c(0.094,	0.906))
    Coronary.heart.disease <- sample(value, 1, replace=T, prob = c(0.128,	0.872))
    Depression <- sample(value, 1, replace=T, prob = c(0.186,	0.814))
    Heart.failure <- sample(value, 1, replace=T, prob = c(0.123,	0.877))
    Hypercholesterolemia <- sample(value, 1, replace=T, prob = c(0.578,	0.422))
    Hypertension <- sample(value, 1, replace=T, prob = c(0.781,	0.219))
    Intermittent.claudication <- sample(value, 1, replace=T, prob = c(0.069,	0.931))
    Nephropathy <- sample(value, 1, replace=T, prob = c(0.086, 0.914))
    Neuropathy <- sample(value, 1, replace=T, prob = c(0.2,	0.8))
    Retinopathy <- sample(value, 1, replace=T, prob = c(0.067,	0.933))
    Stroke <- sample(value, 1, replace=T, prob = c(0.042,	0.958))
  }
  
  return(c(Cancer,Coronary.heart.disease,Depression,Heart.failure,Hypercholesterolemia,Hypertension,Intermittent.claudication,
         Intermittent.claudication,Nephropathy,Neuropathy,Retinopathy,Stroke))
}