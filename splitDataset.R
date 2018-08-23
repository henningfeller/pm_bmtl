# splitData.R

patientsComplete <- read.csv2("patientData/patientsComplete.csv")
patientsComplete <- patientsComplete[,2:ncol(patientsComplete)]


N <- nrow(patientsComplete)

sets <- 5
setCount <- 2000


sampledIdx <- c()
for (i in 1:sets) {
  setSample <- patientsComplete[0,]
  
  while (nrow(setSample) < setCount) {
    idx <- round(runif(1,0,N)+0.5) # uniform draw, rounded up
    # check, if entry was sampled already
    # probably performance nightmare, but we only do it once
    if (!(idx %in% sampledIdx)) {
      sampledIdx <- c(sampledIdx, idx)
      
      candidate <- patientsComplete[idx,]
      if (sum(candidate[,c("STK","AMI","ARF")]) < 3) {
      	# only if not all adverse health events have not occured
      	setSample <- rbind(setSample, candidate)
      }
    }
  }
  
  fileName <- paste("patientData/patients", setCount, "_", i, ".csv", sep="")
  write.csv2(setSample, file = fileName, row.names = FALSE)
}



# Save not-sampled patients to another dataset
patientsRest <- patientsComplete[0,]
for (i in 1:N) {
  if (!(i %in% sampledIdx)) {
  	
  	candidate <- patientsComplete[i,]
  	if (sum(candidate[,c("STK","AMI","ARF")]) < 3) {
  		# only if not all adverse health events have not occured
  		patientsRest <- rbind(patientsRest, candidate)
  	}
  }
}
write.csv2(patientsRest, "patientData/patients2000_Rest.csv")


# Neglected patients due to 3 adverse health events
N - sets*setCount - nrow(patientsRest)
