source("inputSimulation.R")

patients <- simulation_of_input_data(2000)

patientsFem <- patients[patients$gender == -1,]

ggplot(patientsFem, aes(x = age, y = height)) +
	geom_point(col =leuphanaPalette$colQualitative$red1, size=0.5) +
	theme_leuphana() +
	xlab("Age") +
	ylab("Height (m)")

ggplot(patientsFem, aes(x = age, y = weight)) +
	geom_point(col =leuphanaPalette$colQualitative$red1, size=0.5) +
	theme_leuphana() +
	xlab("Age") +
	ylab("Weight (kg)")

ggplot(patientsFem, aes(x = age, y = bmi)) +
	geom_point(col =leuphanaPalette$colQualitative$red1, size=0.5) +
	theme_leuphana() +
	xlab("Age") +
	ylab("BMI")
