library(MASS)
outputs <- list() 
for (g in 1:1000) {
	outputs[[g]] <- simulate_model_parameters(inputVariables,
																						targetVariables)
}

allAlphas <- matrix(NA, nrow = 1000, ncol = length(targetVariables))
for (g in 1:1000) {
	allAlphas[g,] <- outputs[[g]][["alpha"]]
}


allAlphas <- round(allAlphas/3)*3
freqAlpha1 <- as.data.frame(table(allAlphas[,1]))
freqAlpha2 <- as.data.frame(table(allAlphas[,2]))
freqAlpha3 <- as.data.frame(table(allAlphas[,3]))

freqAlpha1 <- cbind(targetVariables[1], freqAlpha1)
freqAlpha2 <- cbind(targetVariables[2], freqAlpha2)
freqAlpha3 <- cbind(targetVariables[3], freqAlpha3)

colnames(freqAlpha1) <- c("TargetVariable", "Alpha", "Freq")
colnames(freqAlpha2) <- c("TargetVariable", "Alpha", "Freq")
colnames(freqAlpha3) <- c("TargetVariable", "Alpha", "Freq")

freqAlpha <- rbind(freqAlpha1, freqAlpha2, freqAlpha3)
freqAlpha$Alpha <- strtoi(freqAlpha$Alpha)

ggplot(freqAlpha, aes(x = Alpha, y = Freq, color = TargetVariable, lty = TargetVariable)) +
			geom_line() +
			theme_leuphana() +
			scale_color_leuphana() +
			xlim(-100,100)

hist(allAlphas[,3], xlim = c(-100,100), breaks=10000)

outputs[[1]][["beta"]]
outputs[[1]][["omega"]]
