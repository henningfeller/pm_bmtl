theme_coord <- function() {
	list(theme_minimal(),
			 geom_vline(data = data.frame(x = c(0)), aes(xintercept = x), size = 0.33, col = "grey50"),
			 geom_hline(data = data.frame(y = c(0)), aes(yintercept = y), size = 0.33, col = "grey50")
	)
}

plot_function <- function(FUN,
													xlim = c(-1,1),
													accuracy = 0.01,
													add.to.plot = NULL,
													ggplot = TRUE,
													col = par("col"),
													lwd = par("lwd"),
													lty = par("lty")) {
	
	# Parameter Checks
	if (!is.null(add.to.plot) & !identical(xlim, c(-1,1))) {
		warning("xlim Parameter will only be used for new plots")
	}
	
	if (!is.null(add.to.plot) & ggplot) {
		# add to existing ggplot
		xlim = layer_scales(add.to.plot)$x$limits
	} else if (!ggplot && add.to.plot) {
		# add to existing normal plot
		xlim <- par("usr")[1:2]
	}
	
	# Generate data in interval
	funData <- data.frame(x = seq(xlim[1], xlim[2], by = accuracy))
	tryCatch( {
			funData$y <- FUN(funData$x)
		}, error = function(err) {
			stop(paste("Could not calculate y values with specified function. Nested error:\n",
								 strsplit(paste(err), ":")[[1]][-1]))
		})
	
	if (ggplot) {
		if (is.null(add.to.plot)) {
			p <- ggplot(data = funData, aes(x = x, y = y)) +
						 theme_coord() +
						 xlim(xlim[1], xlim[2])
		} else {
			p <- add.to.plot
		}
		
		p <- p + geom_line(data = funData, aes(x = x, y = y),
											 col = col, size = (lwd/2), linetype = lty)
	} else {
		if ((!add.to.plot) | is.null(add.to.plot)) {
			p <- plot(funData$x, funData$y, type = "l", xlim = xlim,
								col = col, lwd = lwd, lty = lty)								
		} else {
			p <- add.to.plot + points(funData$x, funData$y, type = "l",
																col = col, lwd = lwd, lty = lty)
		}
	}
	return(p)
}

#&====================================================================&#
#& Loading Required Libraries
#&====================================================================&#
library(ggplot2)
