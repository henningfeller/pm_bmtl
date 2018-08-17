# c(leuphanaPalette$red_washed1,
# 	leuphanaPalette$beige1,
# 	leuphanaPalette$blue_washed1,
# 	leuphanaPalette$grey2,
# 	leuphanaPalette$turquise2,
# 	leuphanaPalette$purple_washed,
# 	leuphanaPalette$turquise1,
# 	leuphanaPalette$beige2,
# 	leuphanaPalette$grey3)

# 
# grey1 = "#F2F3F5",
# grey2 = "#E2E3E7",
# grey3 = "#CFD0D4",
# grey4 = "#C4C7C0",
# turquise1 = "#AFD2C8",
# turquise2 = "#87C2B0",
# turquise3 = "#428772",
# beige1 = "#E6DCB9",
# beige2 = "#CCBF84",
# red_washed1 = "#B79C8B",
# red_washed2 = "#965F5A",
# blue_washed1 = "#A3B8CC",
# blue_washed2 = "#6488AA",
# blue = "#245787",
# purple_washed = "#B8A9B8",
# purple_washed2 = "#9F759F",
# purple = "#854185",
# red = "#742129")

#&====================================================================&#
#& Loading Required Libraries
#&====================================================================&#
library(ggplot2)


leuphanaPalette <- list()

leuphanaPalette$theme <- list(red = "#742129",
															red_light = "#B79C8B",
															grey = "#F2F3F5")

leuphanaPalette$fillQualitative <- list(red1 = "#B79C8B",
																		 blue1 = "#A3B8CC",
																		 turquise1 = "#AFD2C8",
																		 purple1 = "#B8A9B8",
																		 grey1 = "#E2E3E7",
																		 red2 = "#965F5A",
																		 blue2 = "#6488AA",
																		 turquise2 = "#87C2B0",
																		 purple2 = "#9F759F",
																		 grey2 = "#CFD0D4")

leuphanaPalette$fillSequential <- list(red1 = "#F2F3F5",
																		red2 = "#E8E3E4",
																		red3 = "#DED2D3",
																		red4 = "#D3C2C1",
																		red5 = "#C9B1B0",
																		red6 = "#BFA19F",
																		red7 = "#B5908E",
																		red8 = "#AA807C",
																		red9 = "#A06F6B",
																		red10 = "#965F5A")

leuphanaPalette$fillDiverging <- list(blue5 = "#6488AA",
																	 blue4 = "#809DB9",
																	 blue3 = "#9DB3C8",
																	 blue2 = "#B9C8D7",
																	 blue1 = "#D6DEE6",
																	 neutral = "#F2F3F5",
																	 red1 = "#E0D5D6",
																	 red2 = "#CDB8B7",
																	 red3 = "#BB9A98",
																	 red4 = "#A87D79",
																	 red5 = "#965F5A")
																	
leuphanaPalette$colQualitative <- list(red1 = "#742129",
																		blue1 = "#6488AA",
																		grey1 = "#C4C7C0",
																		purple1 = "#854185",
																		turquise1 = "#428772",
																		red2 = "#965F5A",
																		blue2 = "#245787",
																		turquise2 = "#87C2B0",
																		purple2 = "#9F759F",
																		grey2 = "#E2E3E7")

leuphanaPalette$colSequential <- list(red1 = "#E2E3E7",
																	 red2 = "#D6CDD2",
																	 red3 = "#CAB8BD",
																	 red4 = "#BDA2A8",
																	 red5 = "#B18D93",
																	 red6 = "#A5777D",
																	 red7 = "#996268",
																	 red8 = "#8C4C53",
																	 red9 = "#80373E",
																	 red10 = "#742129" )

leuphanaPalette$colDiverging <- list(blue5 = "#245787",
																	blue4 = "#4A739A",
																	blue3 = "#708FAD",
																	blue2 = "#96ABC1",
																	blue1 = "#BCC7D4",
																	neutral = "#E2E3E7",
																	red1 = "#CCBCC1",
																	red2 = "#B6959B",
																	red3 = "#A06F75",
																	red4 = "#8A484F",
																	red5 = "742129" )

#&====================================================================&#
#& scale_color_leuphana
#& scale_fill_leuphana
#&--------------------------------------------------------------------&#
#& Discrete Color Scales
#&====================================================================&#
scale_color_leuphana <- function(type = "qualitative", ...) {
	type <- tolower(type)
	if ((type != "sequential") & (type != "qualitative") & (type != "diverging")) {
		warning(paste("Value \"", type, "\" is not valid for Parameter type. \"qualitative\" is used."))
		type <- "qualitative"
	}

	discrete_scale("colour", "leuphana", leuphana_pal(type = type, fill = FALSE),
								 ...)
}

scale_fill_leuphana <- function(type = "qualitative", ...) {
	type <- tolower(type)
	if ((type != "sequential") & (type != "qualitative") & (type != "diverging")) {
		warning(paste("Value \"", type, "\" is not valid for Parameter type. \"qualitative\" is used."))
		type <- "qualitative"
	}
	
	discrete_scale("fill", "leuphana", leuphana_pal(type = type, fill = TRUE, ...),
								 ...)
}



#&====================================================================&#
#& Function leuphana_pal
#&--------------------------------------------------------------------&#
#& The most important function. Returning 
#&--------------------------------------------------------------------&#
#& Parameters
#&   --> type
#&				value range:
#&					"qualitative" - (default) each group gets an color, which 
#&													has no relation to other colors
#&					"sequential" -	colors are in a sequential order from
#&													light grey to key dark red
#&					"diverging"  -	colors are in sequential order from
#&													dark blue over light grey to dark key red
#&   <-- function with param. n (required for discrete_scale function)
#&====================================================================&#
leuphana_pal <- function(type = "qualitative", fill = FALSE) {
	#&--------------------------------------------------------------------&#
	#& Type check
	#&--------------------------------------------------------------------&#
	type <- tolower(type)
	if ((type != "sequential") & (type != "qualitative") & (type != "diverging")) {
		warning(paste("Value \"", type, "\" is not valid for Parameter type. \"qualitative\" is used."))
		type <- "qualitative"
	}
	
	
	#&--------------------------------------------------------------------&#
	#& Qualitative Scale
	#&--------------------------------------------------------------------&#
	if (type == "qualitative") {
		if (fill) {
			function(n) {
				col = c()
				for(i in 1:10) {
					col = c(col, leuphanaPalette$fillQualitative[[i]])
				}
				unname(col)
			}
		} else {
			function(n) {
				col = c()
				for(i in 1:10) {
					col = c(col, leuphanaPalette$colQualitative[[i]])
				}
				unname(col)
			}
		}
		
		
		#&--------------------------------------------------------------------&#
		#& Sequential Scale
		#&--------------------------------------------------------------------&#
	} else if (type == "sequential") {
		if (fill) {
			ColSequence <- leuphanaPalette$fillSequential
			standardCol <- leuphanaPalette$fillQualitative$red1
		} else { # point colors
			ColSequence <- leuphanaPalette$colSequential
			standardCol <- leuphanaPalette$colQualitative$red1
		}
		
		function(n) {
			if (n == 1) {
				col <- standardCol 
			} else { 
				if (n == 2) {
					colIndex <- c(4,6)
				} else if (n == 3) {
					colIndex <- c(3,5,7)
				} else if (n == 4) {
					colIndex <- c(2,4,6,8)
				} else if (n == 5) {
					colIndex <- c(1,3,5,7,9)
				} else if (n == 6) {
					colIndex <- 2:7
				} else if (n == 7) {
					colIndex <- 2:8
				} else if (n == 8) {
					colIndex <- 2:9
				} else if (n == 9) {
					colIndex <- 1:9
				} else if (n == 10) {
					colIndex <- 1:10
				}
				
				col <- c()
				for(i in colIndex) {
					col <- c(col, ColSequence[[i]])
				}
			}
			
			unname(col)
		}
		
		
		#&--------------------------------------------------------------------&#	
		#& Diverging Scale 
		#&--------------------------------------------------------------------&#
	} else if (type == "diverging") {
		if (fill) {
			ColSequence <- leuphanaPalette$fillDiverging
			standardCol <- leuphanaPalette$fillQualitative$red1
		} else { # point colors
			ColSequence <- leuphanaPalette$colDiverging
			standardCol <- leuphanaPalette$colQualitative$red1
		}
		function(n) {
			if (n == 1) {
				col <- standardCol 
			} else {
				if (n == 2) {
					colIndex <- c(2,10)
				} else if (n == 3) {
					colIndex <- c(2,6,10)
				} else if (n == 4) {
					colIndex <- c(2,4,8,10)
				} else if (n == 5) {
					colIndex <- c(2,4,6,8,10)
				} else if (n == 6) {
					colIndex <- c(1,3,5,7,9,11)
				} else if (n == 7) {
					colIndex <- 3:9
				} else if (n == 8) {
					colIndex <- c(2:5, 7:10)
				} else if (n == 9) {
					colIndex <- 2:10
				} else if (n == 10) {
					colIndex <- c(1:5, 7:11)
				} else if (n == 11) {
					colIndex <- 1:11
				}
				
				col <- c()
				for(i in colIndex) {
					col <- c(col, ColSequence[[i]])
				}
			}
			
			unname(col)
		}		
	}
}

#&====================================================================&#
#& scale_*_leuphana_continuous
#& scale_*_leuphana_gradient*
#&--------------------------------------------------------------------&#
#& Various continuous scales for color and fill
#&====================================================================&#
scale_color_leuphana_continuous <- function (invert = FALSE,
																						 low.overwrite = NULL,
																						 high.overwrite = NULL, ...) {
	
	if (is.null(low.overwrite)) {
		scaleLow <- leuphanaPalette$fillQualitative$grey1
	} else {
		scaleLow <- low.overwrite
	}
	
	if (is.null(high.overwrite)) {
		scaleHigh <- leuphanaPalette$colQualitative$red1
	} else {
		scaleHigh <- high.overwrite
	}
	
	if (invert) {
		tempLow <- scaleLow
		scaleLow <- scaleHigh
		scaleHigh <- tempLow
	}
	
	# now create the scale 
	scale_color_continuous(low  = scaleLow,
												 high = scaleHigh, ...)
} 

########################################################################
scale_color_leuphana_gradient <- function (invert = FALSE,
																					 low.overwrite = NULL,
																					 high.overwrite = NULL, ...) {
	
	if (is.null(low.overwrite)) {
		scaleLow <- leuphanaPalette$fillQualitative$grey1
	} else {
		scaleLow <- low.overwrite
	}
	
	if (is.null(high.overwrite)) {
		scaleHigh <- leuphanaPalette$colQualitative$red1
	} else {
		scaleHigh <- high.overwrite
	}
	
	if (invert) {
		tempLow <- scaleLow
		scaleLow <- scaleHigh
		scaleHigh <- tempLow
	}
	
	# now create the scale 
	scale_color_gradient(low  = scaleLow,
											high = scaleHigh, ...)
}

########################################################################
scale_color_leuphana_gradient2 <- function (invert = FALSE,
																						low.overwrite = NULL,
																						mid.overwrite = NULL,
																						high.overwrite = NULL, ...) {
	if (is.null(low.overwrite)) {
		scaleLow <- leuphanaPalette$colQualitative$blue1
	} else {
		scaleLow <- low.overwrite
	}
	
	if (is.null(mid.overwrite)) {
		scaleMid <- leuphanaPalette$fillQualitative$grey1
	} else {
		scaleMid <- mid.overwrite
	}
	
	if (is.null(high.overwrite)) {
		scaleHigh <- leuphanaPalette$colQualitative$red1
	} else {
		scaleHigh <- high.overwrite
	}
	
	if (invert) {
		tempLow <- scaleLow
		scaleLow <- scaleHigh
		scaleHigh <- tempLow
	}
	
	# now create the scale 
	scale_color_gradient2(low  = scaleLow,
												 mid  = scaleMid,
												 high = scaleHigh, ...)
}

########################################################################
# Fill Scales
scale_fill_leuphana_continuous <- function (invert = FALSE,
																						low.overwrite = NULL,
																						high.overwrite = NULL, ...) {
	
	if (is.null(low.overwrite)) {
		scaleLow <- leuphanaPalette$fillQualitative$grey1
	} else {
		scaleLow <- low.overwrite
	}
	
	if (is.null(high.overwrite)) {
		scaleHigh <- leuphanaPalette$fillQualitative$red1
	} else {
		scaleHigh <- high.overwrite
	}
	
	if (invert) {
		tempLow <- scaleLow
		scaleLow <- scaleHigh
		scaleHigh <- tempLow
	}
	
	# now create the scale 
	scale_fill_continuous(low  = scaleLow,
												high = scaleHigh, ...)
} 

########################################################################
scale_fill_leuphana_gradient <- function (invert = FALSE,
																					low.overwrite = NULL,
																					high.overwrite = NULL, ...) {
	
	if (is.null(low.overwrite)) {
		scaleLow <- leuphanaPalette$fillQualitative$grey1
	} else {
		scaleLow <- low.overwrite
	}
	
	if (is.null(high.overwrite)) {
		scaleHigh <- leuphanaPalette$fillQualitative$red1
	} else {
		scaleHigh <- high.overwrite
	}
	
	if (invert) {
		tempLow <- scaleLow
		scaleLow <- scaleHigh
		scaleHigh <- tempLow
	}
	
	# now create the scale 
	scale_fill_gradient(low  = scaleLow,
											high = scaleHigh, ...)
}

scale_fill_leuphana_gradient2 <- function (invert = FALSE,
																					 low.overwrite = NULL,
																					 mid.overwrite = NULL,
																					 high.overwrite = NULL, ...) {
	
	if (is.null(low.overwrite)) {
		scaleLow <- leuphanaPalette$fillQualitative$blue1
	} else {
		scaleLow <- low.overwrite
	}
	
	if (is.null(mid.overwrite)) {
		scaleLow <- leuphanaPalette$fillQualitative$grey1
	} else {
		scaleLow <- mid.overwrite
	}
	
	if (is.null(high.overwrite)) {
		scaleHigh <- leuphanaPalette$fillQualitative$red1
	} else {
		scaleHigh <- high.overwrite
	}
	
	if (invert) {
		tempLow <- scaleLow
		scaleLow <- scaleHigh
		scaleHigh <- tempLow
	}
	
	# now create the scale 
	scale_fill_gradient2(low  = scaleLow,
											 mid  = scaleMid,
											 high = scaleHigh, ...)
}

#&====================================================================&#
#& Leuphana Theme
#&--------------------------------------------------------------------&#
#& 
#&====================================================================&#
theme_leuphana <- function(base_size = 12) {
	myTheme <- theme_bw(base_family = "sans", base_size = base_size) + 
		theme(text = element_text(color = leuphanaPalette$theme$red, family = "sans"),
					plot.title = element_text(family = "serif", face = "bold", hjust = 0.5, size = (1.4*base_size)),
					plot.background = element_blank(),
					
					panel.background = element_blank(),
					panel.border = element_rect(fill = NULL, color = leuphanaPalette$theme$red, size = 1), 
					panel.grid = element_blank(),
					panel.grid.major.y = element_line(color = leuphanaPalette$theme$red_light, linetype = 2, size = 0.33),
					
					axis.line = element_line(color = leuphanaPalette$theme$red),
					axis.title = element_text(size = base_size),
					axis.ticks = element_line(color = leuphanaPalette$theme$red),
					axis.ticks.length = unit(0.2, "lines"),
					
					legend.background = element_blank(),
					legend.title = element_text(size = base_size, hjust = 0.5, family = "sans"),
					legend.key = element_rect(color = leuphanaPalette$theme$grey, fill = NULL), 
					legend.position = "right",
					
					strip.background = element_blank()
					)
	myTheme
}


# 
# # carsLm <- lm(dist ~ speed, data = cars)
# 
# groups = 4
# testdata <- data.frame(
# 		x = rnorm(groups*10, 0, 5),
# 		y = rnorm(groups*10, 0, 5),
# 		z = rep(1:groups, times = 10) )
# 
# testdata$z <- as.factor(testdata$z)
# 
# ggplot(data = testdata, aes(x, y, z, col = z)) +
# 	geom_point(size = 2) +
#  	ggtitle("RANDOM POINTS") +
# 	theme_leuphana() +
# 	xlab("X") +
# 	ylab("Y") +
# 	scale_color_leuphana()

