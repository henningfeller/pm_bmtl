# runif_sphere_plot.R

library(plotly)
library(ggplot2)
library(ggforce)
source("LKJdistribution.R")


# two dimensional
points <- as.data.frame(runif_spere(1000, 2))
colnames(points) <- c("X", "Y")

circles <- data.frame(x0 = c(0), y0 = c(0))

ggplot() +
	# geom_circle(data = circles, aes(x0=x0, y0=y0, r=1), col=leuphanaPalette$colQualitative$grey1) +
	geom_point(data = points, aes(x = X, y = Y), col = leuphanaPalette$theme$red, size = 0.1) +
	theme_coord() +
	xlim(-1.2, 1.2) +
	ylim(-1.2, 1.2)


# three dimensional
x <- seq(-1,1,length.out = 101)
z <- matrix(0, nrow = 101, ncol = 101)

for (i in 1:101) {
	for (j in 1:101) {
		z[i,j] <- sqrt(1 - x[i]^2 - x[j]^2)
	}
}


points <- as.data.frame(runif_spere(100, 3))
colnames(points) <- c("X", "Y", "Z")
plot_ly(points, x = ~X, y = ~Y, z = ~Z, size = 0.33, color = 1, colors = leuphanaPalette$theme$red, type="scatter3d")
