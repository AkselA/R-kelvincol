K2XYZ <- function(K, Y=0.5, deg=c("2", "10")) {
	deg <- match.arg(deg)
	x <- K2x(K, deg)
	y <- K2y(K, deg)
	xyY2XYZ(x, y, Y)
}
