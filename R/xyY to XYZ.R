xyY2XYZ <- function(x, y, Y=0.5) {
	if (length(x) > 1 & missing(y)) {
		q <- x
		x <- q[, 1]
		y <- q[, 2]
		if (length(q) == 3) Y <- x[, 3]
	}
	
	X <- (Y/y) * x
	Z <- (Y/y) * (1 - x - y)
	cbind(X, Y, Z)
}
