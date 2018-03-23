
#' @export
K2xy <- function(K, method=c("charity2", "charity10", "kim", "krystek")) {
	method <- match.arg(method)
	switch(method,
	       charity2=K2xy1(K, "2"),
	       charity10=K2xy1(K, "10"),
	       kim=K2xy2(K),
	       krystek=K2xy3(K)
	       )
}

s <- seq(1500, 4500, by=50)
plot(K2xy(s), type="n")
lines(K2xy(s, "charity2"), lwd=2, col="#ff000088")
lines(K2xy(s, "charity10"), lwd=2, col="#00ff0088")
lines(K2xy(s, "kim"), lwd=2, col="#0000ff88")
lines(K2xy(s, "krystek"), lwd=2, col="#ff00ff88")

#' @export
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


#' @export
K2XYZ <- function(K, Y=0.5, method="charity2") {
	xy <- K2xy(K, method)
	xyY2XYZ(xy[,1], xy[,2], Y)
}

