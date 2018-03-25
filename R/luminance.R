#' @export
K2Y <- function(K, shape=1.15, bright=0.1) {
	# This has nothing to do with a real relationhip between
	# colour temperature and luminance.
    Y3 <- ((log(K) - 6.8)/3)^shape
    limiter((Y3*shape) + (K*0.00001*bright))
}

#' @export
RGB2sat <- function(R, G, B) {
	if (length(R) > 1 & missing(G)) {
		RGB <- R
	} else {
	    RGB <- cbind(R, G, B)
	}
	I <- rowMeans(RGB)
	m <- apply(RGB, 1, min)
	S <- I
	S[I != 0] <- 1 - (m[I != 0]/S[I != 0])
	S
}

#' @export
satval <- function(K, slope=TRUE) {
	sat <- 1.05 - RGB2sat(K2RGB(K, Y=0.2))
	sat <- sat/1.08
	if (slope) {
	    sat <- sat + log(K)*0.08 - 0.59
	    sat <- sat / 1.11
	}
	sat
}

#' @export
splval <- splinefun(
  data.frame(
  x=c(800,   1000,  1500, 2200, 2700, 3500, 5500, 9000, 13000, 20000, 40000),
  y=c(0.005, 0.018, 0.15, 0.45, 0.6,  0.74, 0.93, 0.89, 0.79,  0.7,   0.6)), 
  method="natural")

# ###
# k <- seq(log(900), log(40000), length.out=100)
# k <- exp(k)
# par(mfrow=c(1, 1), mar=c(2, 2, 1, 1))
# plot(k, K2Y(k), log="x", cex.axis=0.8, type="l")
# lines(k, satval(k), col="red")
# lines(k, satval(k, FALSE), col="green")
# lines(k, splval(k), col="blue")

# ###
# k <- seq(log(1000), log(40000), length.out=40)
# k <- exp(k)
# par(mfrow=c(5, 1), mar=c(2, 2, 0.5, 1))
# plot(k, seq_along(k), cex=6, pch=21, lwd=0.5, bg=K2hex(k, Y=0.8), log="x")
# plot(k, seq_along(k), cex=6, pch=21, lwd=0.5, bg=K2hex(k, Y=K2Y(k)), log="x")
# plot(k, seq_along(k), cex=6, pch=21, lwd=0.5, bg=K2hex(k, Y=satval(k)), log="x")
# plot(k, seq_along(k), cex=6, pch=21, lwd=0.5, bg=K2hex(k, Y=splval(k)), log="x")
# plot(k, seq_along(k), cex=6, pch=21, lwd=0.5, bg=K2hex(k, Y=log(K2$P2(k)/10^9 + 2)/20), log="x")

# ###
# k <- seq(900, 15000, 400)
# par(mfrow=c(5, 1), mar=c(2, 2, 0.5, 1))
# plot(k, pch=21, cex=6, lwd=0.5, bg=K2hex(k, Y=0.8, alpha=0.9))
# plot(k, pch=21, cex=6, lwd=0.5, bg=K2hex(k, Y=K2Y(k), alpha=0.9))
# plot(k, pch=21, cex=6, lwd=0.5, bg=K2hex(k, Y=satval(k), alpha=0.9))
# plot(k, pch=21, cex=6, lwd=0.5, bg=K2hex(k, Y=splval(k), alpha=0.9))
# plot(k, pch=21, cex=6, lwd=0.5, bg=K2hex(k, Y=log(K2$P2(k)/10^9 + 2)/20, alpha=0.9))

