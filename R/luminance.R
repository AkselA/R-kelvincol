K2Y <- function(K, shape=1.2, bright=0) {
	# This has nothing to do with a real relationhip between
	# colour temperature and luminance.
    Y3 <- ((log(K) - 6.8)/3)^shape
    (Y3*shape)+(K*0.00001*bright)
}

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

satval <- function(K, slope=0.22) {
	sat <- 1-RGB2sat(K2RGB(K))
	sat <- sat + log(K)*slope - 10*slope
	(sat + 0.7) / 1.6
}
