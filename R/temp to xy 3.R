K2u <- function(K) {
	# Krystek, Michael P. (January 1985)
	# nøyaktig til 8*(10^-5) for K ∈ (1000, 15000)
	num <- 0.860117757 + 1.54118254*(10^-4)*K + 1.28641212*(10^-7)*(K^2)
	den <- 1 - 8.42420235*(10^-4)*K + 7.08145163*(10^-7)*(K^2)
	num/den
}

K2v <- function(K) {
	# Krystek, Michael P. (January 1985)
	# nøyaktig til 9*(10^-5) for K ∈ (1000, 15000)
	num <- 0.317398726 + 4.22806245*(10^-5)*K + 4.20481691*(10^-8)*(K^2)
	den <- 1 - 2.89741816*(10^-5)*K + 1.61456053*(10^-7)*(K^2)
	num/den
}

uv2xy <- function(u, v, prime=TRUE) {
	if (length(u) > 1 & missing(v)) {
		q <- u
		u <- q[, 1]
		v <- q[, 2]
	}
	if (!prime) v <- v*1.5
    x <- 9*u / (6*u - 16*v + 12)
    y <- 4*v / (6*u - 16*v + 12)
    cbind(x, y)
}

K2xy3 <- function(K) {
	uv2xy(K2u(K), K2v(K))
}
