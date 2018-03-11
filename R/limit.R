fitrange <- function(W, lower=0, upper=1) {
	if (lower > upper) warning("upper bound must be strictly larger than lower bound")
	if (length(W) == 0) return(numeric(0))
	newrange <- upper - lower
	oldrange <- max(W, na.rm=TRUE) - min(W, na.rm=TRUE)
	if (oldrange == 0) {
		d <- abs(W - lower) < abs(W - upper)
		ifelse(d, lower, upper)
	} else {
	    (W - min(W, na.rm=TRUE)) * (newrange/oldrange) + lower
    }
}

limiter <- function(x, type=c("medium", "soft", "hard")) {
	type <- match.arg(type)

	dtf <- data.frame(
      x=c(-0.5, 0, 0.29, 0.5, 0.71, 1, 1.5), 
      y=c( 0,   0, 0.25, 0.5, 0.75, 1, 1  ))

    switch(type,
      "hard" = {
	    x[x < 0] <- 0
	    x[x > 1] <- 1
	    x
	  },
	  "medium" = {
	  	spline(dtf, xout=x, method="hyman")$y
	  },
	  "soft" = {
	  	spline(dtf[-c(3, 5),], xout=x, method="hyman")$y
	  }
	)
}
