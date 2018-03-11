XYZ2RGB <- function(X, Y, Z, gamma=TRUE, 
    limit=c("medium", "soft", "hard", "none")) {
   	
	limit <- match.arg(limit)
	
	if (length(X) > 1 & missing(Y)) {
		XYZ <- X
	} else {
	    XYZ <- cbind(X, Y, Z)
	}
	
	m <- matrix(c(
      3.2404542, -1.5371385, -0.4985314,
     -0.9692660,  1.8760108,  0.0415560,
      0.0556434, -0.2040259,  1.0572252),
      byrow=TRUE, nrow=3)
    
    RGB <- XYZ %*% t(m)
    
    colnames(RGB) <- c("R", "G", "B")
            
    if (limit %in% c("medium", "soft", "hard")) {
    	RGB[,1] <- limiter(RGB[,1], type=limit)
    	RGB[,2] <- limiter(RGB[,2], type=limit)
    	RGB[,3] <- limiter(RGB[,3], type=limit)
    }
        
    g <- function(RGB) {
    	s <- RGB <= 0.0031308
    	
    	RGB[s]  <-  12.92 * RGB[s]
    	RGB[!s] <- (1.055 * RGB[!s]^(1/2.4)) - 0.055
        RGB
    }
        
    if (gamma) {
    	cc <- g(RGB)
    } else {
    	cc <- RGB
    }
    cc
}

K2RGB <- function(K, Y=0.5, deg=c("2", "10"), gamma=TRUE, 
  limit=c("medium", "soft", "hard", "none", "linear")) {
  	
  	deg <- match.arg(deg)
  	limit <- match.arg(limit)
	
	XYZ <- K2XYZ(K, Y, deg)
	RGB <- apply(XYZ, 1, XYZ2RGB, gamma=gamma, limit=limit)
	RGB <- t(RGB)
	colnames(RGB) <- c("R", "G", "B")
	RGB
}

K2hex <- function(K, Y=0.5, deg=c("2", "10"), gamma=TRUE, 
  limit=c("medium", "soft", "hard", "none", "linear"), alpha=1) {
  	
  	deg <- match.arg(deg)
  	limit <- match.arg(limit)
	
	XYZ <- K2XYZ(K, Y, deg)
	RGB <- apply(XYZ, 1, XYZ2RGB, gamma=gamma, limit=limit)
	RGB <- t(RGB)
	colnames(RGB) <- c("R", "G", "B")
	rgb(RGB[,"R"], RGB[,"G"], RGB[,"B"], alpha)
}
