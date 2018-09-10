 
temp.colors.fun <- function() {	
    temp <- colorRampPalette({
      k <- seq(900, 14000, length.out=100)
      r <- K2RGB(k, Y=splval(k))
      h <- rgb2hsv(t(r), maxColorValue=1)
      h[2,] <- h[2,] * 1.5
      h[2, h[2,] > 1] <- 1
      h <- hsv(h[1,], h[2,], h[3,])
      h
      })
    
      # image2(matrix(1:(16*16), 16), col=temp.colors(16*16))
    
    templog <- colorRampPalette({
      k <- seq(log(1000), log(45000), length.out=100)
      k <- exp(k)
      r <- K2RGB(k, Y=splval(k), method="kim")
      h <- rgb2hsv(t(r), maxColorValue=1)
      h[2,] <- h[2,] * 1.5
      h[2, h[2,] > 1] <- 1
      h <- hsv(h[1,], h[2,], h[3,])
      h  
      })
    
    temp.colors <- list(temp, templog)
    temp.colors
}

# there's probably a neater way of doing this using .onLoad or .onAttach

#' @export
kelvin.colors <- function(n) {
	tryCatch(temp.colors1(n), 
	  error=function(e) {
	  	  message("exported \'temp.colors1\'")
	      temp.colors1 <<- kelvincol:::temp.colors.fun()[[1]]
	  	  temp.colors1(n)
	  }
	  )
}


#' @export
kelvinlog.colors <- function(n) {
	tryCatch(temp.colors2(n), 
	  error=function(e) {
	  	  message("exported \'temp.colors2\'")
	      temp.colors2 <<- kelvincol:::temp.colors.fun()[[2]]
	  	  temp.colors2(n)
	  }
	  )
}

# opar <- par(no.readonly = TRUE)
# par(mar=c(2, 2, 0.5, 0.5), mfrow=c(2, 2))

# image(matrix(1:(16*5), ncol=16), col=kelvin.colors(10^4))
# image(matrix(1:(16*5), ncol=16), col=kelvinlog.colors(10^4))

# image(jitter(volcano, 2), col=kelvin.colors(10^4))
# image(jitter(volcano, 2), col=kelvinlog.colors(10^4))

# par(old.par)