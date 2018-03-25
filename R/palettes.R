#' @export
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
    assign("temp.colors", temp.colors)
    
}

# #' @export
# kelvin.colors <- function(n) kelvincol::temp.colors[[1]]


# #' @export
# kelvinlog.colors <- function(n) temp.colors[[2]](n)

# image2(matrix(1:(16*16), 16), col=templog.colors(16*16))
# plot(1:16, bg=temp.colors(16), pch=21, cex=6)


# k <- seq(900, 15000, 1000)
# r <- K2RGB(k, Y=splval(k))
# h <- rgb2hsv(t(r), maxColorValue=1)
# hsv(h[1,], h[2,], h[3,])

# Y=splval(k)
# Y <- (Y-0.01)/0.97
# range(Y)