kelvin <- list()
kelvin$"2" <- read.csv("kelvincol/data/kelvin-2deg31.csv")
kelvin$"10" <- read.csv("kelvincol/data/kelvin-10deg64.csv")

temp.col <- colorRampPalette({
  k <- seq(900, 14000, length.out=100)
  r <- K2RGB(k, Y=splval(k))
  h <- rgb2hsv(t(r), maxColorValue=1)
  h[2,] <- h[2,] * 1.5
  h[2, h[2,] > 1] <- 1
  h <- hsv(h[1,], h[2,], h[3,])
  h
  })

templog.col <- colorRampPalette({
  k <- seq(log(1000), log(45000), length.out=100)
  k <- exp(k)
  r <- K2RGB(k, Y=splval(k), method="kim")
  h <- rgb2hsv(t(r), maxColorValue=1)
  h[2,] <- h[2,] * 1.5
  h[2, h[2,] > 1] <- 1
  h <- hsv(h[1,], h[2,], h[3,])
  h  
  })
  
temp.colors <- list(temp.col, templog.col)

suff <- expand.grid(c("x", "y", "P"), c("2", "10"), stringsAsFactors=FALSE)
K2 <- list()

for (i in 1:nrow(suff)) {
	K2[[i]] <- splinefun(kelvin[[suff[i, 2]]][c("K", suff[i, 1])])
}

names(K2) <- apply(suff, 1, paste, collapse="")
K2

rm(h, k, r, temp.col, templog.col, suff, i)
