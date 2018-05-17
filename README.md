# kelvincol

Utilities for converting [colour temperatures](https://en.wikipedia.org/wiki/Color_temperature) to displayable rgb values.

Various algorithms and methods are available

```R

install_github("AkselA/R-kelvincol")
library(kelvincol)

kel <- c(1000, 1200, 1500, 2000, 4000, 10000, 20000)

K2XYZ(kel)
K2RGB(kel)
K2hex(kel)

K2hex(kel, method="kim")
K2hex(kel, method="krystek")
```
\
As there is no 'right' way to decide luminance, various methods that aim to give pleasing and functional results are supplied.
```R
opar <- par(no.readonly = TRUE)
par(mfrow=c(4, 1), mar=c(2, 2, 0.5, 1))

k <- seq(900, 15000, 400)

plot(k, pch=21, cex=6, lwd=0.5, bg=K2hex(k, Y=0.8, alpha=0.9))
plot(k, pch=21, cex=6, lwd=0.5, bg=K2hex(k, Y=K2Y(k), alpha=0.9))
plot(k, pch=21, cex=6, lwd=0.5, bg=K2hex(k, Y=satval(k), alpha=0.9))
plot(k, pch=21, cex=6, lwd=0.5, bg=K2hex(k, Y=splval(k), alpha=0.9))

par(old.par)
```
\
Other ancillary functions are also included, like a couple of 'heat-style' palettes.
```R
opar <- par(no.readonly = TRUE)
par(mar=c(2, 2, 0.5, 0.5), mfrow=c(2, 2))

image(matrix(1:(16*5), ncol=16), col=kelvin.colors(10^4))
image(matrix(1:(16*5), ncol=16), col=kelvinlog.colors(10^4))

image(jitter(volcano, 2), col=kelvin.colors(10^4))
image(jitter(volcano, 2), col=kelvinlog.colors(10^4))

par(old.par)
```
