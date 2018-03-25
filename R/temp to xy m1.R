
splinegen <- function() {
    suff <- expand.grid(c("x", "y", "P"), c("2", "10"), stringsAsFactors=FALSE)
    K2 <- list()

    for (i in 1:nrow(suff)) {
    	K2[[i]] <- splinefun(kelvin[[suff[i, 2]]][c("K", suff[i, 1])])
    }

    names(K2) <- apply(suff, 1, paste, collapse="")
    K2
}


#' @export
K2xy1 <- function(K, deg=c("2", "10")) {
	# Mitchell N. Charity
	# http://www.vendian.org/mncharity/dir3/blackbody/
	deg <- match.arg(deg)
	tryCatch({
      x <- kelvinK2[[paste0("x", deg)]](K)
	  y <- kelvinK2[[paste0("y", deg)]](K)
	  cbind(x, y)
	  }, 
	  error=function(e) {
	  	  message("exported \'kelvinK2\'")
	      kelvinK2 <<- kelvincol:::splinegen()
          x <- kelvinK2[[paste0("x", deg)]](K)
          y <- kelvinK2[[paste0("y", deg)]](K)
          cbind(x, y)
	  }
	  )
}

K2xy1(2000)