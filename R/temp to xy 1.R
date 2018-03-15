#' @export
splinegen <- function() {
    suff <- expand.grid(c("x", "y", "P"), c("2", "10"), stringsAsFactors=FALSE)
    K2 <- list()

    for (i in 1:nrow(suff)) {
    	K2[[i]] <- splinefun(kelvin[[suff[i, 2]]][c("K", suff[i, 1])])
    }

    names(K2) <- apply(suff, 1, paste, collapse="")
    K2
}

