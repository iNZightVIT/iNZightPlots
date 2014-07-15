rescale <- function(x) {
    r <- 4 * (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE)) + 0.5
    r[is.na(x)] <- NA
    r
}

Darken <- function(x = "#FFFFFF") {
    x <- gsub('#', '', x)
    if (nchar(x) == 3)
        x <- paste(rep(strsplit(x, '')[[1]], each = 2), collapse = '')

    ## catch any transparency added to the colours
    if (nchar(x) == 8) {
        alpha <- substr(x, 7, 8)
        x <- substr(x, 1, 6)
    } else {
        alpha <- ""
    }

    if (nchar(x) != 6)
        stop("Not a valid hexadecimal code!")

  # Now start the function!

    r <- substr(x, 1, 2)
    g <- substr(x, 3, 4)
    b <- substr(x, 5, 6)

    dark <- strtoi(c(r, g, b), base = 16) * 0.6 / 255
    
    paste0(rgb(dark[1], dark[2], dark[3]), alpha)
}

darken <- Vectorize(Darken)  # allow it to work on a vector of Xs


convert.to.factor <-
function(x) {
    if (is.factor(x)) {
      # to simplify coding elsewhere, allow convert to factor to simply return
      # the supplied x vector if it is already a factor.
        x.fact <- x
    } else {
    
        ## converts a 
        if (length(unique(x)) < 5)
            x.fact <- factor(x)
        else {  
            x.quantiles <- round((quantile(x, na.rm = TRUE)), 0)  
            x.fact <- try(cut(x, c(-Inf, ifelse(unique(x.quantiles[2:4]) == 3,
                                                x.quantiles[2:4],
                                                unique(x.quantiles[2:4])),
                                   Inf)))
            
            if (inherits(x.fact, "try-error")) {
                eps <- .Machine$double.eps
                x.quantiles <- round((quantile(x, na.rm = TRUE)), 2) + eps * (0:10)
                x.fact <- cut(x, c(-Inf, ifelse(unique(x.quantiles[2:4]) == 3,
                                                x.quantiles[2:4],
                                                unique(x.quantiles[2:4])),
                                   Inf))
            }
            
            if ((x.quantiles[2] == x.quantiles[3]) && (x.quantiles[3] == x.quantiles[4]))
                levels(x.fact) <-
                    c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
                      paste(c("(", x.quantiles[2], " - ", x.quantiles[5], "]"), collapse = ""))
            else if (x.quantiles[2] == x.quantiles[3])
                levels(x.fact) <-
                    c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
                      paste(c("(", x.quantiles[2], " - ", x.quantiles[4], "]"), collapse = ""),
                      paste(c("(", x.quantiles[4], " - ", x.quantiles[5], "]"), collapse = ""))
            else if (x.quantiles[3] == x.quantiles[4])
                levels(x.fact) <-
                    c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
                      paste(c("(", x.quantiles[2], " - ", x.quantiles[3], "]"), collapse = ""),
                      paste(c("(", x.quantiles[3], " - ", x.quantiles[5], "]"), collapse = ""))
            else
                levels(x.fact) <-
                    c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
                      paste(c("(", x.quantiles[2], " - ", x.quantiles[3], "]"), collapse = ""),
                      paste(c("(", x.quantiles[3], " - ", x.quantiles[4], "]"), collapse = ""),
                      paste(c("(", x.quantiles[4], " - ", x.quantiles[5], "]"), collapse = ""))
        }
    }

  # Remove any empty levels -_-
    factor(x.fact) 
}


nullPlot <- function(otps, xattr) {
    # simply draw nothing!
    out <- list(xlim = c(-Inf, Inf), ylim = c(-Inf, Inf))
    class(out) <- "inznull"
    out
}
plot.inznull <- function(...) {
    return(invisible(NULL))
}


genCols <- function(n) {
    # This allows us to simply edit the type of colour schemes used
    #rainbow(n, v = 0.7, start = 1/6)
    hcl((1:n) / n * 360, c = 80, l = 50)
}

colourPoints <- function(x, col.args, opts = inzpar()) {
    if (is.null(x))
        return(opts$col.pt)
    xclass <- ifelse(is.numeric(x), "numeric", "factor")
    switch(xclass,
           "numeric" = {
               xr <- col.args$n.range
               xm <- xr[1]
               xsc <- as.integer(199 * ((x - xm) / diff(xr)) + 1)
               ifelse(is.na(x), col.args$missing, col.args$n.cols[xsc])
           }, "factor" = {
               x <- as.character(x)
               x[is.na(x)] <- "missing"
               col.args$f.cols[x]
           })
}
