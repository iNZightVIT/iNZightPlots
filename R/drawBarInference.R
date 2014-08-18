drawBarInference <- function(x, y = NULL, opts) {
  # A function which calculates the inferences lines necessary for the
  # plot. Returns a list of lines (for each bar, essentially) with
  # comparison and confidence intervals.

  # Some error checking ... but only send the error message once.
    if (!is.null(opts$inference.par))
        if (opts$inference.par != "proportion")
            warning('Invalid inference parameter: please remove inference.par, or set it equal to "proportion".')

  # Make a table
    tab <- if (is.null(y)) table(x) else table(y, x)
   
    if (length(dim(tab)) == 1) {
      # dealing with a single X factor
        phat <- matrix(makeBars(x), nrow = 1)
        
        n <- sum(tab)

        size.comp <- matrix(1.96 * errorbarsize(proportionCovs(tab)),
                            nrow = 1)
        
        se <- matrix(sqrt(phat * (1 - phat) / n), nrow = 1)
        size.conf <- 1.96 * se

        Phat <- phat
        phat[tab < 5] <- NA

        if (n > 0) {
            comp <- if ("comp" %in% opts$inference.type) {
                list(low = phat - size.comp, upp = phat + size.comp)
            } else NULL
            
            conf <- if ("conf" %in% opts$inference.type) {
                list(low = phat - size.conf, upp = phat + size.conf)
            } else NULL
            
            max <- max(comp$upp, conf$upp, Phat, na.rm = TRUE)
        } else {
            comp <- NULL
            conf <- NULL
            max <- 0
        }
    } else {
        ## for two variables x, y 
        ## a native version is also providing here:
        phat <- tab / rowSums(tab)
        se <- sqrt(sweep(phat * (1 - phat), 1, rowSums(tab), "/"))

        ii <- rowSums(tab) > 0

        
        if (any(rowSums(tab) > 0)) {
            ## The moecalc function returns errors that iNZight users don't need to know about ...
            op <- options(warn = -1)
            out <- lapply(1:ncol(tab), function(i)
                          iNZightMR:::moecalc(iNZightMR:::seBinprops(rowSums(tab)[ii], phat[ii, i]), est = phat[ii, i]))
            options(op)
            
            Phat <- phat
            phat[tab < 5] <- NA
            
            comp <-
                if ("comp" %in% opts$inference.type) {
                    low <- upp <- phat * 0
                    low[ii, ] <- sapply(out, function(x) x$compL)
                    upp[ii, ] <- sapply(out, function(x) x$compU)
                    
                    list(low = low, upp = upp)
                } else NULL
            
            conf <-
                if ("conf" %in% opts$inference.type) {
                    list(low = phat - sqrt(phat * (1 - phat) / rowSums(tab)) * 1.96,
                         upp = phat + sqrt(phat * (1 - phat) / rowSums(tab)) * 1.96)
                } else NULL

            max <- max(comp$upp, conf$upp, Phat, na.rm = TRUE)
        } else {
            comp <- NULL
            conf <- NULL
            max <- 0
        }
    }

    

    list(comp = comp, conf = conf, max = max, n = nrow(as.matrix(phat)))
   
}

drawInferenceLines <- function(x, i, xx, opts, guides) {
  # Draws inference lines stored in a list!
  # x is a list of stuff
  # xx is the x positions
  # opts are all other options

    if (!is.null(x$conf)) {     
        low <- pmax(0, x$conf$low[, i])
        upp <- pmin(1, x$conf$upp[, i])
 
        for (j in 1:x$n) {
            y <- c(low[j], upp[j])
            if (any(is.na(y))) next
            
            grid.lines(xx[j], unit(y, "native"),
                       gp =
                       gpar(col = opts$inf.col.conf,
                            lwd = opts$inf.lwd.conf / sqrt(x$n),
                            lineend = "butt"))
        }
    }

    if (!is.null(x$comp)) {
        low <- pmax(0, x$comp$low[, i])
        upp <- pmin(1, x$comp$upp[, i])
 
        for (j in 1:x$n) {
            y <- c(low[j], upp[j])
            if (any(is.na(y))) next

            grid.lines(xx[j], unit(y, "native"),
                       gp =
                       gpar(col = opts$inf.col.comp,
                            lwd = opts$inf.lwd.comp / sqrt(x$n),
                            lineend = "butt"))
            
            if (guides == 'group') {
                grid.polyline(rep(c(0.05, 0.95), 2),
                              unit(rep(y, each = 2), "native"),
                              id = rep(1:2, each = 2),
                              gp = gpar(lty = 3, col = "grey50", lwd = 1))
            }
        }
    }
}
