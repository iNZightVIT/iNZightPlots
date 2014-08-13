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
        phat <- tab / sum(tab)
        if (length(phat) < 3) # binary case
            out <- iNZightMR:::moecalc(iNZightMR:::seBinprops(tab, phat), est = phat)
        else
            out <- iNZightMR:::moecalc(iNZightMR:::seMNprops(sum(tab), phat), est = phat) 
        ## notice this is not an error
        ## the detail listed in iNZightMR/R/ses.moecalc.R row 50 - 73 
        comp <-
            if ("comp" %in% opts$inference.type) list(low = matrix(out$compL, nrow = 1),
                                                      upp = matrix(out$compU, nrow = 1))
            else NULL
        print(comp)
        conf <-
            if ("conf" %in% opts$inference.type) list(low = matrix(out$confL, nrow = 1),
                                                      upp = matrix(out$confU, nrow = 1))
            else NULL
        print(conf)
    } else {
        ## for two variables x, y 
        ## a native version is also providing here:
        phat <- tab / rowSums(tab)
        out <- lapply(1:nrow(tab), function(i)
                      iNZightMR:::moecalc(iNZightMR:::seBinprops(tab[i, ], phat[i, ]), est = phat[i, ]))
        
        comp <-
            if ("comp" %in% opts$inference.type) {
                list(low = t(sapply(out, function(x) x$compL)),
                     upp = t(sapply(out, function(x) x$compU)))
            } else NULL
        
        conf <-
            if ("conf" %in% opts$inference.type) {
                list(low = t(sapply(out, function(x) x$confL)),
                     upp = t(sapply(out, function(x) x$confU)))
            } else NULL
    }
  
    Phat <- phat
    phat[tab < 5] <- NA
    comp <- lapply(comp, function(m) { m[tab < 5] <- NA; m})
    conf <- lapply(conf, function(m) { m[tab < 5] <- NA; m})
    
    list(comp = comp, conf = conf,
         max  = max(comp$upp, conf$upp, Phat, na.rm = TRUE),
         n    = nrow(as.matrix(phat)))
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
