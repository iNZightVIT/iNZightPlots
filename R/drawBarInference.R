drawBarInference <- function(x, y = NULL, opts) {
  # A function which calculates the inferences lines necessary for the
  # plot. Returns a list of lines (for each bar, essentially) with
  # comparison and confidence intervals.
    
  # Some error checking ... but only send the error message once.
    if (!is.null(opts$inference.par))
        if (opts$inference.par != "proportion" & i == 1)
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
    } else {
      # dealing with an X and Y factor combination
        phat <- makeBars(x, y)
        n <- rowSums(tab)
        
        size.comp <- 1.96 *
            t(apply(tab, 1, function(r) errorbarsize(proportionCovs(r))))

        se <- sqrt(sweep(phat * (1 - phat), 1, n, "/"))
    }

    size.conf <- 1.96 * se

    Phat <- phat
    phat[tab < 5] <- NA

  # Create comparison intervals
    if ("comp" %in% opts$inference.type)
        comp <- list(low = phat - size.comp, upp = phat + size.comp)
    else
        comp <- NULL

  # Create confidence intervals
    if ("conf" %in% opts$inference.type)
        conf <- list(low = phat - size.conf, upp = phat + size.conf)
    else
        conf <- NULL

    list(comp = comp, conf = conf,
         max  = max(comp$upp, conf$upp, Phat, na.rm = TRUE),
         n    = ncol(as.matrix(phat)))
}

drawInferenceLines <- function(x, i, xx, opts) {
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
                            lwd = opts$inf.lwd.conf / sqrt(x$n)))
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
                            lwd = opts$inf.lwd.comp / sqrt(x$n)))
        }
    }
}
