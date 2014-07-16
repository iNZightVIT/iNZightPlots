create.inz.hexplot <- function(obj) {
    # make a plot using hexagonal binning

    # take the dataframe and settings from the object
    df <- obj$df
    opts <- obj$opts
    xattr <- obj$xattr
    
    if (xattr$class == "inz.survey")
        df <- df$variables
    
    v <- colnames(df)
    vn <- xattr$varnames

    # first need to remove missing values
    missing <- apply(df[ , v %in% c("x", "y")], 1, function(x) any(is.na(x)))
    n.missing <- sum(missing)
    df <- df[!missing, ]

    xbins <-
        if (is.null(opts$hex.bins)) {
            if ((cpt <- opts$cex.pt) > 1) {
                (1 - (cpt - 1) / 2.5) * (25) + 5
            } else {
                (1 - (cpt - 0.05) / 0.95) * 70 + 30
            }
        } else {
            opts$hex.bins
        }
    
    ## hexbin returns an S4 object, so need to use the @ operator
    hb <- hexbin(df$x, df$y, IDs = TRUE, xbins = xbins)
    
    cellid <- hb@cID
    ## now manipulate the counts with the weight variable
    W <- switch(xattr$class,
                "inz.freq" = df$freq,
                "inz.survey" = weights(obj$df, "sampling")[!missing],
                "inz.simple" = rep(1, nrow(df)))

    hb@count <- as.vector(tapply(W, cellid, sum))
    hb@xcm <- as.vector(tapply(1:length(df$x), cellid,
                               function(i) weighted.mean(df$x[i], W[i])))
    hb@ycm <- as.vector(tapply(1:length(df$y), cellid,
                               function(i) weighted.mean(df$x[i], W[i])))

    out <- list(hex = hb, n.missing = n.missing, svy = obj$df,
                colby = df$colby, nacol = FALSE,
                xlim = if (nrow(df) > 0) range(df$x, na.rm = TRUE) else c(-Inf, Inf),
                ylim = if (nrow(df) > 0) range(df$y, na.rm = TRUE) else c(-Inf, Inf))
    class(out) <- "inzhex"

    out
}

plot.inzhex <- function(obj, gen) {
    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    opts <- gen$opts
    mcex <- gen$mcex
    col.args <- gen$col.args

    grid.hexagons(obj$hex, style = "centroids", maxcnt = gen$maxcount)
    
    ## ---------------------------------------------------------------------------- ##
    ## Now that the main plot has been drawn, time to add stuff to it!

    # Line of Equality (LOE)
    if (opts$LOE) {
        xx <- c(min(xlim, ylim), max(xlim, ylim))
        grid.lines(xx, xx, default.units = "native",
                   gp = gpar(col = opts$col.LOE, lty = opts$lty.LOE))
    }

    addXYsmoother(obj, opts, col.args, xlim, ylim)
    addXYtrend(obj, opts, col.args, xlim, ylim)
    
    invisible(NULL)
}
