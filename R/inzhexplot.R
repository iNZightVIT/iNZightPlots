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
    if (xattr$class == "inz.freq") {
        W <- df$freq
    } else {
        W <- weights(obj$df, "sampling")[!missing]
    }
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

    # Smoothers and quantiles:
    ## if (length(opts$quant.smooth) > 0) {
    ##     qs <- try(calcQSmooth(obj$svy, opts$quant.smooth, opts), TRUE)
    ##     if (!inherits(qs, "try-error")) {
    ##         qp <- qs$qp
    ##         lty <- qs$lty
    ##         lwd <- qs$lwd
    ##         for (q in 1:length(qp))
    ##             try(addQuantileSmoother(obj$svy, NULL, quantile = qp[q],
    ##                                     col = opts$col.smooth,
    ##                                     lty = lty[q], lwd = lwd[q]), TRUE)
    ##     }
    ## } else if (!is.null(opts$smooth)) {
    ##   # Smoothers
    ##     if (opts$smooth != 0) {
    ##         if (opts$smooth > 1) {
    ##             warning("Smoothing value must be in the interval [0, 1]")
    ##         } else {
    ##             if (length(unique(obj$col)) == 1 | !opts$trend.by) {
    ##                 try(addSmoother(obj$svy, NULL, f = opts$smooth,
    ##                                 col = opts$col.smooth, bs = opts$bs.inference), TRUE)
    ##             } else {
    ##                 byy <- as.factor(obj$col)  # pseudo-by-variable
    ##                 xtmp <- lapply(levels(byy), function(c) subset(obj$x, obj$col == c))
    ##                 ytmp <- lapply(levels(byy), function(c) subset(obj$y, obj$col == c))
                    
    ##                 for (b in 1:length(levels(byy)))
    ##                     try(addSmoother(xtmp[[b]], ytmp[[b]],
    ##                                     f = opts$smooth,
    ##                                     col = darken(levels(byy)[b]),
    ##                                     bs = FALSE, lty = 2), TRUE)
    ##             }
    ##         }
    ##     }
    ## }

    addXYsmoother(obj, opts, col.args, xlim, ylim, x = obj$svy, y = NULL)
    addXYtrend(obj, opts, col.args, xlim, ylim, x = obj$svy, y = NULL)
    
    invisible(NULL)
}
