create.inz.scatterplot <- function(obj) {
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

    # --- look through inzpar for settings

    # Jitter on x and y
    if ("x" %in% strsplit(opts$jitter, '')[[1]])
        df$x <- jitter(df$x)
    if ("y" %in% strsplit(opts$jitter, '')[[1]])
        df$y <- jitter(df$y)


    ## The plotting symbol:
    pch <- rep(ifelse(opts$alpha == 1,
                      ifelse(opts$fill.pt == "transparent", opts$pch, 21),
                      19), nrow(df))

    ## --- this is where FREQUENCY or SURVEY information is used to control sizes of points
    # size of points
    if ("freq" %in% v) {
        propsize <- df$freq / xattr$max.freq * 4 + 0.5
    } else if ("weights" %in% v) {
        propsize <- df$weights
    } else if ("sizeby" %in% v) {
        propsize <- df$sizeby
    } else {
        propsize <- 1
    }
    
    pch[is.na(propsize)] <- 4
    propsize[is.na(propsize)] <- 0.6

    # Combine everything together into a classed list which will have a `plot` method
    out <- list(x = df$x, y = df$y, colby = df$colby, propsize = propsize, pch = pch,
                fill.pt = opts$fill.pt, n.missing = n.missing,
                nacol = if ("colby" %in% v) any(is.na(df$colby)) else FALSE,
                nasize = if ("sizeby" %in% v) any(is.na(df$sizeby)) else FALSE,
                xlim = if (nrow(df) > 0) range(df$x, na.rm = TRUE) else c(-Inf, Inf),
                ylim = if (nrow(df) > 0) range(df$y, na.rm = TRUE) else c(-Inf, Inf))

    class(out) <- "inzscatter"

    out
}


plot.inzscatter <- function(obj, gen) {
    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    opts <- gen$opts
    mcex <- gen$mcex
    col.args <- gen$col.args

    if (length(obj$x) == 0)
        return()

    grid.points(obj$x, obj$y, pch = obj$pch, 
                gp =
                gpar(col = colourPoints(obj$colby, col.args, opts),
                     cex = obj$propsize * opts$cex.pt,
                     lwd = opts$lwd.pt, alpha = opts$alpha,
                     fill = obj$fill.pt))
        
    # Connect by dots if they want it ...
    if (opts$join) {
        if (length(unique(obj$cols)) == 1 | !opts$lines.by) {
            grid.lines(obj$x, obj$y, default.units = "native",
                       gp =
                       gpar(lwd = opts$lwd, lty = opts$lty,
                            col = opts$col.line))
        } else {
            byy <- as.factor(obj$cols)  # pseudo-by-variable
            xtmp <- lapply(levels(byy), function(c) subset(obj$x, obj$cols == c))
            ytmp <- lapply(levels(byy), function(c) subset(obj$y, obj$cols == c))
            
            for (b in 1:length(levels(byy)))
                grid.lines(xtmp[[b]], ytmp[[b]], default.units = "native",
                           gp =
                           gpar(lwd = opts$lwd, lty = opts$lty,
                                col = levels(byy)[b]))
        }
    }
    
    ## add rugs --- these only make sense for a scatter plot
    if ("x" %in% strsplit(opts$rug, '')[[1]]) {
      # Add marks on the x-axis at the location of every data point
        grid.polyline(x = unit(rep(obj$x, each = 2), "native"),
                      y = unit(rep(c(0, 0.5), length(obj$x)), "char"),
                      id.lengths = rep(2, length(obj$x)))
    }
    if ("y" %in% strsplit(opts$rug, '')[[1]]) {
      # Same, but for the y-axis
        grid.polyline(y = unit(rep(obj$y, each = 2), "native"),
                      x = unit(rep(c(0, 0.5), length(obj$y)), "char"),
                      id.lengths = rep(2, length(obj$y)))
    }

    ## ---------------------------------------------------------------------------- ##
    ## Now that the main plot has been drawn, time to add stuff to it!

    # Line of Equality (LOE)
    if (opts$LOE) {
        xx <- c(min(xlim, ylim), max(xlim, ylim))
        grid.lines(xx, xx, default.units = "native",
                   gp = gpar(col = opts$col.LOE, lty = opts$lty.LOE))
    }

    if (opts$trend.by)
        if (is.null(col.args$f.cols))
            opts$trend.by <- FALSE

    # Smoothers and quantiles:
    if (length(opts$quant.smooth) > 0) {
        qs <- try(calcQSmooth(cbind(obj$x, obj$y), opts$quant.smooth, opts), TRUE)
        if (!inherits(qs, "try-error")) {
            qp <- qs$qp
            lty <- qs$lty
            lwd <- qs$lwd
            for (q in 1:length(qp))
                try(addQuantileSmoother(obj$x, obj$y, quantile = qp[q],
                                        col = opts$col.smooth,
                                        lty = lty[q], lwd = lwd[q]), TRUE)
        }
    } else if (!is.null(opts$smooth)) {
      # Smoothers
        if (opts$smooth != 0) {
            if (opts$smooth > 1) {
                warning("Smoothing value must be in the interval [0, 1]")
            } else {
                if (length(unique(obj$col)) == 1 | !opts$trend.by) {
                    try(addSmoother(obj$x, obj$y, f = opts$smooth,
                                    col = opts$col.smooth, bs = opts$bs.inference), TRUE)
                } else {
                    byy <- as.factor(obj$col)  # pseudo-by-variable
                    xtmp <- lapply(levels(byy), function(c) subset(obj$x, obj$col == c))
                    ytmp <- lapply(levels(byy), function(c) subset(obj$y, obj$col == c))
                    
                    for (b in 1:length(levels(byy)))
                        try(addSmoother(xtmp[[b]], ytmp[[b]],
                                        f = opts$smooth,
                                        col = darken(col.args$f.cols[b]),
                                        bs = FALSE, lty = opts$smoothby.lty), TRUE)
                }
            }
        }
    }

    # Trend lines:
    # ------------------------------------------------------------- #
    # If the `by` variable has been set, then the points are        
    # coloured by the levels of `by`. Thus, there is more than one
    # level of `unique(col)`. In this case, we need to add the
    # trend lines for each level of by (i.e., each colour). The
    # colours of these lines are darker versions of the points.
    # ------------------------------------------------------------- #

    
        
    if (!is.null(opts$trend)) {
        if (length(unique(obj$col)) == 1 | !opts$trend.by) {
            lapply(opts$trend, function(o) {
                order = which(c("linear", "quadratic", "cubic") == o)  # gives us 1, 2, or 3
                addTrend(obj$x, obj$y, order = order, xlim = xlim,
                         col = opts$col.trend[[o]], bs = opts$bs.inference)
            })
        } else if (opts$trend.parallel) {
            byy <- as.factor(obj$col)
            lapply(opts$trend, function(o) {
                order = which(c("linear", "quadratic", "cubic") == o)
                addParTrend(obj$x, obj$y, byy, order = order, xlim = xlim,
                            cols = col.args$f.cols)
            })
        } else {
            byy <- as.factor(obj$col)  # pseudo-by-variable
            xtmp <- lapply(levels(byy), function(c) subset(obj$x, obj$col == c))
            ytmp <- lapply(levels(byy), function(c) subset(obj$y, obj$col == c))
            
            for (b in 1:length(levels(byy)))
                lapply(opts$trend, function(o) {
                    order = which(c("linear", "quadratic", "cubic") == o)
                    addTrend(xtmp[[b]], ytmp[[b]],
                             order = order, xlim = xlim,
                             col = col.args$f.cols[b],
                             bs = opts$bs.inference)
                })
        }
    }
    
    invisible(NULL)
}
