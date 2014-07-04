create.inz.gridplot <- function(obj) {
    ## The original "grid" style plot for large sample sizes. This will only be used for
    ## simple iid data, and instead hexagonal binning will be used for frequency and
    ## survey data plots.

    df <- obj$df
    opts <- obj$opts
    xattr <- obj$xattr

    v <- colnames(df)
    vn <- xattr$varnames

    # first need to remove missing values
    missing <- apply(df[ , v %in% c("x", "y")], 1, function(x) any(is.na(x)))
    n.missing <- sum(missing)
    df <- df[!missing, ]

    ## Because this requires the xlimits to first be calculated, we can only create the
    ## plot AFTER we have done all the plots ...

    makeRects <- function(args, xlim, ylim) {
        df <- args$df
        opts <- args$opts
        xattr <- args$xattr
        
        # Set up the grid
        # (for now, we will scale the bin size by point size)
        Npt <- min(250, floor(opts$scatter.grid.bins / (opts$cex.pt * 2)))
        
        #  scatter.grid <- matrix(0, nrow = Npt, ncol = Npt)
        xbrk <- seq(xlim[1], xlim[2], length = Npt + 1)
        ybrk <- seq(ylim[1], ylim[2], length = Npt + 1)
        xx <- cut(df$x, xbrk)
        yy <- cut(df$y, ybrk)
        
        scatter.grid <- as.matrix(table(yy, xx))
        scatter.grid <- scatter.grid[Npt:1, ]
    
        # If a point has very high density, it dominates. So instead, shade by quantiles.
        nquants <- max(3, length(df$x) / 100)
        br <- unique(quantile(c(scatter.grid), seq(0, 1, length = nquants)))
        which.quant <- as.numeric(cut(c(scatter.grid), breaks = c(-1, br)))
        
        hcols <- hcl(0, 0, seq(100 * (1 - opts$alpha / 2), 0,
                               length = length(br)))
        shade <- matrix(hcols[which.quant], nrow = nrow(scatter.grid))
        
        is0 <- c(scatter.grid) == 0
        
        # centers of all grid boxes
        xv = (rep(1:Npt, each = Npt) - 0.5) / Npt
        yv = (rep(Npt:1, Npt) - 0.5) / Npt
        
        # We will attempt to use grid.polygon() to draw all of the
        # grid squares at the same time!
        wx <- 0.5 / Npt
        wy <- 0.5 / Npt
        xmat <- sapply(xv, function(x) x + c(-1, -1, 1, 1) * wx)
        ymat <- sapply(yv, function(y) y + c(-1, 1, 1, -1) * wy)
        id <- matrix(rep((1:Npt^2), each = 4), nrow = 4)

        # Remove zero-count cells:
        xmat <- xmat[, !is0]
        ymat <- ymat[, !is0]
        id <- id[, !is0]
        shade <- shade[!is0]

        list(xg = xmat, yg = ymat, id = id, col = shade)
    }

    out <- list(makeRects = makeRects, args = list(df = df, opts = opts, xattr = xattr),
                n.missing = n.missing,
                xlim = if (nrow(df) > 0) range(df$x, na.rm = TRUE) else c(-Inf, Inf),
                ylim = if (nrow(df) > 0) range(df$y, na.rm = TRUE) else c(-Inf, Inf))
    class(out) <- "inzgrid"

    out
}

plot.inzgrid <- function(obj, gen) {
    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    opts <- gen$opts
    mcex <- gen$mcex
    
    gr <- obj$makeRects(obj$args, xlim, ylim)
    
    grid.polygon(gr$xg, gr$yg, id = gr$id, default.units = "npc",
                 gp = gpar(fill = gr$col, col = gr$col))

    ## ---------------------------------------------------------------------------- ##
    ## Now that the main plot has been drawn, time to add stuff to it!

    # Line of Equality (LOE)
    if (opts$LOE) {
        xx <- c(min(xlim, ylim), max(xlim, ylim))
        grid.lines(xx, xx, default.units = "native",
                   gp = gpar(col = opts$col.LOE, lty = opts$lty.LOE))
    }

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
                                        col = darken(levels(byy)[b]),
                                        bs = FALSE, lty = 2), TRUE)
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
        lapply(opts$trend, function(o) {
            order = which(c("linear", "quadratic", "cubic") == o)  # gives us 1, 2, or 3
            addTrend(obj$x, obj$y, order = order, xlim = xlim,
                     col = opts$col.trend[[o]], bs = opts$bs.inference)
        })

    }
    
    invisible(NULL)
}
