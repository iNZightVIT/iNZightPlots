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

        print(args)
        
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
    gr <- obj$makeRects(obj$args, gen$xlim, gen$ylim)
    
    pushViewport(viewport(0.5, 0.5, 0.8, 0.8, "npc", xscale = gen$xlim, yscale = gen$ylim))
    grid.xaxis()
    grid.yaxis()
    grid.polygon(gr$xg, gr$yg, id = gr$id, default.units = "npc",
                 gp = gpar(fill = gr$col, col = gr$col))
}
