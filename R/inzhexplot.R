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

    xbins <- opts$hex.bins
    
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
                xlim = if (nrow(df) > 0) hb@xbnds else c(-Inf, Inf),
                ylim = if (nrow(df) > 0) hb@ybnds else c(-Inf, Inf),
                x = df$x, y = df$y, n.bins = xbins,
                trend = opts$trend, trend.by = opts$trend.by, smooth = opts$trend,
                n.boot = opts$n.boot)
    class(out) <- "inzhex"

    out
}

plot.inzhex <- function(obj, gen) {
    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    opts <- gen$opts
    mcex <- gen$mcex
    col.args <- gen$col.args

    ## adding grid lines?
    if (opts$grid.lines) {
        at.x <- pretty(gen$LIM[1:2])
        at.y <- pretty(gen$LIM[3:4])
        at.X <- c(rep(at.x, each = 2), rep(current.viewport()$xscale, length(at.y)))
        at.Y <- c(rep(current.viewport()$yscale, length(at.x)), rep(at.y, each = 2))
        col.grid <- opts$col.grid
        if (sum(col2rgb(opts$bg) / 255) > 0.95 * 3) {
            col.grid <- "#cccccc"
        } else {
            
        }
        grid.polyline(at.X, at.Y, id.lengths = rep(2, length(at.X)/2),
                      default.units = "native",
                      gp = gpar(col = col.grid, lwd = 1))
    }

    if (!is.null(obj$colby)) {
        print(convertWidth(current.viewport()$width, "in", TRUE))
        print(convertHeight(current.viewport()$height, "in", TRUE))
        panel.hextri(x = obj$x, y = obj$y, groups = levels(obj$colby),
                     subscripts = as.numeric(obj$colby), colours = col.args$f.cols,
                     nbins = obj$n.bins, style = opts$hex.style, diffuse = opts$hex.diffuse,
                     shape = convertHeight(current.viewport()$height, "in", TRUE) /
                         convertWidth(current.viewport()$width, "in", TRUE))
    } else {
        grid.hexagons(obj$hex, style = "centroids", maxcnt = gen$maxcount, border = opts$col.pt,
                      pen = opts$col.pt[1])
    }
    
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
