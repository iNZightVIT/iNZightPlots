plot.inzscatter <- function(obj, opts = inzpar(), axis = c(2, 2, 1, 1),
                            title = NULL, mcex, sub = 0) {

    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale

    if (axis[1] > 0)
        grid.xaxis(gp = gpar(cex = opts$cex.axis), label = (axis[1] == 2))
    if (axis[2] > 0) {
        yax <- yaxisGrob(gp = gpar(cex = opts$cex.axis), label = (axis[2] == 2))
        if (axis[2] == 2) 
            yax <- editGrob(yax, edits = gEdit("labels", rot = 90, hjust = 0.5, vjust = 0))
        grid.draw(yax)
    }
    if (axis[3] > 0) {
        pushViewport(viewport(x = 0.5, y = 1, height = unit(sub, "in"), just = "bottom",
                              xscale = xlim))
        grid.xaxis(gp = gpar(cex = opts$cex.axis), label = (axis[3] == 2), main = FALSE)
        upViewport()
    }
    if (axis[4] > 0) {
        yax <- yaxisGrob(gp = gpar(cex = opts$cex.axis), label = (axis[4] == 2), main = FALSE)
        if (axis[4] == 2)
            yax <- editGrob(yax, edits = gEdit("labels", rot = 270, hjust = 0.5, vjust = -0.5))
        grid.draw(yax)
    }

    ## calculate the height of the subtitle if it is specified
    hgt <- unit.c(
        if (!is.null(title)) {
            subt <- textGrob(title, gp = gpar(cex = opts$cex.lab, fontface = "bold"))
            unit(convertHeight(grobHeight(subt), "in", TRUE) * 2, "in")
        } else {
            unit(0, "null")
        },
        unit(1, "null"))
    pushViewport(viewport(layout = grid.layout(2, 1, heights = hgt)))

    if (!is.null(title)) {
        pushViewport(viewport(layout.pos.row = 1))
        grid.rect(gp = gpar(fill = opts$col.sub))
        grid.draw(subt)
        upViewport()
    }

    pushViewport(viewport(layout.pos.row = 2, xscale = xlim, yscale = ylim, clip = "on"))
    grid.points(obj$x, obj$y, pch = obj$pch, 
                gp =
                gpar(col = obj$cols, cex = obj$propsize * opts$cex.pt,
                     lwd = opts$lwd.pt, alpha = opts$alpha))
    

    ## ---------------------------------------------------------------------------- ##
    ## Now that the main plot has been drawn, time to add stuff to it!

    ## add rugs
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

    # Line of Equality (LOE)
    if (opts$LOE) {
        xx <- c(min(xlim, ylim), max(xlim, ylim))
        grid.lines(xx, xx, default.units = "native",
                   gp = gpar(col = opts$col.LOE, lty = opts$lty.LOE))
    }

    # Smoothers and quantiles:
    if (length(opts$quant.smooth) > 0) {
        qs <- calcQSmooth(obj$x, opts$quant.smooth, opts)
        if (!is.null(qs)) {
            qp <- qs$qp
            lty <- qs$lty
            lwd <- qs$lwd
            for (q in 1:length(qp))
                addQuantileSmoother(obj$x, obj$y, quantile = qp[q],
                                    col = opts$col.smooth,
                                    lty = lty[q], lwd = lwd[q])
        }
    } else if (!is.null(opts$smooth)) {
      # Smoothers
        if (opts$smooth != 0) {
            if (opts$smooth > 1) {
                warning("Smoothing value must be in the interval [0, 1]")
            } else {
                if (length(unique(obj$col)) == 1 | !opts$trend.by) {
                    addSmoother(obj$x, obj$y, f = opts$smooth,
                                col = opts$col.smooth, bs = opts$bs.inference)
                } else {
                    byy <- as.factor(obj$col)  # pseudo-by-variable
                    xtmp <- lapply(levels(byy), function(c) subset(obj$x, obj$col == c))
                    ytmp <- lapply(levels(byy), function(c) subset(obj$y, obj$col == c))
                    
                    for (b in 1:length(levels(byy)))
                        addSmoother(xtmp[[b]], ytmp[[b]],
                                    f = opts$smooth,
                                    col = darken(levels(byy)[b]),
                                    bs = FALSE, lty = 2)
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
        } else {
            byy <- as.factor(obj$col)  # pseudo-by-variable
            xtmp <- lapply(levels(byy), function(c) subset(obj$x, obj$col == c))
            ytmp <- lapply(levels(byy), function(c) subset(obj$y, obj$col == c))
            
            for (b in 1:length(levels(byy)))
                lapply(opts$trend, function(o) {
                    order = which(c("linear", "quadratic", "cubic") == o)
                    addTrend(xtmp[[b]], ytmp[[b]],
                             order = order, xlim = xlim,
                             col = darken(levels(byy)[b]),
                             bs = FALSE)  # opts$bs.inference)
                })
        }
    }
    
    upViewport()

    invisible(NULL)
}

