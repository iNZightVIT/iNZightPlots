iNZscatterplot <-
    function(x, y, axis = c(0, 0, 0, 0), lab = NULL,
             layout, xlim = range(x), ylim = range(y),
             col = opts$col.pt, prop.size = NULL, opts) {
  # --------------------------------------------------------------------------- #
  # Makes a scatter plot of the supplied X and Y data.
  # Can only be called from the iNZplot() function.
  # Includes the functionlity to:
  #  - draw the appropriate axes, depending on the position in the layout grid
  #  - draws the subtitle if grouping variable 1 is being defined
  #  - add any additional features: eg, smoother, trendline, jitter ...
  # --------------------------------------------------------------------------- #

    pushViewport(viewport(layout = layout,
                          xscale = xlim))

  # --------------------------------------------------------------------------- #
  #                                                             Draw the x-axes
  # these need to be ABOVE the labels
    agp <- gpar(cex = opts$cex.axis)
    if (axis[1] == 1)
        grid.xaxis(label = FALSE, gp = agp)
    if (axis[1] == 2)
        grid.xaxis(gp = agp)
    if (axis[3] == 1)
        grid.xaxis(main = FALSE, label = FALSE, gp = agp)
    if (axis[3] == 2)
        grid.xaxis(main = FALSE, gp = agp)

  # --------------------------------------------------------------------------- #
  #                                                            Add the subtitle
    if (!is.null(lab)) {
        pushViewport(viewport(layout.pos.row = 1))
        grid.rect(gp = gpar(fill = opts$col.sub))
        grid.text(lab, gp = gpar(cex = opts$cex.lab))
        upViewport()
    }

  # =========================================================================== #
  # Start main plot
    pushViewport(viewport(layout.pos.row = 2,
                          xscale = xlim, yscale = ylim))

  # --------------------------------------------------------------------------- #
  #                                                                  Add y-axes
    if (axis[2] == 1)
        grid.yaxis(label = FALSE, gp = agp)
    if (axis[2] == 2)
        grid.yaxis(gp = agp)
    if (axis[4] == 1)
        grid.yaxis(main = FALSE, label = FALSE, gp = agp)
    if (axis[4] == 2)
        grid.yaxis(main = FALSE, gp = agp)

  # --------------------------------------------------------------------------- #
  #                                                        Draw the scatterplot

  # Check that there are some points to plot:
    if (length(x) > 0) {
        pushViewport(viewport(clip = "on",
                              xscale = xlim,
                              yscale = ylim))  # so nothing goes outside the box

      # Point sizes:
        if (!is.null(prop.size))
            cex <- proportionalPointSize(prop.size, opts$cex.pt)
        else
            cex <- opts$cex.pt

        grid.points(x, y, pch = opts$pch,
                    gp =
                    gpar(cex = cex, col = col,
                         lwd = opts$lwd.pt,
                         alpha = opts$alpha))

     # Connect by dots if they want it ...
        if (opts$join)
            grid.lines(x, y, default.units = "native",
                       gp =
                       gpar(lwd = opts$lwd, lty = opts$lty,
                            col = opts$col.line))

  # --------------------------------------------------------------------------- #
  #                                          Add any addional plotting features

      # Rugs
        if ("x" %in% strsplit(opts$rug, '')[[1]]) {
          # Add marks on the x-axis at the location of every data point
            grid.polyline(x = unit(rep(x, each = 2), "native"),
                          y = rep(c(0, 0.02), length(x)),
                          id.lengths = rep(2, length(x)))
        }
        if ("y" %in% strsplit(opts$rug, '')[[1]]) {
          # Same, but for the y-axis
            grid.polyline(y = unit(rep(y, each = 2), "native"),
                          x = rep(c(0, 0.02), length(y)),
                          id.lengths = rep(2, length(y)))
        }
    
      # Line of Equality (LOE)
        if (opts$LOE) {
            addLOE(x, y, col = opts$col.LOE, lty = opts$lty.LOE,
                   xlim = xlim, ylim = ylim)
        }

      # Smoothers
        if (!is.null(opts$smooth))
            if (opts$smooth != 0)
                if (opts$smooth > 1)
                  # !!! Move this error checking to the beginning of the plot
                  # function (otherwise it produces it g1.levels times!
                    warning("Smoothing value must be in the interval [0, 1]")
                else
                    addSmoother(x, y, f = opts$smooth,
                                col = opts$col.smooth, bs = opts$bs.inference)

      # Trend lines:
      # ------------------------------------------------------------- #
      # If the `by` variable has been set, then the points are        
      # coloured by the levels of `by`. Thus, there is more than one
      # level of `unique(col)`. In this case, we need to add the
      # trend lines for each level of by (i.e., each colour). The
      # colours of these lines are darker versions of the points.
      # ------------------------------------------------------------- #
        
        if (!is.null(opts$trend)) {
            if (length(unique(col)) == 1 | !opts$trend.by) {
                lapply(opts$trend, function(o) {
                    order = which(c("linear", "quadratic", "cubic") == o)  # gives us 1, 2, or 3
                    addTrend(x, y, order = order, xlim = xlim,
                             col = opts$col.trend[[o]], bs = opts$bs.inference)
                })
            } else {
                byy <- as.factor(col)  # pseudo-by-variable
                xtmp <- lapply(levels(byy), function(c) subset(x, col == c))
                ytmp <- lapply(levels(byy), function(c) subset(y, col == c))

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

        upViewport()  # end clipping
    }
    
    upViewport()  # end main plot
    upViewport()  # end this subplot
}
