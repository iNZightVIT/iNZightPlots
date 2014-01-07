iNZscatterplot <-
function(x, y, axis = c(0, 0, 0, 0), lab = NULL,
                           layout, xlim = range(x), ylim = range(y),
                           col = opts$col.pt, opts) {
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
    pushViewport(viewport(clip = "on",
                          xscale = xlim,
                          yscale = ylim))  # so nothing goes outside the box

    grid.points(x, y, pch = opts$pch,
                gp =
                gpar(cex = opts$cex.pt, col = col,
                     lwd = opts$lwd.pt))

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

  # Trend lines
    if (!is.null(opts$trend))
        lapply(opts$trend, function(o) {
            order = which(c("linear", "quadratic", "cubic") == o)
            addTrend(x, y, order = order, xlim = xlim,
                     col = opts$col.trend[[o]], bs = opts$bs.inference)
        })

    upViewport()  # end clipping
    upViewport()  # end main plot
    upViewport()  # end this subplot
}
