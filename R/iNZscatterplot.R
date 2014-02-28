iNZscatterplot <-
    function(x, y, axis = c(0, 0, 0, 0), ax.add = 0, lab = NULL,
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

    pushViewport(viewport(0.5, 1, height = ax.add, width = 1,
                          default.units = "npc", just = "bottom",
                          xscale = xlim))
    if (axis[3] == 1)
        grid.xaxis(main = FALSE, label = FALSE, gp = agp)
    if (axis[3] == 2)
        grid.xaxis(main = FALSE, gp = agp)
    upViewport()

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

      # Draw a scatter plot:
        largesample <-
            if (is.null(opts$largesample))
                length(x) > opts$large.sample.size
            else
                opts$largesample
        
        if (!largesample) {
          # Point sizes:
            if (!is.null(prop.size))
                cex <- proportionalPointSize(prop.size, opts$cex.pt)
            else
                cex <- opts$cex.pt
            
            grid.points(x, y, pch = if (opts$alpha == 1) opts$pch else 19,
                        gp =
                        gpar(cex = cex, col = col,
                             lwd = opts$lwd.pt,
                             alpha = opts$alpha))
            
          # Connect by dots if they want it ...
            if (opts$join) {
                if (length(unique(col)) == 1 | !opts$lines.by) {
                    grid.lines(x, y, default.units = "native",
                               gp =
                               gpar(lwd = opts$lwd, lty = opts$lty,
                                    col = opts$col.line))
                } else {
                    byy <- as.factor(col)  # pseudo-by-variable
                    xtmp <- lapply(levels(byy), function(c) subset(x, col == c))
                    ytmp <- lapply(levels(byy), function(c) subset(y, col == c))
                    
                    for (b in 1:length(levels(byy)))
                        grid.lines(xtmp[[b]], ytmp[[b]], default.units = "native",
                                   gp =
                                   gpar(lwd = opts$lwd, lty = opts$lty,
                                        col = levels(byy)[b]))
                }
            }
        } else {
          # draw grid plot
            
          # Set up the grid
          # (for now, we will scale the bin size by point size)
            Npt <- min(250, floor(opts$scatter.grid.bins / (opts$cex.pt * 2)))
            
            scatter.grid <- matrix(0, nrow = Npt, ncol = Npt)
            xx <- cut(x, Npt)
            yy <- cut(y, Npt)
            scatter.grid <- as.matrix(table(yy, xx))[Npt:1, ]

          # Different possible colouration patterns for the grid plot.
            # hcols <- rev(heat.colors(n = max(scatter.grid) + 1))
            # hcols <- rainbow(n = max(scatter.grid) + 1)[c(scatter.grid) + 1]
            hcols <- hcl(0, 0, seq(100 * (1 - opts$alpha / 2), 0,
                                   length = max(scatter.grid) + 1))
            shade <- matrix(hcols[scatter.grid + 1], nrow = nrow(scatter.grid))

            grid.xaxis()
            grid.yaxis()
            
            is0 <- c(scatter.grid) == 0

          # centers of all grid boxes
            xv = (rep(1:Npt, each = Npt) - 0.5) / Npt
            yv = (rep(Npt:1, Npt) - 0.5) / Npt

        # -------------------------------------------------------------- #
        #                                                       OLD CODE
        # -------------------------------------------------------------- #
        #                
        # # grid.rect(gp = gpar(fill = hcols[1]))
        # grid.points(unit(xv[!is0], "npc"), unit(yv[!is0], "npc"),
        #             size = unit(1 / Npt, "npc") * 1.35, pch = 15,
        #             gp = gpar(col = shade[!is0]))
        # ============================================================== #

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

            grid.polygon(xmat, ymat, id = id, default.units = "npc",
                         gp = gpar(fill = shade, col = shade))
        
        }
        
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

      # Smoothers + Quantile curves
        if (length(opts$quant.smooth) > 0) {
          # check quantiles are correct:
            if (opts$quant.smooth[1] == "default") {
                qp <- 0.5
                if (length(x) > opts$quant.cutoff[1]) qp <- c(qp, 0.25)
                if (length(x) > opts$quant.cutoff[2]) qp <- c(qp, 0.1)
            } else {
                qp <- opts$quant.smooth
            }
            
            if (any(qp < 1 & qp > 0)) {
                qp <- qp[qp > 0 & qp < 1]  # remove invalid quantiles

                qp[qp > 0.5] <- qp[qp > 0.5] - 0.5  # symmetry!
              # incase user gives c(0.25, 0.75), remove duplicates
                qp <- sort(unique(qp), decreasing = TRUE)

              # Sort out the line type and width:
                nn <- length(qp)
              # bb: the base number of repeats for each unit
                bb <- rep(nn %/% 3, 3)
                be <- nn %% 3  # which units repeated once more
                q.reps <- bb
                if (be != 0) q.reps[1:be] <- q.reps[1:be] + 1
                lty <- rep(1:3, q.reps)

              # Line width (less complicated! ...)
                lwd <- rep(1, length(qp))
                lwd[1] <- 2
                if (length(x) > opts$large.sample.size)
                    lwd <- lwd + 1
                
                for (q in 1:length(qp))
                    addQuantileSmoother(x, y, quantile = qp[q],
                                        col = opts$col.smooth,
                                        lty = lty[q], lwd = lwd[q])
            }  # else we can't draw anything ...
        } else if (!is.null(opts$smooth)) {
          # Smoothers
            if (opts$smooth != 0) {
                if (opts$smooth > 1) {
                    warning("Smoothing value must be in the interval [0, 1]")
                } else {
                    if (length(unique(col)) == 1 | !opts$trend.by) {
                        addSmoother(x, y, f = opts$smooth,
                                    col = opts$col.smooth, bs = opts$bs.inference)
                    } else {
                        byy <- as.factor(col)  # pseudo-by-variable
                        xtmp <- lapply(levels(byy), function(c) subset(x, col == c))
                        ytmp <- lapply(levels(byy), function(c) subset(y, col == c))
                        
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
    
