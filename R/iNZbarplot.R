iNZbarplot <-
    function(x, y = NULL, axis = c(0, 0), lab = NULL, x.lev, y.lev = NULL,
             layout, xlim, ylim, col = opts$col.bar, opts) {
  # --------------------------------------------------------------------------- #
  # Makes a bar plot of the supplied X data, possibly broken down by Y
  # Can only be called from the iNZplot() function.
  # Includes the functionlity to:
  #  - draw the appropriate axes, depending on the position in the layout grid
  #  - draws the subtitle if grouping variable 1 is being defined
  #  - add any additional features
  # --------------------------------------------------------------------------- #

    pushViewport(viewport(layout = layout,
                          xscale = xlim))

  # --------------------------------------------------------------------------- #
  #                                                             Draw the x-axes
    if (axis[1] == 2) {
        grid.text(x.lev,
                  x = unit((0:length(x.lev))[-1] - 0.5, "native"),
                  y = unit(-1, "lines"),
                  gp = gpar(cex = opts$cex.axis))
    }

  # --------------------------------------------------------------------------- #
  #                                                            Add the subtitle
    if (!is.null(lab)) {
        pushViewport(viewport(layout.pos.row = 1))
        grid.rect(gp = gpar(fill = opts$col.sub))
        grid.text(lab, gp = gpar(cex = opts$cex.lab))
        upViewport()
    }

  # =========================================================================== #
  #                                                             Start main plot
    
    pushViewport(viewport(layout.pos.row = 2,
                          xscale = xlim,
                          yscale = c(0, ylim[2])))

  # --------------------------------------------------------------------------- #
  #                                                               Add the yaxis
    
    if (axis[2] == 2) {
        grid.yaxis(gp = gpar(cex = opts$cex.axis))
    } else if (axis[2] == 1) {
        grid.yaxis(main = FALSE, label = FALSE,
                   gp = gpar(cex = opts$cex.axis))
    }

  # --------------------------------------------------------------------------- #
  #                                                      set up layout for bars

    layout4 <- grid.layout(nrow = 1, ncol = length(x.lev))
    pushViewport(viewport(layout = layout4))

    if (!is.null(y)) {
      # calculate multiple bars, multiple x-points
        hgt <- makeBars(x, y)
    } else {
        hgt <- makeBars(x)
    }

    print(hgt)
    for (i in 1:length(x.lev)) {
        pushViewport(viewport(layout.pos.col = i,
                              yscale = ylim * 1.05))

        if (is.null(y)) {
          # Plotting a single bar for each level of g1
            grid.rect(x = 0.5, y = 0,
                      height = unit(hgt[i], "native"),
                      width = 0.8,
                      just = "bottom",
                      gp =
                      gpar(fill = opts$bar.fill, col = opts$bar.col,
                           lwd = opts$bar.lwd))
                      
        } else {
          # Plotting a bar for each level of y, for each level of g1
            xx <- 1 / (ncol(hgt) + 1) * (1:ncol(hgt))
            yy <- hgt[i, ]

          # sort out the colours
            cols <- opts$bar.col
            if (length(cols) < nrow(hgt)) {
                col <- hcl(1:nrow(hgt) / nrow(hgt) * 360, c = 80, l = 50)
            } else {
                col <- cols[1:nrow(hgt)]
            }

            grid.rect(x = xx, y = 0,
                      height = unit(yy, "native"),
                      width = 0.8 / (ncol(hgt) + 1),
                      just = "bottom",
                      gp =
                      gpar(fill = col, col = opts$bar.col,
                           lwd = opts$bar.lwd))
        }
        
        upViewport()
    }
    
    upViewport()  # back to layout4
    upViewport()  # back to sub plot/layout3
    upViewport()  # back to layout2
}
