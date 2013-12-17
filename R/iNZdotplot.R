iNZdotplot <- function(x, y = NULL, axis = c(0, 0), lab = NULL,
                       layout, xlim = range(x), ylim = NULL,
                       col = opts$col.pt, opts) {
  # --------------------------------------------------------------------------- #
  # Makes a dot plot of the supplied X data, grouped by Y if provided.
  # Can only be called from the iNZplot() function.
  # Includes the functionlity to:
  #  - draw the appropriate axes, depending on the position in the layout grid
  #  - draws the subtitle if grouping variable 1 is being defined
  #  - add any additional features (e.g., boxplot...)
  # --------------------------------------------------------------------------- #

    pushViewport(viewport(layout = layout,
                          xscale = xlim))

  # --------------------------------------------------------------------------- #
  #                                                             Draw the x-axes
  # these need to be ABOVE the labels
    agp <- gpar(cex = opts$cex.axis)
    if (axis[1] == 2)
        grid.xaxis(gp = agp)
    
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
    
    ylim2 <- c(0, length(levels(y)))  # need a yscale
    pushViewport(viewport(layout.pos.row = 2,
                          xscale = xlim,
                          yscale = ylim2))

  # --------------------------------------------------------------------------- #
  #                                                                  Add y-axes

  # This will be a little different and will print text in the correct
  # locations, according to the y-grouping variable.

    if (!is.null(y)) {
      # Need to set up yaxes
        if (axis[2] == 2) {
            text <- levels(y)
            
            grid.text(text,
                      x = unit(-0.1, "lines"),
                      y = unit(length(text):1 - 0.5, "native"),
                      just = "right", rot = 0,
                      gp = gpar(cex = opts$cex.axis))
        }
    }

  # --------------------------------------------------------------------------- #
  #                                                            Draw the dotplot

  # ========================================================================= #
  # DOTPLOT
  # -------
  # If Y is NULL, then just plot a single dotplot
  # If Y is !NULL, then divide the region into n.y rows, and do a dotplot for
  # each level of Y.
  # If BOX is TRUE, then draw a boxplot underneath each dotplot.
  # ========================================================================= #

    if (is.null(y)) {
      # Calculate x and y values for plotting the stacked dots
        dat <- makePoints(x)
        x <- dat$x
        y <- dat$y
        if (is.null(ylim))
            ylim <- c(0, max(y)) * 1.04

      # Draw the plot
        drawDotplot(x, y, xlim = xlim, ylim = ylim,
                    col = col, opts = opts)
    } else {
      # Subset some new lists for each y
        n.y <- length(levels(y))
        x.list <- lapply(levels(y),
                         function(l) subset(x, y == l))
        names(x.list) <- levels(y)
        col.list <- lapply(levels(y),
                           function(l) subset(col, y == l))
        names(col.list) <- levels(y)

      # Create a new layout for the y grouping
        layout4 <- grid.layout(n.y, 1)
        pushViewport(viewport(layout = layout4))

      # ------------------------------------------------------------------ #
      # Inside layout4

        for (k in 1:n.y) {
            pushViewport(viewport(layout.pos.row = k))
 
            if (length(x.list[[k]]) == 0) {
              # This is the case when there are no observations in the
              # group.
                upViewport()
                next
            }

            dat <- makePoints(x.list[[k]])
            drawDotplot(dat$x, dat$y, xlim = xlim, ylim = ylim,
                        col = col.list[[k]], opts = opts)
            upViewport()  # back to layout4
        }

      # ------------------------------------------------------------------ #
        
        upViewport()  # out of layout4
    }

    upViewport()
    upViewport()
    
}

makePoints <- function(x) {
  # Returns the X and Y values to make a dotplot
    if (length(x) > 0) {
        prettyrange <- range(pretty(x))
        maxBins = 75
        if (length(unique(x)) == 1) {
            xbins = x
        } else if (min(diff(sort(unique(x)))) >=
                   diff(prettyrange) / (maxBins - 1)) {
            xbins = x
        } else {
            xbins <- round(maxBins * (x - prettyrange[1]) /
                           diff(prettyrange))
        }
        
      # Calculate positioning of points
        v.max   <- 0.5
        v.add   <- 0.01
        xbin    <- xbins
        temp    <- stacking(x, xbin)
        v.space <- v.max / max(temp$du)
        cramp   <- v.add / v.space
        
        y <- min(v.space, v.add) * (temp$du - 1)
        out <- list(x = temp$x, y = y)
    } else {
        out <- NULL
    }
    out
}

drawDotplot <- function(x, y, xlim, ylim, col, opts) {
  # Draws a dotplot in a selected space. Also draws the boxplot if it
  # is requested.

  # First step: set up the layout
    if (opts$box) {
        h1 <- unit(3, "null")
        h2 <- unit(1, "null")
    } else {
        h1 <- unit(1, "null")
        h2 <- unit(0, "null")
    }

    layout5 <- grid.layout(2, 1,
                           heights = unit.c(h1, h2),
                           widths = unit(1, "null"))
    pushViewport(viewport(layout = layout5))

  # Draw a box if asked for
    if (opts$box) {
        pushViewport(viewport(layout.pos.row = 2,
                              xscale = xlim))
        drawBoxPlot(x, opts)
        upViewport()
    }

  # Draw the dotplot in the first row
    pushViewport(viewport(layout.pos.row = 1,
                          xscale = xlim,
                          yscale = ylim))
    grid.points(x, y, default.units = "native",
                gp =
                gpar(cex = opts$cex.pt, col = col, pch = opts$pch,
                     lwd = opts$lwd.pt, fill = opts$fill.pt))
    upViewport()  # back to layout5

    upViewport()  # out of layout5
}

