iNZdotplot <-
    function(x, y = NULL, axis = c(0, 0), lab = NULL,
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

    ylim2 <- c(0, ifelse(is.null(y), 1, length(levels(y))))  # need a yscale
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

        dat <- makePoints(x, cols = col, xlim = xlim, opts = opts)
        x <- dat$x
        y <- dat$y
        col <- dat$col
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

      # Adding guidelines to comparison and confidence intervals
        guides <- if (is.null(opts$inference.type)) NULL else numeric(0)

        for (k in 1:n.y) {
            CEX <- sqrt(sqrt(n.y) / n.y)
            pushViewport(viewport(layout.pos.row = k, gp = gpar(cex = CEX)))
 
            if (length(x.list[[k]]) == 0) {
              # This is the case when there are no observations in the
              # group.
                upViewport()
                next
            }
            
            dat <- makePoints(x.list[[k]], cols = col.list[[k]], xlim = xlim,
                              opts = opts)
            guides <- drawDotplot(dat$x, dat$y, xlim = xlim, ylim = ylim,
                                  col = dat$col, opts = opts, guides = guides)
            upViewport()  # back to layout4
        }

      # ------------------------------------------------------------------ #
        
        upViewport()  # out of layout4

      # Draw inference guide lines
        if (length(guides) > 0)
            grid.polyline(x = unit(rep(guides, each = 2), "native"),
                          y = rep(c(0, 1), length(guides)),
                          id = rep(1:length(guides), each = 2),
                          gp = gpar(lty = 3, col = "grey50", lwd = 0.5))
        
    }

    upViewport()
    upViewport()
}
