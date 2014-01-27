drawDotplot <-
function(x, y, xlim, ylim, col, opts, guides = NULL) {
  # Draws a dotplot in a selected space. Also draws the boxplot if it
  # is requested.

    infmean <- if (is.null(opts$inference.par)) FALSE else opts$inference.par == "mean"
    makebox <- opts$box & length(x) > 5 & !infmean
  # If user wants median inference, then also leave space
    med.inf <- mean.inf <- FALSE
    if (!is.null(opts$inference.type) & length(x) >= 10) {
        if (is.null(opts$inference.par)) {
            med.inf <- TRUE
            opts$inference.par <- "median"
        } else if (opts$inference.par == "median")
            med.inf <- TRUE
        else if (opts$inference.par == "mean")
            mean.inf <- TRUE
        else {
            warning("Invalid inference parameter. Using median (default).")
            opts$inference.par <- "median"
            med.inf <- TRUE
        }
    }
    ex.space <- makebox | med.inf | mean.inf | opts$box
        
  # First step: set up the layout
  # NOTE: if opts$box is true we will always leave space for the box
  #       but only plot it if there are more than 5 points
    if (ex.space) {
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

  # Draw a box if asked for (or median inference)
    if (ex.space) {
      # Draw mean inference if asked for
        pushViewport(viewport(layout.pos.row = 2,
                              xscale = xlim))
        if (mean.inf) {
            guides <- drawMeanInference(x, opts, guides)
        } else {
            if (makebox) drawBoxPlot(x, opts)
            if (med.inf) guides <- drawMedianInference(x, opts, guides)
        }
        
        upViewport()
    }

    pushViewport(viewport(layout.pos.row = 1,
                          xscale = xlim,
                          yscale = ylim))

    if (length(x) > opts$large.sample.size) {
      # Draw a histogram if sample size is large:
        h <- hist(x, opts$hist.bins, plot = FALSE)

        xx <- h$breaks
        yy <- h$density
        for (b in 1:length(yy)) {
            grid.rect(xx[b], 0,
                      width = xx[b + 1] - xx[b],
                      height = yy[b],
                      default.units = "native",
                      just = c("left", "bottom"),
                      gp = gpar(fill = col))
        }
    } else {
      # Draw the dotplot in the first row
        grid.points(x, y, default.units = "native",
                    pch = opts$pch,
                    gp =
                    gpar(cex = opts$cex.pt, col = col,
                         lwd = opts$lwd.pt, fill = opts$fill.pt))
    }

    upViewport()  # back to layout5
    
    upViewport()  # out of layout5

    guides
}
