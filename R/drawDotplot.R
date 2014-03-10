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
                          yscale = ylim,
                          name = "DOTPLOTVP"))

    largesample <-
        if (is.null(opts$largesample))
            length(x) > opts$large.sample.size
        else
            opts$largesample

    if (largesample) {
        wd <- convertWidth(unit(1 * opts$cex.pt, "char"),
                           "npc", valueOnly = TRUE)
        ht <- convertHeight(unit(1 * opts$cex.pt, "char"),
                            "npc", valueOnly = TRUE)
        nx <- floor(1 / (wd * 0.8))
        ny <- floor(convertHeight(unit(1, "npc"),
                                  "npc", valueOnly = TRUE) / ht)

        h <- iNZhist(x, nbins = nx, xlim = xlim)
        
      # Draw a histogram if sample size is large:
      #  h <- iNZhist(x, opts$hist.bins / (2 * opts$cex.pt), xlim = xlim)

        xx <- h$breaks
        yy <- h$counts

      # Need to make a vector of points for corners of polygons
        wx <- diff(xx)[1] / 2
        x.mid <- xx[-length(xx)] + wx
        xmat <- sapply(x.mid, function(x) x + c(-1, -1, 1, 1) * wx)
        ymat <- sapply(yy, function(y) y * c(0, 1, 1, 0))
        matid <- matrix(rep(1:ncol(xmat), each = 4), nrow = 4)

        grid.polygon(xmat, ymat, id = matid, default.units = "native",
                     gp = gpar(fill = opts$col.pt))
    } else if (length(x) > 0) {
      # Draw the dotplot in the first row
        grid.points(x, y, default.units = "native",
                    pch = if (opts$alpha == 1) opts$pch else 19,
                    gp =
                    gpar(cex = opts$cex.pt, col = col,
                         lwd = opts$lwd.pt, fill = opts$fill.pt,
                         alpha = opts$alpha))
    }

    upViewport()  # back to layout5
    
    upViewport()  # out of layout5

    guides
}
