makePoints <-
function(x, cols = NULL, xlim = range(x, na.rm = TRUE),
         useHist = length(x) > opts$large.sample.size,
         opts = inzPlotDefaults()) {

  # ----------------------------------------------------------------------- #
  # This function should be renamed at some point, but I'll get around
  # to that some time...
  # ----------------------------------------------------------------------- #
  # If the sample size is within a certain threshold, then this function
  # will return the x and y values of points and produce a dot-plot.
  # Otherwise, it will output the original x values and the densities
  # in the bins (this is for y-axis scaling only!).
  # ----------------------------------------------------------------------- #

    wd <- convertWidth(unit(1 * opts$cex.pt, "char"),
                       "npc", valueOnly = TRUE)
    ht <- convertHeight(unit(1 * opts$cex.pt, "char"),
                        "npc", valueOnly = TRUE)
    nx <- floor(1 / (wd * 0.8))
    ny <- floor(convertHeight(unit(1, "npc"),
                              "npc", valueOnly = TRUE) / ht)
    
    h <- iNZhist(x, nbins = nx, xlim = xlim)
    
    if (useHist) {
      # ---------------------------------------------------------- #
      #                                                  HISTOGRAM
      # Do a histogram: we only care about the y-values,
      # althuogh the x-values are returned because they are also
      # used somewhere.

    #    h <- iNZhist(x, opts$hist.bins / (2 * opts$cex.pt), xlim = xlim)
        out <- list(x = x, y = h$counts, cols = cols, ymax = max(h$counts))
        
    } else if (length(x) > 0) {
      # ---------------------------------------------------------- #
      #                                                    DOTPLOT
      # Returns the X and Y values to make a dotplot
                
        y <- unlist(sapply(h$counts[h$counts != 0], function(c) 1:c))

      # Now we want to scale the y-values to the same scale as hist() gives
      #  y.h <- iNZhist(x, opts$hist.bins / (2 * opts$cex.pt),
      #                 xlim = xlim)$counts
        

      # Sort out colours D:
        o <- order(x)
        if (!is.null(cols))
            col <- cols[o]
        
        out <- list(x = h$x, y = y,
                    cols = col, ymax = max(y, ny))
    } else {
        out <- list(ymax = 1)
    }
    out
}
