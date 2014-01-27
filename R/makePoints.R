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
    
    
    if (useHist) {
      # ---------------------------------------------------------- #
      #                                                  HISTOGRAM
      # Do a histogram: we only care about the y-values,
      # althuogh the x-values are returned because they are also
      # used somewhere.
        
        h <- iNZhist(x, opts$hist.bins, xlim = xlim)
        out <- list(x = x, y = h$density, cols = cols)
        
    } else if (length(x) > 0) {
      # ---------------------------------------------------------- #
      #                                                    DOTPLOT
      # Returns the X and Y values to make a dotplot
        
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
        temp    <- stacking(x, xbin, cols)
        v.space <- v.max / max(temp$du)
        cramp   <- v.add / v.space
        
        y <- min(v.space, v.add) * (temp$du - 1)

      # Now we want to scale the y-values to the same scale as hist() gives
        y.h <- iNZhist(x, opts$hist.bins, xlim = xlim)$density
        print(y.h)
        factor <- max(y.h) / max(y)
        
        out <- list(x = temp$x, y = y * factor, cols = temp$col)
    } else {
        out <- NULL
    }
    out
}
