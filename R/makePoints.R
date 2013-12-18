makePoints <-
function(x, cols = NULL) {
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
        temp    <- stacking(x, xbin, cols)
        v.space <- v.max / max(temp$du)
        cramp   <- v.add / v.space
        
        y <- min(v.space, v.add) * (temp$du - 1)
        out <- list(x = temp$x, y = y, cols = temp$col)
    } else {
        out <- NULL
    }
    out
}
