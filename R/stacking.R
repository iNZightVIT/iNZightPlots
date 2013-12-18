stacking <-
function(x, bins, cols = NULL) {
  # Takes a univariate, continuous variable and a binning
  # factor, and produces a list.
    
    oo <- order(x)
    x <- x[oo]
    bins <- bins[oo]
    bins <- factor(bins)
    xg <- split(x, bins)
    xo <- lapply(xg, seq_along)
    x <- unlist(xg, use.names = FALSE)
    du <- unlist(xo, use.names = FALSE)
    
    cols <- if (is.null(cols)) 1 else cols[oo]
    
    list(x = x, du = du, col = cols)
}
