addSmoother <-
function(x, y, f, col, bs, lty = 1) {
    sm <- lowess(x, y, f = f)
    grid.lines(sm$x, sm$y,
               default.units = "native",
               gp = gpar(col = col, lwd = 2, lty = lty))

    if (bs) {
        for (i in 1:30) {
          # User wants bootstrap inference for the smoother:
            id <- sample(1:length(x), replace = TRUE)
            x2 <- x[id]
            y2 <- y[id]
            sm <- lowess(x2, y2, f = f)
            grid.lines(sm$x, sm$y,
                       default.units = "native",
                       gp = gpar(col = col, lwd = 1, lty = 3))
        }
    }
}

addQuantileSmoother <-
function(x, y, quantile, col, lty, lwd) {
  # Draws quantiles on a plot.
    if (quantile < 0.5)  # symmetry
        quantile <- c(quantile, 1 - quantile)

  # Because we are using the `svysmooth()` function from the `survey` package,
  # we need to supply a design (here, everything is IID)
    des <- suppressWarnings(svydesign(ids = ~1, data = data.frame(x = x, y = y)))
    
    invisible(sapply(quantile,
                     function(a) {
                         s <- svysmooth(y ~ x, design = des,
                                        method = "quantreg", quantile = a)$x
                         grid.lines(s$x, s$y, default.units = "native",
                                    gp = gpar(col = col, lty = lty, lwd = lwd))
                     }))
}
