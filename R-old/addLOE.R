addLOE <-
function(x, y, col, lty, xlim, ylim) {
    xx <- c(min(xlim, ylim), max(xlim, ylim))
    grid.lines(xx, xx, default.units = "native",
               gp = gpar(col = col, lty = lty))
}
