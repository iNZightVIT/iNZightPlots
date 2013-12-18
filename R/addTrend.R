addTrend <-
function(x, y, order, xlim, col, bs) {
    xx <- seq(xlim[1], xlim[2], length = 1001)
    yy <- predict(lm(y ~ poly(x, order)), data.frame(x = xx))
    grid.lines(xx, yy,
               default.units = "native",
               gp = gpar(col = col, lwd = 2))

    if (bs) {
        for (i in 1:30) {
          # User wants bootstrap inference for this line.
            id <- sample(1:length(x), replace = TRUE)
            x2 <- x[id]
            y2 <- y[id]
            yy <- predict(lm(y2 ~ poly(x2, order)), data.frame(x2 = xx))
            grid.lines(xx, yy,
                       default.units = "native",
                       gp = gpar(col = col, lwd = 1, lty = 3))
        }
    }
}
