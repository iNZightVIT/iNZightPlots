drawBoxPlot <-
function(x, opts) {
  # Draws a box plot in a prepared grid layout.
  # Assumes it can fill the entire region.

  # Calculate quantiles for box
    q <- quantile(x, c(0, 0.25, 0.5, 0.75, 1))

  # Draw the box
    lwd <- rep(opts$box.lwd, length = 2)
    grid.rect(x = q[2], y = unit(0.8, "npc"),
              width = q[4] - q[2],
              height = unit(0.6, "npc"),
              default.units = "native",
              just = c("left", "top"),
              gp =
              gpar(lwd  = lwd[1],
                   col  = opts$box.col,
                   fill = opts$box.fill))

  # Draw the median
    grid.lines(x = unit(q[3], "native"),
               y = unit(c(0.2, 0.8), "npc"),
               gp = gpar(lwd = lwd[1], col = opts$box.col))

  # Draw the whiskers
    grid.lines(x = unit(q[1:2], "native"), y = 0.5,
               gp = gpar(lwd = lwd[2], col = opts$box.col))
    grid.lines(x = unit(q[4:5], "native"), y = 0.5,
               gp = gpar(lwd = lwd[2], col = opts$box.col))
}
