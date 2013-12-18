drawDotplot <-
function(x, y, xlim, ylim, col, opts) {
  # Draws a dotplot in a selected space. Also draws the boxplot if it
  # is requested.

  # First step: set up the layout
    if (opts$box) {
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

  # Draw a box if asked for
    if (opts$box) {
        pushViewport(viewport(layout.pos.row = 2,
                              xscale = xlim))
        drawBoxPlot(x, opts)
        upViewport()
    }

  # Draw the dotplot in the first row
    pushViewport(viewport(layout.pos.row = 1,
                          xscale = xlim,
                          yscale = ylim))
    grid.points(x, y, default.units = "native",
                gp =
                gpar(cex = opts$cex.pt, col = col, pch = opts$pch,
                     lwd = opts$lwd.pt, fill = opts$fill.pt))
    upViewport()  # back to layout5

    upViewport()  # out of layout5
}
