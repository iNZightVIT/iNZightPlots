drawLegend <-
function(legend) {
  # USES GRID
  # Draws a legend in the center of the current viewport.

    lab       <- legend$lab
    n         <- length(lab)
    col       <- legend$col
    pch       <- rep(legend$pch, length = n)
    cex       <- legend$cex
    cex.title <- ifelse(is.null(legend$cex.title), 1.2 * cex,
                        legend$cex.title)
    cex.pt    <- legend$cex.pt
    title     <- legend$title
    fill      <- legend$fill

    

  # Set up a viewport in the middle of the plotting area

  # Line position for each label
  # *** This is modified from the grid.legend() function in the Grid package ***
    gap <- unit(0.5, "lines")
    
    leg.width <-
        max(sapply(lab, function(x)
                   convertWidth(grobWidth(textGrob(x, gp =
                                                   gpar(cex = cex))), "mm")))
    legend.layout <-
        grid.layout(n, 4,
                    widths = unit.c(unit(0, "mm"), unit(2, "lines"),
                        unit(leg.width, "mm"), unit(0.5, "lines")), 
                    heights =
                    unit.pmax(unit(2, "lines"),
                              gap + unit(rep(1, n), "strheight", as.list(lab))))

    fg <- frameGrob(layout = legend.layout) #, gp = gpar(cex = opts$cex))
    
    for (i in 1L:n) {
        fg <- placeGrob(fg, pointsGrob(0.5, 0.5, pch = pch[i],
                                       gp = gpar(col = col[i], cex = cex.pt, lwd = 2)),
                        col = 2, row = i)
       
        fg <- placeGrob(fg, textGrob(lab[i], x = 0 , y = 0.5,
                                     just = c("left", "center"),
                                     gp = gpar(cex = cex)),
                        col = 3, row = i)
    }

    fg <- placeGrob(fg, textGrob(title, x = 0.5, y = 1,
                                 just = c("center", "bottom"),
                                 gp = gpar(cex = cex.title)))

    fg
}
