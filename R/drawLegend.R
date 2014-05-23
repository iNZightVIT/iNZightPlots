drawLegend <-
function(legend) {
  # USES GRID
  # Draws a legend in the center of the current viewport.
    lab       <- legend$lab
    n         <- length(lab)
    col       <- legend$cols
    pch       <- rep(legend$pch, length = n)
    cex       <- legend$cex
    cex.title <- ifelse(is.null(legend$cex.title), 1.2 * cex,
                        legend$cex.title)
    cex.pt    <- legend$cex.pt
    cex.mult  <- legend$cex.mult
    lwd       <- legend$lwd
    title     <- legend$title
    fill      <- legend$fill

    

  # Set up a viewport in the middle of the plotting area

  # Line position for each label
  # *** This is modified from the grid.legend() function in the Grid package ***
    gap <- unit(0.5, "lines")
    
    leg.width <-
        max(sapply(c(title, lab), function(x)
                   convertWidth(grobWidth(textGrob(x, gp =
                                                   gpar(cex = cex * cex.mult))), "mm")))
    legend.layout <-
        grid.layout(n + 1, 4,
                    widths = unit.c(unit(0, "mm"), unit(2, "lines"),
                        unit(leg.width, "mm"), unit(0.5, "lines")), 
                    heights =
                    unit.pmax(unit(2, "lines"),
                              gap + unit(rep(1, n), "strheight", as.list(lab))))

    fg <- frameGrob(layout = legend.layout)
    
    for (i in 1L:n) {
        fg <- placeGrob(fg, pointsGrob(0.5, 0.5, pch = pch[i],
                                       gp =
                                       gpar(col = col[i], cex = cex.pt * cex.mult, lwd = lwd,
                                            fill = fill[i])),
                        col = 2, row = i + 1)
       
        fg <- placeGrob(fg, textGrob(lab[i], x = 0 , y = 0.5,
                                     just = c("left", "center"),
                                     gp = gpar(cex = cex * cex.mult)),
                        col = 3, row = i + 1)
    }

    fg <- placeGrob(fg, textGrob(title, x = 0.5, y = 1,
                                 just = "left",
                                 gp = gpar(cex = cex.title * cex.mult)),
                    col = 2, row = 1)

    fg
}



drawContLegend <- function(legend) {
    var <- legend$var
    title <- legend$title
    cex       <- legend$cex
    cex.title <- ifelse(is.null(legend$cex.title), 1.2 * cex,
                        legend$cex.title)
    cex.mult  <- legend$cex.mult
    cex.axis <- legend$cex.axis
    missCOL <- legend$missCOL
    cex.pt <- legend$cex.pt
    lwd <- legend$lwd
    

    title.grob <- textGrob(title, gp = gpar(cex = cex * cex.mult * cex.title),
                           just = c("center"))
    title.hgt <- convertHeight(grobHeight(title.grob), "in") * 2

    vp <- viewport(yscale = range(var, na.rm = TRUE))
    yax <- yaxisGrob(main = FALSE, vp = vp, gp = gpar(cex * cex.mult * cex.axis))
    
    yax.wd <- convertWidth(unit(2, "lines"), "in", TRUE) * cex * cex.mult * cex.axis * 1.5
    title.wd <- convertWidth(grobWidth(title.grob), "in", TRUE)
    rect.wd <- convertWidth(unit(2, "char"), "in", TRUE)
    tot.wd <- max(title.wd, rect.wd + yax.wd)
    
    legend.layout <- grid.layout(nrow = 5, ncol = 2,
                                 widths = unit(c(rect.wd, yax.wd), "in"),
                                 heights = unit.c(title.hgt, unit(1, "lines"), unit(0.4, "npc"),
                                     unit(1, "lines"), unit(1, "lines")))

    ## vectorize the drawing of the scale to make it fast!
    xx <- rep(c(0, 1, 1, 0), 200)
    yy <- rep(0:200 / 200, each = 4)[1:800 + 2]
    id <- rep(1:200, each = 4)
    poly <- polygonGrob(xx, yy, id = id, gp = gpar(lty = 0, fill = rainbow(200, start = 1/6)))
    
    fg <- frameGrob(layout = legend.layout)
    fg <- placeGrob(fg, rectGrob(width = unit(2, "char")),
                    col = 1, row = 3)
    fg <- placeGrob(fg, poly, row = 3, col = 1)
    fg <- placeGrob(fg, title.grob, row = 1)
    fg <- placeGrob(fg, yax, row = 3, col = 1)

    if (missCOL) {
        fg <- placeGrob(fg, pointsGrob(0.5, 0.5, gp =
                                       gpar(col = "grey50", cex = cex.pt * cex.mult, lwd = lwd)),
                        row = 5, col = 1)
        fg <- placeGrob(fg, textGrob("NA", x = 0.5, y = 0.5,
                                     gp = gpar(cex = cex * cex.mult)),
                    col = 2, row = 5)
    }
    
    fg
}
