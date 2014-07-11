drawLegend <-
function(lab, col, pch = opts$pch, cex.mult = 1,
         title = "", any.missing = FALSE, opts = inzpar()) {

    legcex <- opts$cex.text * cex.mult

    title.grob <- textGrob(title, gp = gpar(cex = legcex * opts$cex.lab),
                           just = c("center"))
    if (title != "") {
        title.hgt <- convertHeight(grobHeight(title.grob), "in") * 2
        title.wd <- convertWidth(grobWidth(title.grob), "in", TRUE)
    } else {
        title.hgt <- unit(0, "in")
        title.wd <- 0
    }
    
    lab.width <- max(sapply(lab, function(x)
                            convertWidth(grobWidth(textGrob(x, gp = gpar(cex = legcex))),
                                         "in", TRUE)))
    lab.height <- convertHeight(grobHeight(textGrob(lab[1], gp = gpar(cex = legcex))),
                                "in", TRUE) * 2

    col2.wd <- max(title.wd - convertWidth(unit(2, "lines"), "in", TRUE), lab.width)

    ## want to create a layout with one row for each label, one for NA (if any are missing),
    ## a row for the title, and a line-height row between title and legend:
    if (any.missing) {
        lab <- append(lab, "missing")
        col <- append(col, opts$col.missing)
    }
    n <- length(lab)
    leg.layout <- grid.layout(n + 2, 3,
                              widths = unit.c(unit(2, "lines"), unit(col2.wd, "in"), unit(0.5, "lines")),
                              heights = unit.c(title.hgt, unit(0.5, "lines"),
                                  unit(rep(lab.height, n), "in")))
    
    fg <- frameGrob(layout = leg.layout)
    fg <- placeGrob(fg, title.grob, row = 1, col = 1:2)

    ## change pch into filled in one:
    if (pch %in% c(1, 2, 5, 6))
        pch <- switch(which(pch == c(1, 2, 5, 6)), 21, 24, 23, 25)

    for (i in 1:n) {
        fg <- placeGrob(fg, pointsGrob(0.5, 0.5, pch = pch,
                                       gp =
                                       gpar(col = col[i], cex = legcex, lwd = opts$lwd.pt,
                                            fill = col[i])),
                        col = 1, row = i + 2)
       
        fg <- placeGrob(fg, textGrob(lab[i], x = 0 , y = 0.5,
                                     just = c("left", "center"),
                                     gp = gpar(cex = legcex)),
                        col = 2, row = i + 2)
    }
    
    fg
}

drawContLegend <- function(var, title = "", height = NULL, cex.mult = 1,
                           any.missing = FALSE, opts = inzpar()) {

    legcex <- opts$cex.text * cex.mult

    title.grob <- textGrob(title, gp = gpar(cex = legcex * opts$cex.lab),
                           just = c("center"))
    title.hgt <- convertHeight(grobHeight(title.grob), "in") * 2

    vp <- viewport(yscale = range(var, na.rm = TRUE))
    yax <- yaxisGrob(main = FALSE, vp = vp, gp = gpar(cex = legcex * opts$cex.axis))

    ## need legend to fit the longest label
    var2 <- if (any.missing) c(var, "missing") else  var
    maxlablen <- convertWidth(grobWidth(textGrob(var2[which.max(nchar(var2))])), "in", TRUE)
    yax.wd <- (convertWidth(unit(1, "lines"), "in", TRUE) + maxlablen) * legcex * opts$cex.axis
    title.wd <- convertWidth(grobWidth(title.grob), "in", TRUE)
    rect.wd <- convertWidth(unit(2, "char"), "in", TRUE)
    col2.wd <- max(title.wd - rect.wd, yax.wd)
    
    legend.layout <- grid.layout(nrow = 5, ncol = 3,
                                 widths = unit.c(unit(rect.wd, "in"), unit(col2.wd, "in"), unit(0.5, "lines")),
                                 heights = unit.c(title.hgt, unit(0.5, "lines"), unit(height, "in"),
                                     unit(1, "lines"), unit(1, "lines")))

    ## vectorize the drawing of the scale to make it fast!
    xx <- rep(c(0, 1, 1, 0), 200)
    yy <- rep(0:200 / 200, each = 4)[1:800 + 2]
    id <- rep(1:200, each = 4)
    poly <- polygonGrob(xx, yy, id = id, gp = gpar(lty = 0, fill = (n.cols <- rainbow(200, start = 1/6))))
    
    fg <- frameGrob(layout = legend.layout)
    fg <- placeGrob(fg, poly, row = 3, col = 1)
    fg <- placeGrob(fg, rectGrob(width = unit(rect.wd, "in")),
                    col = 1, row = 3)
    fg <- placeGrob(fg, title.grob, row = 1, col = 1:2)
    fg <- placeGrob(fg, yax, row = 3, col = 1)

    if (any.missing) {
        fg <- placeGrob(fg, pointsGrob(0.5, 0.5, pch = 21, gp =
                                       gpar(col = opts$col.missing, cex = legcex,
                                            lwd = opts$lwd.pt, fill = opts$col.missing)),
                        row = 5, col = 1)
        fg <- placeGrob(fg, textGrob("missing", x = unit(1, "lines"), y = 0.5, just = c("left", "center"),
                                     gp = gpar(cex = legcex * opts$cex.axis)),
                    col = 2, row = 5)
    }
    
    list(fg = fg, n.cols = n.cols)
}


drawLinesLegend <- function(x, opts = inzpar(), cex.mult = 1) {

    lines.list <- list()
    if (length(opts$trend) > 0) {
        if (all(opts$trend != FALSE)) {
            if (opts$trend.by) { #opts$trend.parallel) {
                for (i in 1:length(opts$trend)) {
                    lines.list <- c(lines.list,
                                    list(c(opts$trend[i], "black",
                                           which(opts$trend[i] == c("linear", "quadratic", "cubic")),
                                           opts$lwd)))
                }
            } else {
                for (i in 1:length(opts$trend)) {
                    lines.list <- c(lines.list,
                                    list(c(opts$trend[i],
                                           opts$col.trend[[opts$trend[i]]],
                                           opts$lty, opts$lwd)))
                }
            }
        }
    }
    if (length(opts$quant.smooth) > 0) {
        qs <- calcQSmooth(x, opts$quant.smooth, opts)
        if (!is.null(qs)) {
            for (i in 1:length(qs$qp)) {
                lines.list <- c(lines.list,
                                list(c(paste0(qs$qp[i] * 100,
                                              ifelse(qs$qp[i] != 0.5,
                                                     paste0(" - ", (1 - qs$qp[i]) * 100),
                                                     ""),
                                              "%"),
                                       opts$col.smooth,
                                       qs$lty[i], qs$lwd[i])))
            }
        }
    } else {
        if (!is.null(opts$smooth)) {
            if (opts$smooth != 0) {
                if (opts$trend.by) {
                    lines.list <- c(lines.list,
                                    list(c("smoother", "black",
                                           opts$smoothby.lty, opts$lwd)))
                } else {
                    lines.list <- c(lines.list,
                                    list(c("smoother", opts$col.smooth,
                                           opts$lty, opts$lwd)))
                }
            }
        }
    }
    if (opts$LOE) {
        lines.list <- c(lines.list,
                        list(c("x=y line", opts$col.LOE, opts$lty.LOE, opts$lwd)))
    }


    ## if there aren't any lines, return nothing, otherwise return legend grob
    if (length(lines.list) == 0)
        return(NULL)
    
    legcex <- opts$cex.text * cex.mult
    lab <- sapply(lines.list, function(x) x[1])
    col <- sapply(lines.list, function(x) x[2])
    lty <- as.numeric(sapply(lines.list, function(x) x[3]))
    lwd <- as.numeric(sapply(lines.list, function(x) x[4]))
    
    lab.width <- max(sapply(lab, function(x)
                            convertWidth(grobWidth(textGrob(x, gp = gpar(cex = legcex))),
                                         "in", TRUE)))
    lab.height <- convertHeight(grobHeight(textGrob(lab[1], gp = gpar(cex = legcex))),
                                "in", TRUE) * 2

    n <- length(lab)
    leg.layout <- grid.layout(n + 1, 3,
                              widths = unit.c(unit(2, "lines"), unit(lab.width, "in"), unit(0.5, "lines")),
                              heights = unit.c(unit(2, "lines"), unit(rep(lab.height, n), "in")))
    
    fg <- frameGrob(layout = leg.layout)

    for (i in 1:n) {
        fg <- placeGrob(fg, linesGrob(c(0.2, 0.8), 0.5,
                                       gp =
                                       gpar(col = col[i], lty = lty[i], lwd = lwd[i] * 2)),
                        col = 1, row = i + 1)
       
        fg <- placeGrob(fg, textGrob(lab[i], x = 0 , y = 0.5,
                                     just = c("left", "center"),
                                     gp = gpar(cex = legcex)),
                        col = 2, row = i + 1)
    }
    
    fg
}
