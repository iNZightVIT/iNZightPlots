drawAxes <- function(x, which = "x", main = TRUE, label = TRUE, opts, sub = 0, heightOnly = FALSE,
                     layout.only = FALSE) {
    if (is.numeric(x)) { 
        switch(which,
               "x" = {
                   if (main) {
                       grid.xaxis(gp = gpar(cex = opts$cex.axis), main = main, label = label)
                   } else {
                       xlim <- current.viewport()$xscale
                       pushViewport(viewport(x = 0.5, y = 1, height = unit(sub, "in"), just = "bottom",
                                             xscale = xlim))
                       grid.xaxis(gp = gpar(cex = opts$cex.axis), label = label, main = FALSE)
                       upViewport()
                   }
               }, "y" = {
                   yax <- yaxisGrob(gp = gpar(cex = opts$cex.axis), main = main, label = label)
                   if (label)
                       yax <- editGrob(yax, edits =
                                       gEdit("labels", rot = ifelse(main, 90, 270),
                                             hjust = 0.5, vjust = ifelse(main, 0, -0.5)))
                   grid.draw(yax)
               })
    } else {
        if (is.null(opts$ZOOM))
            x.lev <- levels(x)
        else {
            ZOOM <- opts$ZOOM
            ww <- ZOOM[1]:(sum(ZOOM) - 1)
            nl <- length(levels(x))
            ww <- ww - nl * (ww > nl)
            x.lev <- levels(x)[ww]
        }
        
        switch(which,
               "x" = {
                   rot <- opts$rot
                   labText <- textGrob(x.lev,
                                       x = unit((0:length(x.lev))[-1] - 0.5, "native"),
                                       y = if (rot) unit(-0.5, "mm") else unit(-1, "lines"),
                                       just = if (rot) c("right", "top") else "center",
                                       rot = ifelse(rot, 30, 0),
                                       gp = gpar(cex = opts$cex.axis * ifelse(rot, 0.8, 1)),
                                       name = "labelText")  # label is important!
                   wm <- which.max(nchar(as.character(x.lev)))
                   tt <- textGrob(levels(x)[wm])
                   # save label widths
                   labwid <- convertWidth(grobWidth(tt), "mm", valueOnly = TRUE)

                   if (heightOnly) {
                       return(grobHeight(labText))
                   } else {
                       grid.draw(labText)
                   }
               }, "y" = {
                   if (!is.null(x) & !layout.only) {
                       labels <- levels(x)
                       Nlab <- length(labels)
                       for (i in 1:Nlab) {
                           seekViewport(paste0("VP:plotregion-", i))
                           grid.text(labels[i], x = unit(-0.5, "lines"), just = "right", gp = gpar(cex = opts$cex.axis))
                           upViewport()
                       }
                   }
               })
    }
}


addGrid <- function(x = FALSE, y = FALSE, gen, opts) {
    if (!opts$grid.lines) return()
    if (!any(x, y)) return()

    col.grid <- opts$col.grid
    if (col.grid == "default") {
        if (any(col2rgb(opts$bg) > 230)) {
            col.grid <- "#00000020"
        } else {
            col.grid <- "#ffffff30"
        }
    }

    if (x) {
        at.x <- pretty(gen$LIM[1:2])
        at.X <- rep(at.x, each = 2)
        at.Y <- rep(current.viewport()$yscale, length(at.x))
        grid.polyline(at.X, at.Y, id.lengths = rep(2, length(at.X)/2),
                      default.units = "native",
                      gp = gpar(col = col.grid, lwd = 1))
    }
    if (y) {
        at.y <- pretty(gen$LIM[3:4])
        at.Y <- rep(at.y, each = 2)
        at.X <- rep(current.viewport()$xscale, length(at.y))
        grid.polyline(at.X, at.Y, id.lengths = rep(2, length(at.Y)/2),
                      default.units = "native",
                      gp = gpar(col = col.grid, lwd = 1))
    }
}
