plot.inzscatter <- function(obj, inzpar = inzpar(),
                            axis = c(2, 2, 1, 1), title = NULL, sub = 0) {

    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale

    if (axis[1] > 0)
        grid.xaxis(gp = gpar(cex = inzpar$cex.axis), label = (axis[1] == 2))
    if (axis[2] > 0) {
        yax <- yaxisGrob(gp = gpar(cex = inzpar$cex.axis), label = (axis[2] == 2))
        if (axis[2] == 2) 
            yax <- editGrob(yax, edits = gEdit("labels", rot = 90, hjust = 0.5, vjust = 0))
        grid.draw(yax)
    }
    if (axis[3] > 0) {
        pushViewport(viewport(x = 0.5, y = 1, height = unit(sub, "in"), just = "bottom",
                              xscale = xlim))
        grid.xaxis(gp = gpar(cex = inzpar$cex.axis), label = (axis[3] == 2), main = FALSE)
        upViewport()
    }
    if (axis[4] > 0) {
        yax <- yaxisGrob(gp = gpar(cex = inzpar$cex.axis), label = (axis[4] == 2), main = FALSE)
        if (axis[4] == 2)
            yax <- editGrob(yax, edits = gEdit("labels", rot = 270, hjust = 0.5, vjust = -0.5))
        grid.draw(yax)
    }

    grid.points(obj$x, obj$y, pch = obj$pch, 
                gp =
                gpar(col = obj$cols, cex = obj$propsize * inzpar$cex.pt,
                     lwd = inzpar$lwd.pt, alpha = inzpar$alpha))

    invisible(NULL)
}


## NOTES to self: need to move the TOP xaxis UP over teh label when g2 multi
