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
    if (axis[3] > 0)
        grid.xaxis(gp = gpar(cex = inzpar$cex.axis), label = (axis[3] == 2), main = FALSE,
                   edits = gEdit(hjust = unit(sub, "in")))
    if (axis[4] > 0) {
        yax <- yaxisGrob(gp = gpar(cex = inzpar$cex.axis), label = (axis[4] == 2), main = FALSE)
        if (axis[4] == 2)
            yax <- editGrob(yax, edits = gEdit("labels", rot = 270, hjust = 0.5, vjust = -0.5))
        grid.draw(yax)
    }
    
    grid.points(obj$x, obj$y)

    invisible(NULL)
}


## NOTES to self: need to move the TOP xaxis UP over teh label when g2 multi
