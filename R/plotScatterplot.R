plot.inzscatter <- function(obj, inzpar = inzpar()) {
    pushViewport(viewport(xscale = obj$xlim, yscale = obj$ylim))

    grid.points(obj$x, obj$y)

    invisible(NULL)
}
