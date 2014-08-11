create.inz.histplot <- function(obj) {
    create.inz.dotplot(obj, hist = TRUE)
}

plot.inzhist <- function(obj, gen) {
    # First step is to grab stuff:
    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    opts <- gen$opts
    mcex <- gen$mcex
    boxplot <- opts$boxplot

    toplot <- obj$toplot
    boxinfo <- obj$boxinfo

    nlev <- length(toplot)
    pushViewport(viewport(layout = grid.layout(nrow = nlev),
                          name = "VP:histplot-levels"))
    Hgts <- if (boxplot) c(3, 1) else c(1, 0)
    dpLayout <- grid.layout(nrow = 2, heights = unit(Hgts, "null"))

    ylim <- c(0, gen$maxcount * 1.05)

    for (i in 1:nlev) {
        pp <- toplot[[i]]
        seekViewport("VP:histplot-levels")
        pushViewport(viewport(layout.pos.row = i))
        pushViewport(viewport(layout = dpLayout))
        
        if (boxplot) {
            pushViewport(viewport(layout.pos.row = 2, xscale = xlim))
            addBoxplot(boxinfo[[i]])
            upViewport()
        }
        
        pushViewport(viewport(layout.pos.row = 1,
                              xscale = xlim, yscale = ylim,
                              name = paste0("VP:plotregion-", i)))

        if (length(pp$x) > 0) {
            bar.x <- rep(pp$breaks, each = 4)
            bar.x <- bar.x[-c(1:2, length(bar.x) - 0:1)]
            bar.y <- c(sapply(pp$counts, function(y) c(0, y, y, 0)))
            bar.id <- rep(1:length(pp$counts), each = 4)
            
            grid.polygon(bar.x, bar.y, bar.id, default.units = "native",
                         gp =
                         gpar(fill = opts$bar.fill, col = opts$bar.col,
                              lwd = opts$bar.lwd))
        }
    }

    seekViewport("VP:histplot-levels")
    upViewport()
}
