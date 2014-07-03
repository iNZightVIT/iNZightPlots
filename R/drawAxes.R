drawAxes <- function(x, which = "x", main = TRUE, label = TRUE, opts, sub = 0) {
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
        x.lev <- levels(x)
        switch(which,
               "x" = {
                   labText <- textGrob(x.lev,
                                       x = unit((0:length(x.lev))[-1] - 0.5, "native"),
                                       y = unit(-1, "lines"),
                                       gp = gpar(cex = opts$cex.axis),
                                       name = "labelText")  # label is important!

                   wm <- which.max(nchar(as.character(x.lev)))
                   tt <- textGrob(levels(x)[wm])
                   # save label widths
                   labwid <- convertWidth(grobWidth(tt), "mm", valueOnly = TRUE)
                   grid.draw(labText)
               }, "y" = {

               })
    }
}
