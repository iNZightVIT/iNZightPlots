plot.iNZightObject <- function(o) {
    args <- list(x = o$x, y = o$y, g1 = o$g1, g2 = o$g2,
                 g1.level = o$g1.level, g2.level = o$g2.level,
                 varnames = o$varnames, xlab = o$xlab, ylab = o$ylab,
                 by = o$by, prop.size = o$prop.size)
    do.call(iNZightPlot, c(args, o$other))
}

summary.iNZightObject <- function(o) {
    args <- list(x = o$x, y = o$y, g1 = o$g1, g2 = o$g2,
                 g1.level = o$g1.level, g2.level = o$g2.level,
                 varnames = o$varnames, xlab = o$xlab, ylab = o$ylab,
                 by = o$by, prop.size = o$prop.size)
    sum <- do.call(getPlotSummary, c(args, o$other))
    cat(sum, sep = '\n')
}

inference <- function(o)
    UseMethod("inference")

inference.iNZightObject <- function(o) {
    args <- list(x = o$x, y = o$y, g1 = o$g1, g2 = o$g2,
                 g1.level = o$g1.level, g2.level = o$g2.level,
                 varnames = o$varnames, xlab = o$xlab, ylab = o$ylab,
                 by = o$by, prop.size = o$prop.size)
    inf <- do.call(getPlotInference, c(args, o$other))
    cat(inf, sep = '\n')
}

print.iNZightObject <- function(o) {
    cat("\n")
    cat("An object of class `iNZightObject`, with the following methods:\n\n")
    cat("- plot()\n- summary()\n- inference()\n\n")
}


print.iNZightPlotSummary <- function(o)
    cat(o, sep = "\n")
