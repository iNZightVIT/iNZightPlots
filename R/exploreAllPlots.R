exploreAllPlots <- function(data) {
    ## Runs iNZightPlot on all of the variables, with a click-for-next thing.

    dev.new()
    
    
    grid.newpage()
    pushViewport(viewport())

  # Title window:
    grid.text(paste("Click the next button to see next plot."))

    oask <- devAskNewPage(TRUE)
    on.exit(devAskNewPage(oask))
    for (i in 1:ncol(data)) {
        iNZightPlot(data[, i], varnames = list(x = colnames(data)[i]))
        dev.flush()
    }
}
