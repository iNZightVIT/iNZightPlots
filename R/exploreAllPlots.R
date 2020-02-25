#' Allows easy viewing of every variable in the data set.
#'
#' @title Explore all Univariate Plots
#' @param data a data frame
#' @return NULL
#' @author Tom Elliott
#' @export
exploreAllPlots <- function(data) {
    ## Runs iNZightPlot on all of the variables, with a click-for-next thing.

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
    devAskNewPage(oask)
    grid.newpage()
}
