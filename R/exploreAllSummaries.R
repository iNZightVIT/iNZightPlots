##' Allows easy access to a summary for every variable in the data set.
##'
##' @title Explore all Univariate Summaries
##' @param data a data set
##' @return allSummaries object
##' @author tell029
##' @export
exploreAllSummaries <- function(data) {
    ## Runs getPlotSummary() on all variables.
    
    sums <- lapply(colnames(data),
                   function(x) {
                       paste(getPlotSummary(data[, x], varnames = list(x = x)),
                             collapse = "\n")
                   })

    tt <- paste(sums, collapse = "\n\n")

    class(tt) <- "allSummaries"
    tt
}

##' @export
print.allSummaries <- function(x, ...)
    cat(x)
