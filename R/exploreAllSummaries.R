exploreAllSummaries <- function(data, gui = NULL) {
    ## Runs getPlotSummary() on all variables.
    
    sums <- lapply(colnames(data),
                   function(x) {
                       paste(getPlotSummary(data[, x], varnames = list(x = x)),
                             collapse = "\n")
                   })

    rule <- paste0(rep("-", 80), collapse = "")
    tt <- paste(sums, collapse = rule)

    class(tt) <- "allSummaries"
    tt
}

print.allSummaries <- function(x)
    cat(x)
