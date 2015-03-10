getSummary <- function(x, y = NULL, g1 = NULL, g1.level = NULL,
                       g2 = NULL, g2.level = NULL, varnames = list(),
                       colby = NULL, sizeby = NULL,
                       data = NULL, design = NULL, freq = NULL,
                       missing.info = TRUE, inzpars = inzpar(), ...) {

    ## Grab a plot object!
    obj <- iNZightPlot(x, y, g1, g1.level, g2, g2.level, varnames, colby, sizeby,
                       data, design, freq, missing.info, inzpars = inzpars,
                       plot = FALSE, ...)


    obj

}
