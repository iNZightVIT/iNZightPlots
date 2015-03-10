getSummary <- function(x, y = NULL, g1 = NULL, g1.level = NULL,
                       g2 = NULL, g2.level = NULL, varnames = list(),
                       colby = NULL, sizeby = NULL,
                       data = NULL, design = NULL, freq = NULL,
                       missing.info = TRUE, inzpars = inzpar(), ...) {

    ## Grab a plot object!
    m <- match.call(expand.dots = FALSE)
    env <- parent.frame()

    if ("design" %in% names(m)) {
        md <- eval(m$design, env)
    } else {
        md <- eval(m$data, env)
    }
    
    ## we now want to create a data object which contains *ALL* of the necessary
    ## information, including survey design, or frequency information:
    df <- inzDataframe(m, data = md, names = varnames, g1.level, g2.level, env = env)

    obj <- iNZightPlot(x = x, y = y, g1 = g1, g1.level = g1.level,
                       g2 = g2, g2.level = g2.level, varnames = varnames,
                       colby = colby, sizeby = sizeby,
                       data = data, design = design, freq = freq,
                       missing.info = missing.info, inzpars = inzpars,
                       plot = FALSE, df = df, ...)


    ### Now we just loop over everything ...

    print(varnames)

}
