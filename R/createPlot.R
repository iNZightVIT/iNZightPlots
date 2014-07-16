createPlot <- function(df, opts, xattr) {
  # This function takes a data.frame object and creates the necessary object which will have a
  # `plot` method.

    if (is.null(df))
        return(nullPlot(opts, xattr))

    large <- ifelse(is.null(lg <- opts$largesample),
                    nrow(df) > opts$large.sample.size, lg)
    wts <- xattr$class != "inz.simple"
    
    v <- xattr$v
    vt <- xattr$vartypes
    xfact <- vt$x == "factor"
    ynull <- ! "y" %in% v
    yfact <- if (ynull) NULL else vt$y == "factor"
    xnum <- !xfact
    ynum <- if (ynull) NULL else !xfact

    ## allow forcing the plot type:
    ## -- here, the switch takes the given plot type, checks the data types are correct,
    ## and if they aren't, just uses the default
    plottype <- gsub("plot", "", opts$plottype)  # remove `plot` from type, if specified
    type <- switch(plottype,
                   "bar" = ifelse(xfact & (ynull | yfact), plottype, "default"),
                   "hist" = ,
                   "dot" = ifelse(xnum & !ynum, plottype, "default"),
                   "scatter" = ,
                   "grid" = ,
                   "hex" = ifelse(xnum & ynum, plottype, "default"),
                   "default")

    # throw a warning if they give an invalid type
    if (type != plottype)
        warning("The plot type specified does not match the supplied data.")

    if (type == "default") {
        if (ynull) {
            type <- ifelse(xfact, "barplot",
                           ifelse(large | wts, "histplot", "dotplot"))
        } else {
            type <- ifelse(xfact,
                           ifelse(yfact, "barplot",
                                  ifelse(large | wts, "histplot", "dotplot")),
                           ifelse(yfact,
                                  ifelse(large, "histplot", "dotplot"),
                                  ifelse(large,
                                         ifelse(wts, "hexplot", "gridplot"),
                                         "scatterplot")))
        }
    } else {
        type <- paste0(type, "plot")
    }

    # Here, we create a class for the object to be plotted, then we use a generic function `create`
    # which will use the correct method, and create the required plot.
    
    pclass <- paste("inz", type, sep = ".")
    obj <- structure(.Data = list(df = df, opts = opts, xattr = xattr),
                     class = pclass)
    create(obj)
}

create <- function(obj)
    UseMethod("create")
