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

    # Here, we create a class for the object to be plotted, then we use a generic function `create`
    # which will use the correct method, and create the required plot.
    
    pclass <- paste("inz", type, sep = ".")
    obj <- structure(.Data = list(df = df, opts = opts, xattr = xattr),
                     class = pclass)
    create(obj)
}

create <- function(obj)
    UseMethod("create")
