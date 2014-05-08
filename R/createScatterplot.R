create.inz.scatterplot <- function(obj) {
    # take the dataframe and settings from the object
    df <- obj$df
    opts <- obj$opts
    v <- colnames(df)

    # first need to remove missing values
    missing <- apply(df, 1, function(x) any(is.na(x)))
    n.missing <- sum(missing)
    df <- df[!missing, ]
    
    # --- look through inzpar for settings

    # Jitter on x and y
    if ("x" %in% strsplit(opts$jitter, '')[[1]])
        df$x <- jitter(df$x)
    if ("y" %in% strsplit(opts$jitter, '')[[1]])
        df$y <- jitter(df$y)

    # colour of points
    if ("colby" %in% v) {
        nby <- length(levels(df$colby))
        if (length(opts$col.pt) >= nby) {
            pt.col <- opts$col.pt[1:nby]
        } else {
            pt.col <- hcl((1:nby) / nby * 360, c = 80, l = 50)
        }
        pt.col <- pt.col[as.numeric(df$colby)]
    } else {
        pt.col <- opts$col.pt[1]
    }

    ## --- this is where FREQUENCY or SURVEY information is used to control sizes of points
    # size of points
    if ("(freqs)" %in% v) {
        propsize <- df$`(freqs)`
    } else if ("(weights)" %in% v) {
        propsize <- df$`(weights)`
    } else if ("sizeby" %in% v) {
        propsize <- df$sizeby
    } else {
        propsize <- NULL
    }
    if (!is.null(propsize))
        propsize <- rescale(propsize)

    # Combine everything together into a classed list which will have a `plot` method
    out <- list(x = df$x, y = df$y, cols = pt.col, propsize = propsize,
                n.missing = n.missing,
                xlim = range(df$x), ylim = range(df$y))
    class(out) <- "inzscatter"

    out
}
