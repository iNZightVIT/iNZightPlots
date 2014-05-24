create.inz.scatterplot <- function(obj) {
    # take the dataframe and settings from the object
    df <- obj$df
    opts <- obj$opts
    v <- colnames(df)

    # first need to remove missing values
    missing <- apply(df[ , v %in% c("x", "y")], 1, function(x) any(is.na(x)))
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
        if (is.factor(df$colby)) {
            nby <- length(levels(df$colby))
            if (length(opts$col.pt) >= nby) {
                pt.col <- opts$col.pt[1:nby]
            } else {
                pt.col <- rainbow(nby, v = 0.7, start = 1/6) #hcl((1:nby) / nby * 360, c = 70, l = 30)
            }
            pt.col <- ifelse(is.na(df$colby), "grey50", pt.col[as.numeric(df$colby)])
        } else {
            ## rescale the colour-by variable on a scale from 1-200 and then use rainbow colours
            cb <- df$colby
            cbsc <- as.integer(199 * ((cb - min(cb, na.rm = TRUE)) / diff(range(cb, na.rm = TRUE))) + 1)
            pt.col <- ifelse(is.na(cb), "grey50", rainbow(200, start = 1/6)[cbsc])
        }
    } else {
        pt.col <- opts$col.pt[1]
    }

    ## The plotting symbol:
    pch <- rep(ifelse(opts$alpha == 1, opts$pch, 19), nrow(df))


    ## --- this is where FREQUENCY or SURVEY information is used to control sizes of points
    # size of points
    if ("(freqs)" %in% v) {
        propsize <- df$`(freqs)`
    } else if ("(weights)" %in% v) {
        propsize <- df$`(weights)`
    } else if ("sizeby" %in% v) {
        propsize <- df$sizeby
    } else {
        propsize <- 1
    }
    
    pch[is.na(propsize)] <- 4
    propsize[is.na(propsize)] <- 1


    # Combine everything together into a classed list which will have a `plot` method
    out <- list(x = df$x, y = df$y, cols = pt.col, propsize = propsize, pch = pch,
                n.missing = n.missing,
                grid.plot =
                if (is.null(opts$largesample)) length(df$x) > opts$large.sample.size
                else opts$largesample,
                nacol = if ("colby" %in% v) any(is.na(df$colby)) else FALSE,
                nasize = if ("sizeby" %in% v) any(is.na(df$sizeby)) else FALSE,
                xlim = range(df$x), ylim = range(df$y))

    
    if (out$grid.plot) {
        ## if using a grid plot, forget sizing by anything execpt for weights and frequencies
        out$propsize <- NULL  # remove propsize
        if ("(freqs)" %in% v) {
            out$freq <- df$`(freqs)`
        }
        if ("(weights)" %in% v) {
            out$weight <- df$`(weights)`
        }
    }
    class(out) <- "inzscatter"

    out
}
