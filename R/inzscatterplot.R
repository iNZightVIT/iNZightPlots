create.inz.scatterplot <- function(obj) {
    # take the dataframe and settings from the object
    df <- obj$df
    opts <- obj$opts
    xattr <- obj$xattr
    
    if (xattr$class == "inz.survey")
        df <- df$variables
    
    v <- colnames(df)
    vn <- xattr$varnames
        
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
    if ("colby" %in% v & nrow(df) > 0) {
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
            cbsc <- as.integer(199 * ((cb - min(cb, na.rm = TRUE)) /
                                      diff(range(cb, na.rm = TRUE))) + 1)
            pt.col <- ifelse(is.na(cb), "grey50", rainbow(200, start = 1/6)[cbsc])
        }
    } else {
        pt.col <- opts$col.pt[1]
    }

    ## The plotting symbol:
    pch <- rep(ifelse(opts$alpha == 1, opts$pch, 19), nrow(df))

    ## --- this is where FREQUENCY or SURVEY information is used to control sizes of points
    # size of points
    if ("freq" %in% v) {
        propsize <- df$freq / xattr$max.freq * 4 + 0.5
    } else if ("weights" %in% v) {
        propsize <- df$weights
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
                nacol = if ("colby" %in% v) any(is.na(df$colby)) else FALSE,
                nasize = if ("sizeby" %in% v) any(is.na(df$sizeby)) else FALSE,
                xlim = if (nrow(df) > 0) range(df$x, na.rm = TRUE) else c(-Inf, Inf),
                ylim = if (nrow(df) > 0) range(df$y, na.rm = TRUE) else c(-Inf, Inf))

    class(out) <- "inzscatter"

    out
}


plot.inzscatter <- function(obj, gen) {
    
}
