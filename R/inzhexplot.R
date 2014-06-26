create.inz.hexplot <- function(obj) {
    # make a plot using hexagonal binning

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

    xbins <-
        if ((cpt <- opts$cex.pt) > 1) {
            (1 - (cpt - 1) / 2.5) * (25) + 5
        } else {
            (1 - (cpt - 0.05) / 0.95) * 70 + 30
        }
    ## hexbin returns an S4 object, so need to use the @ operator
    hb <- hexbin(df$x, df$y, IDs = TRUE, xbins = xbins)
    cellid <- hb@cID
    ## now manipulate the counts with the weight variable
    if (xattr$class == "inz.freq") {
        W <- df$freq
    } else {
        W <- weights(obj$df, "sampling")[!missing]
    }
    hb@count <- as.vector(tapply(W, cellid, sum))
    hb@xcm <- as.vector(tapply(1:length(df$x), cellid,
                               function(i) weighted.mean(df$x[i], W[i])))
    hb@ycm <- as.vector(tapply(1:length(df$y), cellid,
                               function(i) weighted.mean(df$x[i], W[i])))

    out <- list(hex = hb, n.missing = n.missing,
                xlim = if (nrow(df) > 0) range(df$x, na.rm = TRUE) else c(-Inf, Inf),
                ylim = if (nrow(df) > 0) range(df$y, na.rm = TRUE) else c(-Inf, Inf))
    class(out) <- "inzhex"

    out
}

plot.inzhex <- function(obj, gen) {
    grid.hexagons(obj$hex, style = "centroids")
}
