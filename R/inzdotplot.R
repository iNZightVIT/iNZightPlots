create.inz.dotplot <- function(obj) {
    df <- obj$df
    opts <- obj$opts
    xattr <- obj$xattr
    
    v <- colnames(df)
    vn <- xattr$varnames

    # first need to remove missing values
    missing <- is.na(df$x)
    n.missing <- sum(missing)
    df <- df[!missing, ]
}

plot.inzdot <- function(obj, gen) {
    
}
