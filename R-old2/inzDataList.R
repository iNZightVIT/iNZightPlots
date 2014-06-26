inzDataList <- function(d, x) {
    
    if (x != "all") {
        # for some reason, the subset() function gives errors
        w <- d$g1 == x
        df <- d[w & !is.na(w), , drop = FALSE]
    } else {
        df <- d
    }

    df <- df[, colnames(df) != "g1", drop = FALSE]
    df
}
