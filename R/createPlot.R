createPlot <- function(d, x) {    
    if (x != "all") {
        # for some reason, the subset() function gives errors
        w <- d$g1 == x
        out <- d[w & !is.na(w), ]
    } else {
        out <- d
    }

    out[, colnames(out) != "g1"]
}
