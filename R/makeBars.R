makeBars <- function(x, y = NULL, cols = NULL) {
  # Takes factor x and y variables and computes the bar heights

    if (is.null(y)) {
        hgt <- table(x) / length(x)
    } else {
        tab <- table(y, x)
        hgt <- sweep(tab, 1, rowSums(tab), "/")
    }
    
    hgt
}
