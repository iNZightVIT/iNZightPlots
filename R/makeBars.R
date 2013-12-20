makeBars <- function(x, y = NULL, cols = NULL) {
  # Takes factor x and y variables and computes the bar heights

    if (is.null(y)) {
        hgt <- table(x) / length(x)
    } else {
        tab <- table(x, y)
        hgt <- apply(tab, 2, function(x) x / sum(x))
    }

    hgt
}
