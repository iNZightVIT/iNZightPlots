makeBars <- function(x, y = NULL, cols = NULL, freq = NULL) {
  # Takes factor x and y variables and computes the bar heights

    if (is.null(y)) {
        if (is.null(freq)) {
            tab <- table(x)
        } else {
            tab <- xtabs(freq ~ x)
        }
        hgt <- tab / sum(tab)
    } else {
        if (is.null(freq)) {
            tab <- table(y, x)
        } else {
            tab <- xtabs(freq ~ y + x)
        }
        hgt <- sweep(tab, 1, rowSums(tab), "/")
    }

    hgt
}
