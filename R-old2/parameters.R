params <- function(x = NULL) {
    if (!is.null(x)) {
        params()[[x]]
    } else {
        list(max.levels = 101)
    }
}
