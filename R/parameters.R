params <- function(x) {
    if (is.null(x)) {
        parameters[[x]]
    } else {
        list(max.levels = 101)
    }
}
