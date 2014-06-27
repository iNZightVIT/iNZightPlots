params <- function(x = NULL) {
    ## A suite of parameters for controlling behaviour
    if (!is.null(x)) {
        params()[[x]]
    } else {
        list(max.levels = 101)
    }
}
