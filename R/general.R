rescale <- function(x)
    5 * (x - min(x)) / diff(range(x)) + 0.5
