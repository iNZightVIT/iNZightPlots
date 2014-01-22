darken <- function(x = "#FFFFFF") {
    x <- gsub('#', '', x)
    if (nchar(x) == 3)
        x <- paste(rep(strsplit(x, '')[[1]], each = 2), collapse = '')

    if (nchar(x) != 6)
        stop("Not a valid hexadecimal code!")

  # Now start the function!

    r <- substr(x, 1, 2)
    g <- substr(x, 3, 4)
    b <- substr(x, 5, 6)

    dark <- strtoi(c(r, g, b), base = 16) * 0.6 / 255
    
    rgb(dark[1], dark[2], dark[3])
}
