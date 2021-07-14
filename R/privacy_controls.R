make_privacy_controls <- function(ctrls = NULL) {
    if (is.null(ctrls)) return(NULL)

    if (is.null(ctrls$rounding)) ctrls$rounding <- ""
    if (is.null(ctrls$suppression)) ctrls$suppression <- NA

    list(
        round = switch(ctrls$rounding,
            "RR3" = function(x) {
                d <- dim(x)
                dn <- dimnames(x)
                x[x %% 3 == 1] <- sapply(x[x %% 3 == 1], function(z) ifelse(runif(1) < 2/3, z - 1, z + 2))
                x[x %% 3 == 2] <- sapply(x[x %% 3 == 2], function(z) ifelse(runif(1) < 1/3, z - 1, z + 2))
                dim(x) <- d
                dimnames(x) <- dn
                x
            },
            function(x) x
        ),
        suppression_matrix = function(x) {
            d <- dim(x)
            dn <- dimnames(x)
            x <- x < ctrls$suppression
            dim(x) <- d
            dimnames(x) <- dn
            if (length(d) == 2) {
                # two way
                x <- cbind(x, apply(x, 1, sum) == 1L)
            } else {
                # one way
                x <- c(x, TRUE)
            }
            x
        },
        suppress = function(x, mat, symbol = "S") {
            if (is.null(mat)) return(x)
            x[mat] <- symbol
            x
        }
    )
}
