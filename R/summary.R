summary.inzdot <- function(object) {
    ## Generate summary information:


    ## Produce a matrix of the required summary:
    toplot <- object$toplot
    missing <- object$n.missing
    if (!is.null(attr(missing, "levels")))
            missing <- attr(missing, "levels")
    else
        missing <- structure(missing, .Names = "all")
    
    do.call(rbind, lapply(names(toplot), function(p) {
        x <- toplot[[p]]$x
        
        s <- suppressWarnings(
            c(min(x), quantile(x, 0.25), quantile(x, 0.5), mean(x), quantile(x, 0.75), max(x),
              sd(x), length(x) + missing(p), missing[p])
        )
        s[!is.finite(s)] <- NA
        signif(s, 4)
    })) -> mat

    mat[is.na(mat)] <- ""

    ## Text formatting to return a character vector - each row of matrix
    mat <- rbind(c("Min", "25%", "Median", "Mean", "75%", "Max", "SD", "Sample Size", "N Missing"), mat)
    colnames(mat) <- NULL

    if (length(toplot) > 1) {
        mat <- cbind(c("", names(toplot)), mat)
    } else {
        mat <- mat[, -1]
    }
    rownames(mat) <- NULL
    
    mat <- matrix(apply(mat, 2, function(col) {
        format(col, justify = "right")
    }), nrow = nrow(mat))

    mat <- apply(mat, 1, paste, collapse = "   ")
    mat
}
