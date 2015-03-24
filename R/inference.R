inference <- function(object, ...)
    UseMethod("inference")


inference.inzdot <- function(object, bs, class, width, ...) {
    toplot <- object$toplot
    inf <- object$inference.info

    if (is.null(inf[[1]]$conf))
        stop("Please specify `inference.type = conf` to get inference information.")

    mat <- inf$mean$conf[, c("lower", "mean", "upper"), drop = FALSE]
    
    mat <- matrix(apply(mat, 2, function(col) {
        format(col, digits = 4)
    }), nrow = nrow(mat))
    
    ## Remove NA's and replace with an empty space
    mat[grep("NA", mat)] <- ""
    
    ## Text formatting to return a character vector - each row of matrix
    mat <- rbind(c("Lower CI", "Mean", "Upper CI"), mat)
    colnames(mat) <- NULL
    
    byFactor <- length(toplot) > 1
    
    if (byFactor)
        mat <- cbind(c("", names(toplot)), mat)
    rownames(mat) <- NULL
    
    mat <- matrix(apply(mat, 2, function(col) {
        format(col, justify = "right")
    }), nrow = nrow(mat))
    
    out <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
    
    plural <- ifelse(byFactor, "s", "")
    bsCI <- ifelse(bs, " Percentile Bootstrap", "")
    out <- c(paste0(ifelse(byFactor,
                           "Group Means", "Mean"),
                    " with 95%", bsCI, " Confidence Interval", plural), "",
             out)

    if (bs) {
        ## BOOTSTRAP MEDIAN
        mat <- inf$median$conf[, c("lower", "mean", "upper"), drop = FALSE]
        
        mat <- matrix(apply(mat, 2, function(col) {
            format(col, digits = 4)
        }), nrow = nrow(mat))
        
        ## Remove NA's and replace with an empty space
        mat[grep("NA", mat)] <- ""
        
        ## Text formatting to return a character vector - each row of matrix
        mat <- rbind(c("Lower CI", "Median", "Upper CI"), mat)
        colnames(mat) <- NULL
        
        byFactor <- length(toplot) > 1
        
        if (byFactor)
            mat <- cbind(c("", names(toplot)), mat)
        rownames(mat) <- NULL
        
        mat <- matrix(apply(mat, 2, function(col) {
            format(col, justify = "right")
        }), nrow = nrow(mat))
        
        mat <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
        
        out <- c(out, "",
                 paste0(ifelse(byFactor,
                           "Group Medians", "Median"),
                    " with 95%", bsCI, " Confidence Interval", plural), "", mat)


        ## BOOTSTRAP INTERQUARTILE RANGE
        mat <- inf$iqr$conf[, c("lower", "mean", "upper"), drop = FALSE]
        
        mat <- matrix(apply(mat, 2, function(col) {
            format(col, digits = 4)
        }), nrow = nrow(mat))
        
        ## Remove NA's and replace with an empty space
        mat[grep("NA", mat)] <- ""
        
        ## Text formatting to return a character vector - each row of matrix
        mat <- rbind(c("Lower CI", "IQR", "Upper CI"), mat)
        colnames(mat) <- NULL
        
        byFactor <- length(toplot) > 1
        
        if (byFactor)
            mat <- cbind(c("", names(toplot)), mat)
        rownames(mat) <- NULL
        
        mat <- matrix(apply(mat, 2, function(col) {
            format(col, justify = "right")
        }), nrow = nrow(mat))
        
        mat <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
        
        out <- c(out, "",
                 paste0(ifelse(byFactor,
                           "Group Interquartile Ranges", "Interquartile Range"),
                    " with 95%", bsCI, " Confidence Interval", plural), "", mat)
    }


    
    ### NOTE: this doesn't account for SURVEY design yet.
    
    if (byFactor & !bs) {
        ## For x ~ factor, we also include an F test, and multiple comparisons
        ## (estimates, pvalues, and confidence intervals).
        
        dat <- do.call(rbind, lapply(names(toplot),
                                     function(t) {
                                         if (is.null(toplot[[t]]$x))
                                                 NULL
                                         else
                                             data.frame(x = toplot[[t]]$x, y = t)
                                     }
                                     ))
        fit <- lm(x ~ y, data = dat)
        
        fstat <- summary(fit)$fstatistic
        
        Ftest <- c("Overall F-test", "",
                   paste0("F: ", signif(fstat[1], 5), "   ", 
                          "df: ", fstat[2], " and ", fstat[3], "   ",
                          "p-value: ",
                          format.pval(pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE),
                                      digits = 2)))
        
        out <- c(out, "", Ftest, "",
                 "", "   *** Differences between Group Means (column - row) ***", "")
        
        means <- predict(fit, newdata = data.frame(y = levels(dat$y)))
        names(means) <- LEVELS <- levels(dat$y)
        diffMat <- outer(means, means, function(x, y) y - x)
        diffMat <- formatTriMat(diffMat, LEVELS)
        
        out <- c(out,
                 "Estimates", "",
                 apply(diffMat, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
        
        mc <- try(s20x::multipleComp(fit))
        if (!inherits(mc, "try-error")) {
            cimat <- triangularMatrix(LEVELS, mc, "ci")
            cimat <- formatMat(cimat)
            
            out <- c(out, "",
                     "95% Confidence Intervals (adjusted for multiple comparisons)", "",
                     apply(cimat, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
            
            
            pmat <- triangularMatrix(LEVELS, mc, "p-values")
            pmat <- formatMat(pmat, 2)
            
            out <- c(out, "",
                     "P-values", "",
                     apply(pmat, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
            
        } else {
            out <- c(out, "", "Unable to compute confidence intervals and p-values.")
        }
    }
    
    out
}

formatTriMat <- function(mat, names) {
    ## Formats a (lower) triangular matrix nicely for display:
    
    mat[!lower.tri(mat)] <- NA
    mat <- mat[-1, ]
    
    mat <- matrix(apply(mat, 2, function(col) {
        format(col, digits = 4)
    }), nrow = nrow(mat))
    mat[grep("NA", mat)] <- ""
    
    mat <- cbind(c("", names[-1]),
                 rbind(names, mat))
    mat <- mat[, -ncol(mat)]
    
    mat <- matrix(apply(mat, 2, function(col) {
        format(col, justify = "right")
    }), nrow = nrow(mat))

    mat
}

formatMat <- function(mat, digits = 4) {
    dn <- dimnames(mat)
    
    mat <- t(apply(mat, 1, function(x) suppressWarnings(as.numeric(x))))
    mat <-  matrix(apply(mat, 2, function(col) {
        format(col, digits = digits)
    }), nrow = nrow(mat))
    mat[grep("NA", mat)] <- ""

    mat <- cbind(c("", dn[[1]]),
                 rbind(dn[[2]], mat))
    
    mat <- matrix(apply(mat, 2, function(col) {
        format(col, justify = "right")
    }), nrow = nrow(mat))

    mat
}



inference.inzbar <- function(object, ...) {

    NULL
}


inference.inzscatter <- function(object, ...) {

    NULL
}
