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


inference.inzscatter <- function(object, bs, vn, ...) {
    d <- data.frame(x = object$x, y = object$y)
    trend <- object$trend
    
    if (is.null(trend))
        return("Please specify a trend line to obtain inference information.")

    ## Ensure the order is linear/quad/cubic
    allT <- c("linear", "quadratic", "cubic")
    tr <- (1:3)[allT %in% trend]
    
    out <- character()

    for (t in tr) {
        fit <- switch(t,
                      lm(y ~ x, data = d),
                      lm(y ~ x + I(x^2), data = d),
                      lm(y ~ x + I(x^2) + I(x^3), data = d))
        
        cc <- summary(fit)$coef
        ci <- confint(fit)

        mat <-
           ## Option 1: 'default' but includes extraneous decimal places for larger numbers
           # cbind(format(cc[, 1], digits = 4),
           #       format(ci[, 1], digits = 4),
           #       format(ci[, 2], digits = 4),
           ## Option 2: align decimal points in columns; ok, but maybe not
           # cbind(decimal.align(sprintf("%.5g", cc[, 1])),
           #       decimal.align(sprintf("%.5g", ci[, 1])),
           #       decimal.align(sprintf("%.5g", ci[, 2])),
           ## Option 3: flush everything right
            cbind(sprintf("%.5g", cc[, 1]),
                  sprintf("%.5g", ci[, 1]),
                  sprintf("%.5g", ci[, 2]),
                  format.pval(cc[, 4], digits = 2))

        rn <- paste0(vn$x, c("", "^2", "^3"))
        
        mat <- rbind(c("Estimate", "Lower CI", "Upper CI", "p-value"), mat)
        mat <- cbind(c("", "Intercept", rn[1:t]), mat)
        mat <- apply(mat, 2, function(x) format(x, justify = "right"))

        out <- c(out, "",
                 paste0(switch(t, "Linear", "Quadratic", "Cubic"), " Trend Coefficients with 95% ",
                        ifelse(bs, "Percentile Bootstrap ", ""), "Confidence Intervals"), "",
                 apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
    }
    

    out
}



decimal.align <- 
    function (x, dechar = ".", nint = NA, ndec = NA, pad.left = TRUE) 
{
    ## The following function is taken from "prettyR" package version 2.1
    ## We only included it here to remove the need to include an entirely
    ## new package in the iNZightVIT distribution.
    
    splitchar <- paste("[", dechar, "]", sep = "")
    splitlist <- strsplit(as.character(x), splitchar)
    ints <- unlist(lapply(splitlist, "[", 1))
    ints[is.na(ints)] <- "0"
    if (pad.left) {
        if (is.na(nint)) 
            nint <- max(nchar(ints))
        ints <- sprintf(paste("%", nint, "s", sep = ""), ints)
    }
    if (is.na(ndec)) 
        ndec <- max(nchar(unlist(lapply(splitlist, "[", 2))))
    decs <- unlist(lapply(splitlist, "[", 2))
    decs[is.na(decs)] <- "0"
    decs <- sprintf(paste("%-", ndec, "s", sep = ""), decs)
    return(paste(ints, decs, sep = dechar))
}
