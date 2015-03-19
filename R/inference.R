inference <- function(object, ...)
    UseMethod("inference")


inference.inzdot <- function(object, bs, width, ...) {
    toplot <- object$toplot
    inf <- object$inference.info

    if (bs) {
        "Not yet implemented."
    } else {
        
        means <- do.call(rbind, lapply(toplot, function(t) mean(t$x)))
        
        if (is.null(inf$conf))
            stop("Please specify `inference.type = conf` to get inference information.")
        
        mat <- cbind(means, inf$conf)
        mat <- mat[, c(2, 1, 3), drop = FALSE]
        
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
        out <- c(paste0(ifelse(byFactor,
                               "Group means", "Mean"),
                        " with 95% Confidence Interval", plural), "",
                 out)
        
        if (byFactor) {
            dat <- do.call(rbind, lapply(names(toplot), function(t) data.frame(x = toplot[[t]]$x, y = t)))
            fit <- lm(x ~ y, data = dat)
            
            fstat <- summary(fit)$fstatistic
            
            Ftest <- c("Overall F-test", "",
                       paste0("F: ", signif(fstat[1], 5), "   ", 
                              "df: ", fstat[2], " and ", fstat[3], "   ",
                              "p-value: ",
                              format.pval(pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE),
                                          digits = 2)))
            
            out <- c(out, "", Ftest, "", #centerText("* * *", width),
                     "", "   *** Differences between Group Means (column - row) ***", "")
            
            means <- predict(fit, newdata = data.frame(y = levels(dat$y)))
            names(means) <- levels(dat$y)
            diffMat <- outer(means, means, function(x, y) y - x)
            diffMat <- formatTriMat(diffMat, names(means))
            
            
            
            out <- c(out,
                     "Estimates", "",
                     apply(diffMat, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
        }
    }
    
    out
}

formatTriMat <- function(mat, names) {
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
}



inference.inzbar <- function(object, ...) {

    NULL
}


inference.inzscatter <- function(object, ...) {

    NULL
}
