summary.inzdot <- function(object, des, ...) {
    ## Generate summary information:
    
    ## Produce a matrix of the required summary:
    toplot <- object$toplot


    if (is.null(des)) {    
        do.call(rbind, lapply(names(toplot), function(p) {
            x <- toplot[[p]]$x
            
            s <- suppressWarnings(
                c(min(x),
                  quantile(x, 0.25),
                  quantile(x, 0.5),
                  quantile(x, 0.75),
                  max(x),
                  mean(x), sd(x), length(x))
                )
            s[!is.finite(s)] <- NA
            s
        })) -> mat
    } else {
        dv <- des$variables
        if ("y" %in% colnames(dv))
            mat <- cbind(tapply(dv$x, dv$y, min),
                         svyby(~x, ~y, des, svyquantile, quantiles = c(0.25, 0.5, 0.75), keep.var = FALSE)[, -1],
                         tapply(dv$x, dv$y, max),
                         svyby(~x, ~y, des, svymean)[, "x"],
                         sqrt(svyby(~x, ~y, des, svyvar)[, "x"]),
                         as.vector(table(dv$y)),
                         svyby(~x, ~y, des, svytotal)[, "x"])
        else
            mat <- cbind(min(dv$x, na.rm = TRUE),
                         svyquantile(~x, des, quantiles = c(0.25, 0.5, 0.75)),
                         max(dv$x),
                         svymean(~x, des)[["x"]],
                         sqrt(svyvar(~x, des)[["x"]]),
                         nrow(dv),
                         svytotal(~x, des)[["x"]])
    }
    
    mat <- matrix(apply(mat, 2, function(col) {
        format(col, digits = 4)
    }), nrow = nrow(mat))

    ## Remove NA's and replace with an empty space
    mat[grep("NA", mat)] <- ""

    ## Text formatting to return a character vector - each row of matrix
    rns <- c("Min", "25%", "Median", "75%", "Max", "Mean", "SD", "Sample Size")
    if (!is.null(des)) rns <- c(rns, "Population Size")
    mat <- rbind(rns,  mat)
    colnames(mat) <- NULL

    if (length(toplot) > 1) {
        mat <- cbind(c("", names(toplot)), mat)
    }
    rownames(mat) <- NULL
    
    mat <- matrix(apply(mat, 2, function(col) {
        format(col, justify = "right")
    }), nrow = nrow(mat))

    mat <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
    mat
}
summary.inzhist <- function(object, des, ...)
    summary.inzdot(object, des, ...)



summary.inzbar <- function(object, des, ...) {
    tab <- round(object$tab)
    perc <- object$phat * 100

    is.survey <- !is.null(des)

    twoway <- length(dim(tab)) == 2
    if (twoway) {
        tab <- as.matrix(tab)
        
        perc <- as.matrix(perc)
        perc <- t(apply(perc, 1, function(p) {
            if (all(is.finite(p)))
                paste0(c(format(p, digits = 3), "100"), "%")
            else
                rep("", length(p) + 1)
            }))

        mat1 <- rbind(c(colnames(tab), "Row Total"),
                      cbind(tab, rowSums(tab)))
        mat1 <- cbind(c("", rownames(tab)), mat1)

        mat1 <- matrix(apply(mat1, 2, function(col) {
            format(col, justify = "right")
        }), nrow = nrow(mat1))
        

        mat2 <- rbind(c(colnames(tab), "Total", "Row N"),
                      cbind(perc, rowSums(tab)))
        mat2 <- cbind(c("", rownames(tab)), mat2)

        mat2 <- matrix(apply(mat2, 2, function(col) {
            format(col, justify = "right")
        }), nrow = nrow(mat2))

        c("Table of Counts:", "",
          apply(mat1, 1, function(x) paste0("   ", paste(x, collapse = "   "))),
          "",
          "Table of Percentages:", "",
          apply(mat2, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
        
    } else {
        mat <- rbind(c(names(tab), "Total"),
                     c(tab, sum(tab)),
                     paste0(c(format(perc, digits = 4), "100"), "%"))

        mat <- cbind(c("", "Count", "Percent"), mat)

        mat <- matrix(apply(mat, 2, function(col) {
            format(col, justify = "right")
        }), nrow = nrow(mat))
        
        mat <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
        mat
    }
}



summary.inzscatter <- function(object, vn, des, ...) {
    x <- object$x
    y <- object$y
    trend <- object$trend

    is.survey <- !is.null(des)

    out <- character()
    if ("linear" %in% trend) {
        beta <- try({
            if (is.survey)
                signif(coef(svyglm(y ~ x, design = des)), 4)
            else
                signif(coef(lm(y ~ x)), 4)
        }, silent = TRUE)

        if (inherits(beta, "try-error"))
            out <- "Unable to fit linear trend."
        else
            out <- c(out,
                     "Linear trend:", "",
                     paste0("    ",
                            vn$y, " = ",
                            beta[1], " + ",
                            beta[2], " * ", vn$x),
                     paste0("    Linear correlation: ",
                            if (is.survey)
                                round(cov2cor(as.matrix(svyvar(y~x, design = des)))[1,2], 2)
                            else
                                round(cor(x, y), 2)),
                     "")
    }
    if ("quadratic" %in% trend) {
        beta <- try({
            if (is.survey)
                signif(coef(svyglm(y ~ x + I(x^2), design = des)), 4)
            else
                signif(coef(lm(y ~ x + I(x^2))), 4)
        }, silent = TRUE)
        
        if (inherits(beta, "try-error"))
            out <- "Unable to fit quadratic trend."
        else
            out <- c(out,
                     "Quadratic trend:", "",
                     paste0("    ",
                            vn$y, " = ",
                            beta[1], " + ",
                            beta[2], " * ", vn$x, " + ",
                            beta[3], " * ", vn$x, "^2"), "")
    }
    if ("cubic" %in% trend) {
        beta <- beta <- try({
            if (is.survey)
                signif(coef(svyglm(y ~ x + I(x^2) + I(x^3), design = des)), 4)
            else
                signif(coef(lm(y ~ x + I(x^2) + I(x^3))), 4)
        }, silent = TRUE)

        if (inherits(beta, "try-error"))
            out <- "Unable to fit linear trend."
        else
            out <- c(out,
                     "Cubic trend:", "",
                     paste0("    ",
                            vn$y, " = ",
                            beta[1], " + ",
                            beta[2], " * ", vn$x, " + ",
                            beta[3], " * ", vn$x, "^2 + ",
                            beta[4], " * ", vn$x, "^3"), "")
    }
    
    rank.cor <- cor(x, y, method = "spearman")
    out <- c(out,
             paste0("Rank correlation: ", sprintf("%.2f", rank.cor),
                    "  (using Spearman's Rank Correlation)"))
      
}
summary.inzgrid <- function(object, vn, des, ...)
    summary.inzscatter(object, vn, des, ...)

summary.inzhex <- function(object, vn, des, ...)
    summary.inzscatter(object, vn, des, ...)
