summary.inzdot <- function(object, des, ...) {
    ## Generate summary information:

    ## Produce a matrix of the required summary:
    toplot <- object$toplot


    if (is.null(des)) {
        smrytype <- "numeric"

        if (!is.null(object$boxinfo[[1]]$opts$transform) &&
            !is.null(object$boxinfo[[1]]$opts$transform$x)) {
            smrytype <- object$boxinfo[[1]]$opts$transform$x
        }

        mat <- do.call(rbind,
            lapply(names(toplot),
                function(p) {
                    x <- toplot[[p]]$x
                    s <- suppressWarnings(
                        switch(smrytype,
                            "datetime" = {
                                xdt <- as.POSIXct(x,
                                    origin = "1970-01-01",
                                    tz = object$boxinfo[[p]]$opts$transform$extra$x$tz
                                )
                                c(
                                    as.character(min(xdt, na.rm = TRUE)),
                                    as.character(max(xdt, na.rm = TRUE)),
                                    as.character(lubridate::seconds_to_period(
                                        diff(range(x, na.rm = TRUE))
                                    )),
                                    length(x)
                                )
                            },
                            "date" = {
                                xd <- as.Date(x, origin = "1970-01-01")
                                c(
                                    as.character(min(xd, na.rm = TRUE)),
                                    as.character(max(xd, na.rm = TRUE)),
                                    sprintf("%i days",
                                        as.integer(diff(range(xd, na.rm = TRUE)))
                                    ),
                                    length(x)
                                )
                            },
                            "time" = {
                                xt <- chron::chron(times. = x)
                                c(
                                    as.character(min(xt, na.rm = TRUE)),
                                    as.character(max(xt, na.rm = TRUE)),
                                    length(x)
                                )
                            },
                            {
                                zz <- c(
                                    min(x),
                                    quantile(x, 0.25),
                                    quantile(x, 0.5),
                                    quantile(x, 0.75),
                                    max(x),
                                    mean(x), sd(x), length(x)
                                )
                                zz[!is.finite(zz)] <- NA
                                zz
                            }
                        )
                    )
                    s
                }
            )
        )
        rns <- switch(smrytype,
            "date" = c("Start Date", "End Date", "Date Range", "Sample Size"),
            "time" = c("Earliest Time", "Latest Time", "Sample Size"),
            "datetime" = c("Start Time", "End Time", "Time Range", "Sample Size"),
            c("Min", "25%", "Median", "75%", "Max", "Mean", "SD", "Sample Size")
        )
    } else {
        dv <- des$variables
        ones <- cbind(rep(1, nrow(dv)))
        if ("y" %in% colnames(dv)) {
            mat <- cbind(
                svyby(~x, ~y, des, svyquantile,
                    quantiles = c(0.25, 0.5, 0.75),
                    na.rm = TRUE,
                    keep.var = FALSE,
                    drop.empty.groups = FALSE
                )[, -1],
                coef(
                    svyby(~x, ~y, des, svymean,
                        na.rm = TRUE,
                        drop.empty.groups = FALSE
                    )
                ),
                sqrt(
                    coef(
                        svyby(~x, ~y, des, svyvar,
                            na.rm = TRUE,
                            drop.empty.groups = FALSE
                        )
                    )
                ),
                coef(
                    svyby(~x, ~y, des, svytotal,
                        na.rm = TRUE,
                        drop.empty.groups = FALSE
                    )
                ),
                coef(svytotal(~y, des)),
                NaN,
                as.vector(table(dv$y)),
                tapply(dv$x, dv$y, min, na.rm = TRUE),
                tapply(dv$x, dv$y, max, na.rm = TRUE)
            )

            semat <- cbind(
                suppressWarnings(
                    SE(
                        svyby(~x, ~y, des, svyquantile,
                            quantiles = c(0.25, 0.5, 0.75),
                            ci = TRUE,
                            se = TRUE,
                            na.rm = TRUE,
                            drop.empty.groups = FALSE
                        )
                    )
                ),
                SE(
                    svyby(~x, ~y, des, svymean,
                        na.rm = TRUE,
                        drop.empty.groups = FALSE
                    )
                ),
                {
                    vv <- svyby(~x, ~y, des, svyvar,
                        na.rm = TRUE,
                        drop.empty.groups = FALSE
                    )
                    vc <- suppressWarnings(diag(vcov(vv)))
                    sqrt(vc / 4 / coef(vv))
                },
                SE(
                    svyby(~x, ~y, des, svytotal,
                        na.rm = TRUE,
                        drop.empty.groups = FALSE
                    )
                ),
                SE(svytotal(~y, des)),
                NA,
                NA,
                NA,
                NA
            )

            dimnames(semat) <- dimnames(mat)
            mat <- rbind(mat, semat)
        } else {
            mat <- cbind(
                if (is_svyrep(des)) {
                    t(
                        rbind(
                            coef(
                                svyquantile(~x, des,
                                    na.rm = TRUE,
                                    quantiles = c(0.25, 0.5, 0.75)
                                )
                            )
                        )
                    )
                } else {
                    svyquantile(~x, des,
                        na.rm = TRUE,
                        quantiles = c(0.25, 0.5, 0.75)
                    )
                },
                coef(
                    svymean(~x, des, na.rm = TRUE)
                ),
                sqrt(
                    coef(
                        svyvar(~x, des, na.rm = TRUE)
                    )
                ),
                coef(svytotal(~x, des, na.rm = TRUE)),
                coef(svytotal(ones, des, na.rm = TRUE)),
                NaN,
                nrow(dv),
                min(dv$x, na.rm = TRUE),
                max(dv$x, na.rm = TRUE)
            )

            semat <- cbind(
                if (is_svyrep(des)) {
                    t(
                        rbind(
                            SE(
                                svyquantile(~x, des,
                                    quantiles = c(0.25, 0.5, 0.75),
                                    a.rm = TRUE,
                                    se = TRUE
                                )
                            )
                        )
                    )
                } else {
                    rbind(
                        SE(
                            svyquantile(~x, des,
                                quantiles = c(0.25, 0.5, 0.75),
                                na.rm = TRUE,
                                se = TRUE
                            )
                        )
                    )
                },
                SE(svymean(~x, des, na.rm = TRUE)),
                {
                    vv <- svyvar(~x, des, na.rm = TRUE)
                    sqrt(vcov(vv) / 4 / coef(vv))
                },
                SE(svytotal(~x, des, na.rm = TRUE)),
                SE(svytotal(ones, des, na.rm = TRUE)),
                NA,
                NA,
                NA,
                NA
            )

            mat <- rbind(mat, semat)
        }
        rns <- c(
            "25%", "Median", "75%", "Mean", "SD", "Total", "Est. Pop. Size",
            "|", "Sample Size", "Min", "Max"
        )
        if (!all(get_weights(des) == 0 | get_weights(des) >= 1)) {
            mat <- mat[, -(6:7)]
            rns <- rns[-(6:7)]
        }
    }

    mat <- matrix(
        apply(mat, 2,
            function(col) {
                format(col, digits = 4)
            }
        ),
        nrow = nrow(mat)
    )

    ## Remove NA's and replace with an empty space
    mat[grep("NA", mat)] <- ""
    mat <- gsub("NaN", "|", mat)

    ## Text formatting to return a character vector - each row of matrix
    mat <- rbind(rns,  mat)
    colnames(mat) <- NULL


    if (length(toplot) > 1) {
        mat <- cbind(
            c(
                "",
                rep(
                    names(toplot),
                    ifelse(exists("semat"), 2, 1)
                )
            ),
            mat
        )
    }
    rownames(mat) <- NULL

    mat <- matrix(
        apply(mat, 2,
            function(col) {
                format(col, justify = "right")
            }
        ),
        nrow = nrow(mat)
    )

    mat <- apply(mat, 1,
        function(x) paste0("   ", paste(x, collapse = "   "))
    )

    if (exists("semat")) {
        top <- 1:((length(mat)-1)/2+1)
        bot <- ((length(mat)-1)/2+2):length(mat)
        out <- c(
            "Population estimates:",
            "",
            mat[top],
            "",
            "Standard error of estimates:",
            "",
            mat[bot]
        )
    } else {
        out <- c("Population estimates:", "", mat)
    }

    out
}

summary.inzhist <- function(object, des, ...)
    summary.inzdot(object, des, ...)


summary.inzbar <- function(object, des, ...) {
    tab <- round(object$tab)
    perc <- object$phat * 100

    is.survey <- !is.null(des)

    # survey tables do this thing where they retain their dimensions,
    # even for one-way tables
    twoway <- length(dim(tab)) == 2 && nrow(tab) > 1
    if (twoway) {
        tab <- as.matrix(tab)

        perc <- as.matrix(perc)
        perc <- t(
            apply(perc, 1,
                function(p) {
                    if (all(is.finite(p)))
                        paste0(c(format(p, digits = 3), "100"), "%")
                    else
                        rep("", length(p) + 1)
                }
            )
        )

        mat1 <- rbind(
            c(colnames(tab), "Row Total"),
            cbind(tab, rowSums(tab))
        )
        mat1 <- cbind(c("", rownames(tab)), mat1)

        mat1 <- matrix(
            apply(mat1, 2,
                function(col) {
                    format(col, justify = "right")
                }
            ),
            nrow = nrow(mat1)
        )


        mat2 <- rbind(
            c(colnames(tab), "Total", "Row N"),
            cbind(perc, rowSums(tab))
        )
        mat2 <- cbind(c("", rownames(tab)), mat2)

        mat2 <- matrix(
            apply(mat2, 2,
                function(col) {
                    format(col, justify = "right")
                }
            ),
            nrow = nrow(mat2)
        )

        out <- c(
            sprintf("Table of %sCounts:",
                ifelse(is.survey, "Estimated Population ", "")
            ),
            "",
            apply(mat1, 1,
                function(x) paste0("   ", paste(x, collapse = "   "))
            ),
            "",
            sprintf("Table of %sPercentages:",
                ifelse(is.survey, "Estimated Population ", "")
            ),
            "",
            apply(mat2, 1,
                function(x) paste0("   ", paste(x, collapse = "   "))
            )
        )

        if (is.survey) {
            mat <- format(
                SE(
                    svyby(~x, ~y, des, svymean,
                        drop.empty.groups = FALSE
                    )
                ) * 100,
                digits = 4
            )
            mat <- cbind(
                c("", rownames(tab)),
                rbind(colnames(tab), mat)
            )
            mat <- matrix(
                apply(mat, 2,
                    function(col) {
                        format(col, justify = "right")
                    }
                ),
                nrow = nrow(mat)
            )
            mat[grep("NA", mat)] <- ""
            mat <- apply(mat, 1,
                function(x) paste0("   ", paste(x, collapse = "   "))
            )
            out <- c(
                out,
                "",
                "Standard error of estimated percentages:",
                "",
                mat
            )
        }
        return(out)
    } else {
        mat <- rbind(
            c(colnames(tab), "Total"),
            c(tab, sum(tab)),
            paste0(
                c(format(round(perc, 2), nsmall = 2), "100"),
                "%"
            )
        )

        mat <- cbind(c("", "Count ", "Percent "), mat)
        if (is.survey) {
            mat <- rbind(
                mat[1:2, ],
                "",
                mat[3, ],
                c(
                    "  std err",
                    paste0(
                        format(round(SE(svymean(~x, des)) * 100, 2),
                            nsmall = 2
                        ),
                        "%"
                    ),
                    NA
                )
            )
        }

        mat <- matrix(
            apply(mat, 2,
                function(col) {
                    format(col, justify = "right")
                }
            ),
            nrow = nrow(mat)
        )

        mat[grep("NA", mat)] <- ""

        mat <- apply(mat, 1,
            function(x) paste0("   ", paste(x, collapse = "   "))
        )


        if (is.survey) {
            return(c("Population Estimates:", "", mat))
        } else {
            return(mat)
        }
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
            out <- c(
                out,
                "Linear trend:", "",
                paste0(
                    "    ",
                    vn$y,
                    " = ",
                    beta[1],
                    " + ",
                    beta[2],
                    " * ",
                    vn$x
                ),
                paste0(
                    "    Linear correlation: ",
                    if (is.survey)
                        round(
                            cov2cor(as.matrix(svyvar(y ~ x, design = des)))[1, 2],
                            2
                        )
                    else
                        round(cor(x, y), 2)
                ),
                ""
            )
    }
    if ("quadratic" %in% trend) {
        beta <- try(
            {
                if (is.survey)
                    signif(
                        coef(svyglm(y ~ x + I(x^2), design = des)),
                        4
                    )
                else
                    signif(
                        coef(lm(y ~ x + I(x^2))),
                        4
                    )
            },
            silent = TRUE
        )

        if (inherits(beta, "try-error"))
            out <- "Unable to fit quadratic trend."
        else
            out <- c(
                out,
                "Quadratic trend:",
                "",
                paste0(
                    "    ",
                    vn$y,
                    " = ",
                    beta[1],
                    " + ",
                    beta[2],
                    " * ",
                    vn$x,
                    " + ",
                    beta[3],
                    " * ",
                    vn$x,
                    "^2"
                ),
                ""
            )
    }
    if ("cubic" %in% trend) {
        beta <- beta <- try(
            {
                if (is.survey)
                    signif(
                        coef(svyglm(y ~ x + I(x^2) + I(x^3), design = des)),
                        4
                    )
                else
                    signif(
                        coef(lm(y ~ x + I(x^2) + I(x^3))),
                        4
                    )
            },
            silent = TRUE
        )

        if (inherits(beta, "try-error"))
            out <- "Unable to fit linear trend."
        else
            out <- c(
                out,
                "Cubic trend:", "",
                paste0(
                    "    ",
                    vn$y,
                    " = ",
                    beta[1],
                    " + ",
                    beta[2],
                    " * ",
                    vn$x,
                    " + ",
                    beta[3],
                    " * ",
                    vn$x,
                    "^2 + ",
                    beta[4],
                    " * ",
                    vn$x,
                    "^3"
                ),
                ""
            )
    }

    if (is.survey) {
        ## rank.cor <- cov2cor(coef(svyvar(rank(y) ~ rank(x), design = des)))[1,2]
        if (!"linear" %in% trend) {
            cor <- round(
                cov2cor(as.matrix(svyvar(y~x, design = des)))[1,2],
                2
            )
            out <- c(
                out,
                paste0(
                    "Correlation: ",
                    sprintf("%.2f", cor),
                    "  (using Pearson's Correlation)"
                )
            )
        }
    } else {
        rank.cor <- cor(x, y, method = "spearman")
        out <- c(
            out,
            paste0(
                "Rank correlation: ",
                sprintf("%.2f", rank.cor),
                "  (using Spearman's Rank Correlation)"
            )
        )
    }

    out
}
summary.inzgrid <- function(object, vn, des, ...)
    summary.inzscatter(object, vn, des, ...)

summary.inzhex <- function(object, vn, des, ...)
    summary.inzscatter(object, vn, des, ...)
