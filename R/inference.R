inference <- function(object, ...)
    UseMethod("inference")


inference.inzdot <- function(object, des, bs, class, width, vn, hypothesis, ...) {
    toplot <- object$toplot
    inf <- object$inference.info

    if (is.character(inf))
        return("Sample too small to get inference")

    if (is.null(inf[[1]]$conf)) {
        warning("Please specify `inference.type = conf` to get inference information.")
        return("No inference information available. Sample size too small?")
    }

    is.survey <- !is.null(des)

    mat <- inf$mean$conf[, c("lower", "mean", "upper"), drop = FALSE]

    mat <- matrix(apply(mat, 2, function(col) {
        format(col, digits = 4)
    }), nrow = nrow(mat))

    ## Remove NA's and replace with an empty space
    mat[grep("NA", mat)] <- ""

    ## Text formatting to return a character vector - each row of matrix
    mat <- rbind(c("Lower", "Mean", "Upper"), mat)
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
                           ifelse(is.survey, "Population Means", "Group Means"),
                           ifelse(is.survey, "Population Mean", "Mean")),
                    " with 95%", bsCI, " Confidence Interval", plural), "",
             out)

    if (bs) {
        if (is.survey) return("Bootstrap inference not yet implemented for survey data.")

        ## BOOTSTRAP MEDIAN
        mat <- inf$median$conf[, c("lower", "mean", "upper"), drop = FALSE]

        mat <- matrix(apply(mat, 2, function(col) {
            format(col, digits = 4)
        }), nrow = nrow(mat))

        ## Remove NA's and replace with an empty space
        mat[grep("NA", mat)] <- ""

        ## Text formatting to return a character vector - each row of matrix
        mat <- rbind(c("Lower", "Median", "Upper"), mat)
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
        mat <- rbind(c("Lower", "IQR", "Upper"), mat)
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
        if (length(toplot) == 2) {
            ## Two sample t-test

            if (is.survey) {
                fit <- try(svyglm(if (is.numeric(des$variables$x)) x ~ y else y ~ x, design = des), TRUE)
                if (inherits(fit, "try-error")) {
                    mat <- rbind(c("Lower", "Mean", "Upper"),
                                 rep(NA, 3))
                } else {
                    ci <- confint(fit)
                    mat <- rbind(c("Lower", "Mean", "Upper"),
                                 format(c(ci[2,1], coef(fit)[2], ci[2, 2]), digits = 4))
                    colnames(mat) <- NULL
                }
            } else {
                ttest <- t.test(toplot[[1]]$x, toplot[[2]]$x,
                    var.equal = hypothesis$var.equal)
                mat <- rbind(c("Lower", "Mean", "Upper"),
                         format(c(ttest$conf.int[1],
                                  diff(rev(ttest$estimate)),
                                  ttest$conf.int[2]),
                                digits = 4))
                colnames(mat) <- NULL
            }

            if (any(is.na(mat))) {
                out <- c(out, "")
            } else {
                mat <- cbind(c("", paste0(names(toplot)[1], " - ", names(toplot)[2])), mat)

                mat <- matrix(apply(mat, 2, function(col) {
                    format(col, justify = "right")
                }), nrow = nrow(mat))

                mat <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))

                out <- c(out, "",
                         sprintf("Difference in %s means with 95%s Confidence Interval",
                                 ifelse(is.survey, "population", "group"), "%"),
                         "", mat)
            }
        }

        if (!is.null(hypothesis)) {
            if (length(toplot) == 2 && hypothesis$test %in% c("default", "t.test")) {
                if (is.survey) {
                    test.out <- try(svyttest(if (is.numeric(des$variables$x)) x ~ y else y ~ x, des), TRUE)
                } else {
                    test.out <- t.test(toplot[[1]]$x, toplot[[2]]$x,
                                       mu = hypothesis$value, alternative = hypothesis$alternative,
                                       var.equal = hypothesis$var.equal)
                }

                if (!inherits(test.out, "try-error")) {
                    pval <- format.pval(test.out$p.value)
                    out <- c(out, "",
                             ifelse(is.survey, "Design-based Two Sample T-test",
                                    paste0(ifelse(hypothesis$var.equal, "", "Welch "), "Two Sample t-test",
                                           ifelse(hypothesis$var.equal, " assuming equal variance", ""))),
                             "",
                             paste0("   t = ", format(test.out$statistic, digits = 5),
                                    ", df = ", format(test.out$parameter, digits = 5),
                                    ", p-value ", ifelse(substr(pval, 1, 1) == "<", "", "= "), pval),
                             "",
                             paste0("          Null Hypothesis: true difference in means is equal to ", test.out$null.value),
                             paste0("   Alternative Hypothesis: true difference in means is ",
                                    ifelse(test.out$alternative == "two.sided", "not equal to",
                                           paste0(test.out$alternative, " than")), " ", test.out$null.value)
                             )
                    if (!is.survey && hypothesis$var.equal) {
                        ## F test for equal variance
                        ftest <- var.test(toplot[[1]]$x, toplot[[2]]$x)

                        svar <- sapply(toplot, function(d) var(d$x))
                        sn <- sapply(toplot, function(d) length(d$x))
                        var.pooled <- sum((sn - 1) * svar) / sum(sn - 1)
                        pval <- format.pval(ftest$p.value)
                        out <- c(out, "",
                                 paste0("          Pooled Variance: ", format(var.pooled, digits = 5)), "",
                                 "F-test for equal variance [NOTE: very sensitive to non-normality]",
                                 "",
                                 paste0("   F = ", format(ftest$statistic, digits = 5),
                                        ", df = ", paste(format(ftest$parameter, digits = 5), collapse = " and "),
                                        ", p-value ", ifelse(substr(pval, 1, 1) == "<", "", "= "), pval))
                    }
                }
            }
        }

        dat <- do.call(rbind, lapply(names(toplot),
                                     function(t) {
                                         if (is.null(toplot[[t]]$x))
                                             NULL
                                         else
                                             data.frame(x = toplot[[t]]$x, y = t)
                                     }
                                     ))
        if (is.survey) {
            fit <- try(svyglm(x ~ y, des), TRUE)
        } else {
            fit <- lm(x ~ y, data = dat)
        }

        if (!is.survey && !is.null(hypothesis) && (length(toplot) > 2 | hypothesis$test == "anova")) {
            fstat <- summary(fit)$fstatistic

            fpval <- format.pval(pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE),
                                 digits = 5)
            Ftest <- c("One-way Analysis of Variance (ANOVA F-test)", "",
                       paste0("   F = ", signif(fstat[1], 5),
                              ", df = ", fstat[2], " and ", fstat[3],
                              ", p-value ", ifelse(substr(fpval, 1, 1) == "<", "", "= "), fpval))
            out <- c(out, "", Ftest, "",
                     "          Null Hypothesis: true group means are equal",
                     "   Alternative Hypothesis: true group means are not equal")
        }

        if (length(toplot) > 2) {
            ## For x ~ factor, we also include an F test, and multiple comparisons
            ## (estimates, pvalues, and confidence intervals).

            out <- c(out, "", "",
                     sprintf("### Difference in mean %s between %s groups", vn$x, vn$y),
                     "    (col group - row group)", "")

            means <- predict(fit, newdata = data.frame(y = levels(dat$y)))
            names(means) <- LEVELS <- levels(dat$y)
            diffMat <- outer(means, means, function(x, y) y - x)
            diffMat <- formatTriMat(diffMat, LEVELS)

            out <- c(out,
                     "Estimates", "",
                     apply(diffMat, 1, function(x) paste0("   ", paste(x, collapse = "   "))))

            if (!is.survey) {
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
        }

    } else if (!is.null(hypothesis) & !bs) {
        ## hypothesis testing - one sample
        if (is.survey) {
            des <- update(des, .h0 = x - hypothesis$value)
            test.out <- svyttest(.h0 ~ 0, des)
        } else {
            test.out <- t.test(toplot$all$x,
                               alternative = hypothesis$alternative,
                               mu = hypothesis$value)
        }
        pval <- format.pval(test.out$p.value)
        out <- c(out, "",
                 sprintf("%sOne Sample t-test", ifelse(is.survey, "Design-based ", "")),
                 "",
                 paste0("   t = ", format(test.out$statistic, digits = 5),
                        ", df = ", test.out$parameter,
                        ", p-value ", ifelse(substr(pval, 1, 1) == "<", "", "= "), pval),
                 "",
                 paste0("          Null Hypothesis: true mean is equal to ", hypothesis$value),
                 paste0("   Alternative Hypothesis: true mean is ",
                        ifelse(test.out$alternative == "two.sided", "not equal to",
                               paste0(test.out$alternative, " than")), " ", hypothesis$value)
                 )
    }



    out
}

formatTriMat <- function(mat, names) {
    ## Formats a (lower) triangular matrix nicely for display:

    mat[!lower.tri(mat)] <- NA
    mat <- mat[-1, , drop = FALSE]

    mat <- matrix(apply(mat, 2, function(col) {
        format(col, digits = 4)
    }), nrow = nrow(mat))

    mat[grep("NA", mat)] <- ""
    mat[grep("NaN", mat)] <- ""

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

    mat <- apply(mat, 1, function(x) suppressWarnings(as.numeric(x)))
    ## If the matrix has a single column, apply returns a vector rather than a matrix
    if (is.matrix(mat))
        mat <- t(mat)
    else
        mat <- matrix(mat, ncol = 1)

    mat <-  matrix(apply(mat, 2, function(col) {
        format(col, digits = digits)
    }), nrow = nrow(mat))
    mat[grep("NA", mat)] <- ""
    mat[grep("NaN", mat)] <- ""

    mat <- cbind(c("", dn[[1]]),
                 rbind(dn[[2]], mat))

    mat <- matrix(apply(mat, 2, function(col) {
        format(col, justify = "right")
    }), nrow = nrow(mat))

    mat
}
inference.inzhist <- function(object, des, bs, class, width, vn, hypothesis, ...)
    inference.inzdot(object, des, bs, class, width, vn, hypothesis, ...)



inference.inzbar <- function(object, des, bs, nb, vn, hypothesis, ...) {
    phat <- object$phat
    inf <- object$inference.info
    is.survey <- !is.null(des)

    if (! "conf" %in% names(inf))
        stop("Please specify `inference.type = conf` to get inference information.")

    if (is.null(inf$conf))
        return("Unable to obtain inference information.")

    if (bs & sum(object$tab) < 10)
        return("Not enough data to perform bootstraps.")

    twoway <- nrow(phat) > 1
    if (is.survey && !twoway) hypothesis <- NULL

    if (!is.null(hypothesis) && !bs) {
        HypOut <- switch(hypothesis$test,
            "proportion" = {
                if (is.survey) {
                    HypOut <- NULL
                } else if (twoway) {
                    HypOut <- NULL
                } else {
                    if (ncol(object$tab) == 2) {
                        if (hypothesis$use.exact) {
                            prtest <- binom.test(object$tab,
                                p = hypothesis$value,
                                alternative = hypothesis$alternative
                            )
                        } else {
                            n <- sum(object$tab)
                            phat <- object$tab[1] / n
                            p <- hypothesis$value
                            Z <- (phat - p) / sqrt(p * (1 - p) / n)
                            prtest <- list(
                                statistic = Z,
                                p.value = switch(hypothesis$alternative,
                                    "two.sided" = pnorm(abs(Z), lower.tail = FALSE),
                                    "less" = pnorm(Z, lower.tail = TRUE),
                                    "greater" = pnorm(Z, lower.tail = FALSE)
                                )
                            )
                        }

                        HypOut <- c(
                            sprintf("%s",
                                ifelse(hypothesis$use.exact,
                                    "Exact binomial test",
                                    "One-sample test of a proportion"
                                )
                            ),
                            "",
                            sprintf("   %s, p-value %s%s",
                                ifelse(hypothesis$use.exact, 
                                    sprintf(
                                        "Number of successes = %s, number of trials = %s",
                                        prtest$statistic, prtest$parameter
                                    ),
                                    sprintf("Z-score = %s",
                                        format(signif(prtest$statistic, 5))
                                    )
                                ),
                                ifelse(prtest$p.value < 2.2e-16, "", "= "), 
                                format.pval(prtest$p.value, digits = 5)
                            ),
                            "",
                            sprintf("          Null Hypothesis: %s",
                                sprintf("true proportion of %s = %s is %s",
                                    vn$x, colnames(object$tab)[1], 
                                    hypothesis$value
                                )
                            ),
                            sprintf("   Alternative Hypothesis: %s", 
                                sprintf("true proportion of %s = %s is %s %s",
                                    vn$x, colnames(object$tab)[1],
                                    ifelse(hypothesis$alternative == "two.sided",
                                        "not equal to",
                                        paste(hypothesis$alternative, "than")
                                    ),
                                    hypothesis$value
                                )
                            ), 
                            ""
                        )

                    } else {
                        HypOut <- NULL
                    }
                }
                HypOut
            },
            "chi2" = ,
            "default" = {
                chi2sim <- NULL
                if (is.survey) {
                    chi2 <- try(svychisq(~y+x, des), TRUE)
                } else {
                    chi2 <- suppressWarnings(chisq.test(object$tab))
                    # from experimenting, tables bigger than this 
                    # start taking too long to simulate the p-value
                    if (prod(dim(object$tab)) > 2000) 
                        hypothesis$simulated.p.value <- FALSE
                    if (any(chi2$expected < 5) || hypothesis$simulated.p.value) {
                        chi2 <- suppressWarnings(
                            chisq.test(object$tab, correct = FALSE)
                        )
                        chi2sim <- suppressWarnings(
                            chisq.test(object$tab, simulate.p.value = TRUE)
                        )
                    }
                }

                if (inherits(chi2, "try-error")) {
                    HypOut <- NULL
                } else {
                    piece1 <- ifelse(twoway,
                                     sprintf("distribution of %s does not depend on %s", vn$x, vn$y),
                                     "true proportions in each category are equal")
                    piece2 <- ifelse(twoway,
                                     sprintf("distribution of %s changes with %s", vn$x, vn$y),
                                     "true proportions in each category are not equal")

                    chi2out <- ""
                    simpval <- ""
                    if (!is.null(chi2sim) && any(chi2$expected < 5)) {
                        chi2out <- " (since some expected counts < 5)"
                    }
                    if (!is.null(chi2sim)) {
                        simpval <- sprintf("\n   Simulated p-value%s %s%s",
                            chi2out,
                            ifelse(chi2sim$p.value < 2.2e-16, "", "= "),
                            format.pval(chi2sim$p.value, digits = 5)
                        )
                    }

                    HypOut <- c(
                        sprintf("Chi-square test for equal %s",
                            ifelse(twoway, "distributions", "proportions")
                        ),
                        "",
                        paste0("   X^2 = ", format(signif(chi2$statistic, 5)), ", ",
                            "df = ", format(signif(chi2$parameter, 5)), ", ",
                            "p-value ", ifelse(chi2$p.value < 2.2e-16, "", "= "), 
                                format.pval(chi2$p.value, digits = 5),
                            simpval
                        ), 
                        "",
                        sprintf("          Null Hypothesis: %s", piece1),
                        sprintf("   Alternative Hypothesis: %s", piece2), 
                        ""
                    )

                }
                HypOut
            }
        )
    } else {
        HypOut <- NULL
    }

    if (twoway) {
        mat <- inf$conf$estimate
        dn <- dimnames(object$tab)

        mat <- matrix(apply(mat, 2, function(col) {
            format(col, digits = 3)
        }), nrow = nrow(mat))

        ## Remove NA's and replace with an empty space
        mat[grep("NA", mat)] <- ""
        mat[grep("NaN", mat)] <- ""

        ## Text formatting to return a character vector - each row of matrix
        mat <- rbind(dn[[2]], mat)
        colnames(mat) <- NULL

        mat <- cbind(c("", dn[[1]]), mat, c("Row sums", rep(1, nrow(phat))))
        rownames(mat) <- NULL

        mat <- matrix(apply(mat, 2, function(col) {
            format(col, justify = "right")
        }), nrow = nrow(mat))

        out <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
        out <- c("Estimated Proportions", "",  out)

        cis <- inf$conf
        cis <- rbind(cis$lower, cis$upper)
        cis <- cis[rep(1:nrow(phat), each = 2) + c(0, nrow(phat)), ]

        cis <- matrix(apply(cis, 2, function(col) {
            format(col, digits = 3)
        }), nrow = nrow(cis))
        cis[grep("NA", cis)] <- ""
        cis[grep("NaN", cis)] <- ""

        cis <- rbind(dn[[2]], cis)
        colnames(cis) <- NULL

        cis <- cbind(c("", rbind(dn[[1]], "")), cis)
        colnames(cis) <- NULL

        cis <- matrix(apply(cis, 2, function(col) {
            format(col, justify = "right")
        }), nrow = nrow(cis))

        bsCI <- ifelse(bs, " Percentile Bootstrap", "")
        out <- c(out, "", paste0("95%", bsCI, " Confidence Intervals"), "",
                 apply(cis, 1, function(x) paste0("   ", paste(x, collapse = "   "))))

        out <- c(out, "", HypOut, "",
                 paste0("### Differences in proportions of ", vn$y, " with the specified ", vn$x))

        if (bs) {
            tab <- object$tab
            dat <- data.frame(x = rep(colnames(tab), times = colSums(tab)),
                              y = rep(rep(rownames(tab), times = ncol(tab)), tab))

            b <- boot(dat, function(d, f) {
                tt <- t(table(d[f, "x"], d[f, "y"]))
                n1 <- nrow(tt)
                n2 <- ncol(tt)
                nn <- rowSums(tt)
                pp <- sweep(tt, 1, nn, "/")

                d <- c()
                for (ii in 1:n2)
                    for (jj in 2:n1)
                        d <- c(d, pp[jj, ii] - pp[jj - 1, ii])

                d
            }, R = nb)
        }

        for (j in 1:ncol(phat)) {
            p <- phat[, j]
            n <- length(p)
            lev <- dn[[2]][j]
            sum <- rowSums(object$tab)

            if (bs) {
                ni <- nrow(tab) - 1
                wi <- 1:ni + (j - 1) * ni
                diff <- matrix(nrow = ni + 1, ncol = ni + 1)
                diff[lower.tri(diff)] <- colMeans(b$t, na.rm = TRUE)[wi]
                diff <- formatTriMat(diff, rownames(tab))
            } else {
                diff <- outer(p, p, function(p1, p2) p1 - p2)
                diff <- formatTriMat(diff, dn[[1]])
            }

            out <- c(out, "",
                     paste0("  # Group differences between proportions with: ", vn$x, " = ", lev),
                     "    (col group - row group)", "",
                     "Estimates", "",
                     apply(diff, 1, function(x) paste0("   ", paste(x, collapse = "   "))))

            if (!is.survey) {
                cis <- matrix(NA, nrow = 2 * (n - 1), ncol = n - 1)
                for (k in 2:n) {
                    for (l in 1:(k - 1)) {
                        wr <- (k - 2) * 2 + 1
                        cis[wr:(wr + 1), l] <-
                            if (bs) quantile(b$t[, j], c(0.025, 0.975), na.rm = TRUE)
                            else pDiffCI(p[k], p[l], sum[k], sum[l])
                    }
                }
                colnames(cis) <- dn[[1]][-n]
                rownames(cis) <- c(rbind(dn[[1]][-1], ""))

                cis <- formatMat(cis, 3)

                out <- c(out, "",
                         paste0("95% ", bsCI, " Confidence Intervals"), "",
                         apply(cis, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
            }
        }
    } else {
        mat <- t(rbind(inf$conf$lower, inf$conf$estimate, inf$conf$upper))

        mat <- matrix(apply(mat, 2, function(col) {
            format(col, digits = 3)
        }), nrow = nrow(mat))

        ## Remove NA's and replace with an empty space
        mat[grep("NA", mat)] <- ""
        mat[grep("NaN", mat)] <- ""

        ## Text formatting to return a character vector - each row of matrix
        mat <- rbind(c("Lower", "Estimate", "Upper"), mat)
        colnames(mat) <- NULL
        LEVELS <- colnames(object$tab)
        mat <- cbind(c("", LEVELS), mat)
        rownames(mat) <- NULL

        mat <- matrix(apply(mat, 2, function(col) {
            format(col, justify = "right")
        }), nrow = nrow(mat))

        out <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))

        bsCI <- ifelse(bs, " Percentile Bootstrap", "")
        out <- c(paste0(sprintf("Estimated %sProportions with 95%s", ifelse(is.survey, "Population ", ""), "%"),
                                bsCI, " Confidence Interval"), "",
                 out)

        if (bs) {
            ## This is about the only place we do bootstrapping within the inference function, as no such
            ## method is applicable to the plots themselves.
            ## All other inferences are generated by the plotting function, which are then displayed on the plot
            ## and hence match the output from this function.

            dat <- data.frame(x = rep(names(object$tab), times = object$tab))
            b <- boot(dat, function(d, f) {
                tt <- table(d[f, ])
                ni <- length(tt)
                nn <- sum(tt)
                pp <- tt / nn

                d <- c()
                for (ii in 2:ni)
                    for (jj in 1:(ii - 1))
                        d <- c(d, pp[ii] - pp[jj])

                d
            }, R = nb)

            diffs <- matrix(nrow = length(LEVELS), ncol = length(LEVELS))
            diffs[lower.tri(diffs)] <- colMeans(b$t)
            diffs <- formatTriMat(diffs, LEVELS)

            cil <- ciu <- matrix(nrow = length(LEVELS), ncol = length(LEVELS))
            cil[lower.tri(cil)] <- apply(b$t, 2, quantile, probs = 0.025)
            ciu[lower.tri(ciu)] <- apply(b$t, 2, quantile, probs = 0.975)

            cil <- formatTriMat(cil, LEVELS)
            ciu <- formatTriMat(ciu, LEVELS)

            cis <- rbind(cil[-1, , drop = FALSE], ciu[-1, , drop = FALSE])
            cis <- cis[order(cis[, 1]), -1, drop = FALSE]

            rownames(cis) <- rbind(LEVELS[-1], "")
            colnames(cis) <- cil[1, -1]

            cis <- formatMat(cis)

        } else {
            diffs <- freq1way.edited(object$tab, "estimates")
            diffs <- formatMat(diffs)

            cis <- freq1way.edited(object$tab, "ci")
            cis <- formatMat(cis)
        }

        out <- c(out, "", HypOut, "",
                 sprintf("### Differences in proportions of %s", vn$x),
                 "    (col group - row group)", "",
                 "Estimates", "",
                 apply(diffs, 1, function(x) paste0("   ", paste(x, collapse = "   "))),
                 "",
                 paste0("95%", bsCI, " Confidence Intervals"), "",
                 apply(cis, 1, function(x) paste0("   ", paste(x, collapse = "   "))))
    }

    out
}

freq1way.edited <- function(tbl, inf.type = "estimates", conf.level = 0.95) {
    ## Before freq1way is called should output the variable name in the table
    level.names <- colnames(tbl)
    n <- sum(tbl)
    ncats <- length(tbl)
    ncatsC2 <- choose(ncats, 2)

    if (is.null(level.names)) level.names <- 1:ncats

    conf.pc <- conf.level * 100
    phat <- tbl / sum(tbl)

    qval <- abs(qnorm((1 - conf.level) / (2 * ncats)))

    matw <- matrix(NA, ncats - 1, ncats - 1)

    dimnames(matw) <- list(level.names[-length(level.names)], level.names[-1])

    qval.adjusted <- abs(qnorm((1 - conf.level) / (2 * ncatsC2)))

    tempw <- ""

    if (inf.type == "estimates") {
        for(i1 in 1:(ncats - 1)) {
            for(i2 in 2:ncats) {
                tempw <- phat[i1] - phat[i2]
                tempw <- signif(tempw, 5)
                matw[i1, i2 - 1] <- ifelse((i1 < i2), tempw , " ")
            }
        }
        t(matw)
    } else {
        testMatrix <- matrix(" ", ncats - 1, 2 * ncats - 2)
        count <- 1
        count.2 <- 0
        for(i1 in 1:(ncats - 1)) {
            count <- 0
            for(i2 in 2:ncats) {
                tempw <- phat[i1] - phat[i2] +
                    abs(qnorm((1 - conf.level) / (2 * ncatsC2))) * c(-1, 1) *
                        sqrt(((phat[i1] + phat[i2]) - ((phat[i1] - phat[i2])^2)) / n)
                tempw <- signif(tempw, 5)
                matw[i1, i2 - 1] <- ifelse((i1 < i2),
                                           paste("(", tempw[1], ",", tempw[2], ")", sep = ""),
                                           " ")

                if(i2 == 2)
                    count <- i2 - 2
                if(i1 < i2) {
                    testMatrix[i1, count + 1 + count.2] <- tempw[1]
                    testMatrix[i1, (count = count + 2) + count.2] <- tempw[2]
                }
            }
            count.2 <- count.2 + 2
        }

        rowNames <- rep("", ncats * 2)
        temp <- 1:(ncats * 2)
        rowNames[(temp %% 2 != 0)] <- level.names

        testMatrix <- t(testMatrix)
        rownames(testMatrix) <- rowNames[-c(1, 2)]
        colnames(testMatrix) <- level.names[-ncats]

        testMatrix
    }
}

pDiffCI <- function(p1, p2, n1, n2, z = 1.96) {
    p <- p1 - p2
    se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)

    p + c(-1, 1) * z * se
}


inference.inzscatter <- function(object, des, bs, nb, vn, ...) {
    d <- data.frame(x = object$x, y = object$y)
    trend <- object$trend

    if (is.null(trend))
        return("Please specify a trend line to obtain inference information.")

    if (bs & nrow(d) < 10)
        return("Not enough observations to perform bootstrap simulation.")

    is.survey <- !is.null(des)

    ## Ensure the order is linear/quad/cubic
    allT <- c("linear", "quadratic", "cubic")
    tr <- (1:3)[allT %in% trend]

    out <- character()

    for (t in tr) {
        if (nrow(d) <= t + 1) {
            out <- c(out, "",
                     paste0("Not enough observations (n = ", nrow(d), ") to fit ",
                            switch(t, "Linear", "Quadratic", "Cubic"), " trend"))
            break
        } else {
            if (bs) {
                if (is.survey) return("Bootstrap inference not yet implemented for survey data.")

                b <- boot(d,
                          function(dat, f) {
                              fit <- switch(t,
                                            lm(y ~ x, data = dat[f, ]),
                                            lm(y ~ x + I(x^2), data = dat[f, ]),
                                            lm(y ~ x + I(x^2) + I(x^3), data = dat[f, ]))
                              c(coef(fit),
                                if (t == 1) cor(d[f, "x"], d[f, "y"]))
                          }, R = object$n.boot)
                mat <-
                    cbind(sprintf("%.5g", colMeans(b$t, na.rm = TRUE)),
                          sprintf("%.5g", apply(b$t, 2, quantile, probs = 0.025, na.rm = TRUE)),
                          sprintf("%.5g", apply(b$t, 2, quantile, probs = 0.975, na.rm = TRUE)))

            } else {
                if (is.survey) {
                    fit <- switch(t,
                                  svyglm(y ~ x, des),
                                  svyglm(y ~ x + I(x^2), des),
                                  svyglm(y ~ x + I(x^2) + I(x^3), des))
                } else {
                    fit <- switch(t,
                                  lm(y ~ x, data = d),
                                  lm(y ~ x + I(x^2), data = d),
                                  lm(y ~ x + I(x^2) + I(x^3), data = d))
                }

                cc <- summary(fit)$coef
                ci <- confint(fit)

                mat <-
                    cbind(sprintf("%.5g", cc[, 1]),
                          sprintf("%.5g", ci[, 1]),
                          sprintf("%.5g", ci[, 2]),
                          format.pval(cc[, 4], digits = 2))
            }

            if (bs & t == 1) {
                covMat <- mat[3, ]
                mat <- mat[1:2, ]
            }


            mat <- rbind(c("Estimate", "Lower", "Upper", if (!bs) "p-value"), mat)

            rn <- paste0(vn$x, c("", "^2", "^3"))
            mat <- cbind(c("", "Intercept", rn[1:t]), mat)
            if (bs & t == 1) {
                mat <- rbind(mat, "", c("correlation", covMat))
            }
            mat <- apply(mat, 2, function(x) format(x, justify = "right"))

            out <- c(out, "",
                     paste0(switch(t, "Linear", "Quadratic", "Cubic"), " Trend Coefficients with 95% ",
                            ifelse(bs, "Percentile Bootstrap ", ""), "Confidence Intervals"), "",
                     apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   "))))

            ## add a 'key' to the end of the output
            if (t == max(tr) & !bs)
                out <- c(out, "", "",
                         "   p-values for the null hypothesis of no association, H0: beta = 0")
        }
    }

    out
}
inference.inzgrid <- function(object, bs, nboot, vn, ...)
    inference.inzscatter(object, bs, nboot, vn, ...)

inference.inzhex <- function(object, bs, nboot, vn, ...)
    inference.inzscatter(object, bs, nboot, vn, ...)


