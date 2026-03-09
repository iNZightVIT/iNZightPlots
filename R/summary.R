summary.inzdot <- function(object, opts, des, survey.options, privacy_controls, ...) {
    ## Generate summary information:

    ## Produce a matrix of the required summary:
    toplot <- object$toplot

    n_mat_q <- NULL
    s_mat <- s_mat_mag <- NULL
    if (is.null(des)) {
        smrytype <- "numeric"

        if (!is.null(object$boxinfo[[1]]$opts$transform) &&
            !is.null(object$boxinfo[[1]]$opts$transform$x)) {
            smrytype <- object$boxinfo[[1]]$opts$transform$x
        }

        mat <- do.call(
            rbind,
            lapply(
                names(toplot),
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
                                    sprintf(
                                        "%i days",
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
            suppressWarnings(
                if (utils::packageVersion("survey") >= "4.1") {
                    smry_q <- svyby(~x, ~y, des,
                        get("oldsvyquantile", asNamespace("survey")),
                        quantiles = c(0.25, 0.5, 0.75),
                        ci = TRUE,
                        se = TRUE,
                        na.rm = TRUE,
                        keep.var = TRUE,
                        drop.empty.groups = FALSE
                    )
                } else {
                    smry_q <- svyby(~x, ~y, des, svyquantile,
                        quantiles = c(0.25, 0.5, 0.75),
                        ci = TRUE,
                        se = TRUE,
                        na.rm = TRUE,
                        keep.var = TRUE,
                        drop.empty.groups = FALSE
                    )
                }
            )
            smry_mean <- svyby(~x, ~y, des, svymean,
                na.rm = TRUE,
                deff = survey.options$deff,
                drop.empty.groups = FALSE,
            )
            smry_var <- svyby(~x, ~y, des, svyvar,
                na.rm = TRUE,
                drop.empty.groups = FALSE
            )
            smry_total <- svyby(~x, ~y, des, svytotal,
                na.rm = TRUE,
                deff = survey.options$deff,
                drop.empty.groups = FALSE
            )
            smry_popsize <- svytotal(~y, des)

            if (!is.null(privacy_controls)) {
                if (privacy_controls$has("suppression")) {
                    s_mat <- privacy_controls$suppression_matrix(coef(smry_popsize))
                    s_mat <- s_mat[-length(s_mat)]
                }
                if (privacy_controls$has("suppression_magnitude")) {
                    s_mat_mag <- privacy_controls$suppression_matrix(
                        as.vector(table(dv$y)),
                        using = "suppression_magnitude"
                    )
                    s_mat_mag <- s_mat_mag[-length(s_mat_mag)]
                }
                if (privacy_controls$has("suppression_quantiles")) {
                    n_mat_q <- as.vector(table(dv$y))
                }
            }

            mat <- cbind(
                smry_q[, 2:4],
                coef(smry_mean),
                sqrt(coef(smry_var)),
                coef(smry_total),
                coef(smry_popsize),
                NaN,
                as.vector(table(dv$y)),
                tapply(dv$x, dv$y, min, na.rm = TRUE),
                tapply(dv$x, dv$y, max, na.rm = TRUE)
            )

            semat <- cbind(
                suppressWarnings(SE(smry_q)),
                SE(smry_mean),
                {
                    vc <- suppressWarnings(diag(vcov(smry_var)))
                    sqrt(vc / 4 / coef(smry_var))
                },
                SE(smry_total),
                SE(smry_popsize),
                NA,
                NA,
                NA,
                NA
            )

            dimnames(semat) <- dimnames(mat)
            mat <- rbind(mat, semat)

            # returns TRUE for TRUE and "replace"
            if (!isFALSE(survey.options$deff)) {
                deffmat <- cbind(
                    NA, NA, NA,
                    deff(smry_mean),
                    NA,
                    deff(smry_total),
                    NA, NA, NA, NA, NA
                )
                colnames(deffmat) <- colnames(mat)
                mat <- rbind(mat, deffmat)
            }
        } else {
            if (utils::packageVersion("survey") >= "4.1") {
                smry_q <- svyquantile(~x, des,
                    na.rm = TRUE,
                    quantiles = c(0.25, 0.5, 0.75),
                    ci = TRUE
                )
            } else {
                smry_q <- svyquantile(~x, des,
                    na.rm = TRUE,
                    quantiles = c(0.25, 0.5, 0.75),
                    a.rm = TRUE,
                    se = TRUE
                )
            }
            smry_mean <- svymean(~x, des,
                na.rm = TRUE,
                deff = survey.options$deff
            )
            smry_var <- svyvar(~x, des, na.rm = TRUE)
            smry_total <- svytotal(~x, des,
                na.rm = TRUE,
                deff = survey.options$deff
            )
            smry_popsize <- svytotal(ones, des, na.rm = TRUE)

            mat <- cbind(
                if (inherits(smry_q, "newsvyquantile")) {
                    rbind(smry_q$x[, "quantile"])
                } else if (is_svyrep(des)) {
                    t(rbind(coef(smry_q)))
                } else {
                    rbind(coef(smry_q))
                },
                coef(smry_mean),
                sqrt(coef(smry_var)),
                coef(smry_total),
                coef(smry_popsize),
                NaN,
                nrow(dv),
                min(dv$x, na.rm = TRUE),
                max(dv$x, na.rm = TRUE)
            )

            semat <- cbind(
                if (inherits(smry_q, "newsvyquantile")) {
                    cn <- colnames(smry_q$x)
                    cnse <- grep("se", cn)
                    if (length(cnse)) {
                        rbind(smry_q$x[, cnse[1]])
                    } else {
                        rbind(rep(NA, nrow(smry_q$x)))
                    }
                } else if (is_svyrep(des)) {
                    t(rbind(SE(smry_q)))
                } else {
                    rbind(SE(smry_q))
                },
                SE(smry_mean),
                sqrt(vcov(smry_var) / 4 / coef(smry_var)),
                SE(smry_total),
                SE(smry_popsize),
                NA,
                NA,
                NA,
                NA
            )

            mat <- rbind(mat, semat)

            # returns TRUE for TRUE and "replace"
            if (!isFALSE(survey.options$deff)) {
                deffmat <- cbind(
                    NA, NA, NA,
                    deff(smry_mean),
                    NA,
                    deff(smry_total),
                    NA, NA, NA, NA, NA
                )
                colnames(deffmat) <- colnames(mat)
                mat <- rbind(mat, deffmat)
            }
        }
        rns <- c(
            "25%", "Median", "75%", "Mean", "SD", "Total", "Est. Pop. Size",
            "|", "Sample Size", "Min", "Max"
        )
        # if (!all(get_weights(des) == 0 | get_weights(des) >= 1)) {
        #     mat <- mat[, -(6:7)]
        #     rns <- rns[-(6:7)]
        # }
    }

    mat <- matrix(
        apply(
            mat, 2,
            function(col) {
                format(col, digits = opts$signif, scientific = FALSE)
            }
        ),
        nrow = nrow(mat)
    )
    # suppress means and totals, and pop size
    if (!is.null(s_mat_mag)) {
        mat[, 4L] <- privacy_controls$suppress(mat[, 4L], s_mat_mag)
        mat[, 6L] <- privacy_controls$suppress(mat[, 6L], s_mat_mag)
    }
    if (!is.null(s_mat)) {
        mat[, 7L] <- privacy_controls$suppress(mat[, 7L], s_mat)
    }
    if (!is.null(n_mat_q)) {
        mat[, 1L] <- privacy_controls$suppress_quantile(
            mat[, 1L], n_mat_q, 0.25
        )
        mat[, 2L] <- privacy_controls$suppress_quantile(
            mat[, 2L], n_mat_q, 0.5
        )
        mat[, 3L] <- privacy_controls$suppress_quantile(
            mat[, 3L], n_mat_q, 0.75
        )
    }

    ## Remove NA's and replace with an empty space
    mat[grep("NA", mat)] <- ""
    mat <- gsub("NaN", "|", mat)

    ## Remove privacy-suppressed columns from mat and rns
    if (!is.null(privacy_controls)) {
        mat <- mat[, -(8:11), drop = FALSE]
        rns <- rns[-(8:11)]
    }

    ## Determine row headers (group names if multiple groups)
    row_names <- if (length(toplot) > 1) {
        rep(
            names(toplot),
            ifelse(exists("semat"),
                ifelse(exists("deffmat"), 3, 2),
                1
            )
        )
    } else {
        NULL
    }

    ## Build output using out_*() nodes, splitting into sections if survey
    header_label <- ifelse(is.null(des), "Estimates", "Population estimates:")

    if (exists("semat") && exists("deffmat")) {
        n_groups <- length(toplot)
        est_rows <- seq_len(n_groups)
        se_rows <- n_groups + seq_len(n_groups)
        deff_rows <- 2L * n_groups + seq_len(n_groups)

        est_rn <- if (!is.null(row_names)) row_names[est_rows] else NULL
        se_rn <- if (!is.null(row_names)) row_names[se_rows] else NULL
        deff_rn <- if (!is.null(row_names)) row_names[deff_rows] else NULL

        node <- out_group(
            out_text(header_label),
            out_blank(),
            out_table(mat[est_rows, , drop = FALSE],
                col_headers = rns, row_headers = est_rn
            ),
            out_blank(),
            out_text("Standard error of estimates:"),
            out_blank(),
            out_table(mat[se_rows, , drop = FALSE],
                row_headers = se_rn
            ),
            out_blank(),
            out_text("Design effects:"),
            out_blank(),
            out_table(mat[deff_rows, , drop = FALSE],
                row_headers = deff_rn
            )
        )
    } else if (exists("semat")) {
        n_groups <- length(toplot)
        est_rows <- seq_len(n_groups)
        se_rows <- n_groups + seq_len(n_groups)

        est_rn <- if (!is.null(row_names)) row_names[est_rows] else NULL
        se_rn <- if (!is.null(row_names)) row_names[se_rows] else NULL

        node <- out_group(
            out_text(header_label),
            out_blank(),
            out_table(mat[est_rows, , drop = FALSE],
                col_headers = rns, row_headers = est_rn
            ),
            out_blank(),
            out_text("Standard error of estimates:"),
            out_blank(),
            out_table(mat[se_rows, , drop = FALSE],
                row_headers = se_rn
            )
        )
    } else {
        node <- out_group(
            out_text(header_label),
            out_blank(),
            out_table(mat, col_headers = rns, row_headers = row_names)
        )
    }

    node
}

summary.inzhist <- function(object, opts, des, survey.options, privacy_controls, ...) {
    summary.inzdot(object, opts, des, survey.options, privacy_controls, ...)
}


summary.inzbar <- function(object, opts, vn, des, survey.options,
                           privacy_controls, table.direction, ...) {
    tab <- round(object$tab)
    perc <- object$phat * 100
    twoway <- length(dim(tab)) == 2 && nrow(tab) > 1

    is.survey <- !is.null(des)

    s_mat <- NULL
    if (!is.null(privacy_controls)) {
        s_mat <- privacy_controls$suppression_matrix(tab)
        if (is.survey && privacy_controls$has("suppression_raw_counts")) {
            rtab <- with(des$variables, table(y, x))
            sr_mat <- privacy_controls$suppression_matrix(
                rtab,
                using_raw = TRUE
            )
            s_mat <- s_mat | sr_mat
        }
        tab <- privacy_controls$round(tab)
        if (twoway) {
            perc <- 100 * sweep(tab, 1, rowSums(tab), "/")
        } else {
            perc <- 100 * tab / sum(tab)
        }
    }

    # survey tables do this thing where they retain their dimensions,
    # even for one-way tables
    if (twoway) {
        tab <- as.matrix(tab)
        s_mat_tab <- NULL
        if (is.survey) {
            # needed for suppressing percentages ...
            svy_tab <- svyby(~x, ~y, des, svytotal,
                drop.empty.groups = FALSE,
                na.rm = TRUE
            )
            smry_mean <- svyby(~x, ~y, des, svymean,
                deff = survey.options$deff,
                drop.empty.groups = FALSE,
                na.rm = TRUE
            )
            if (!is.null(privacy_controls) && privacy_controls$has("check_rse")) {
                xhat <- coef(svy_tab)
                xse <- as.matrix(SE(svy_tab))
                dim(xhat) <- dim(xse) <- dim(tab)
                dimnames(xhat) <- dimnames(xse) <- dimnames(tab)
                rse_mat_tab <- privacy_controls$rse_matrix(xhat, xse)

                s_mat_tab <- s_mat | rse_mat_tab == "suppress"
            }
        }

        perc <- format(
            round(as.matrix(perc), opts$round_percent),
            nsmall = opts$round_percent
        )
        perc <- t(
            apply(
                perc, 1,
                function(p) {
                    if (!any(grepl("NA", p))) {
                        paste0(c(p, "100"), "%")
                    } else {
                        rep("", length(p) + 1)
                    }
                }
            )
        )

        cm1 <- cbind(tab, rowSums(tab))
        mat1 <- rbind(
            c(
                colnames(tab),
                sprintf(
                    "%s Total",
                    switch(table.direction,
                        vertical = "Column",
                        horizontal = "Row"
                    )
                )
            ),
            if (!is.null(s_mat_tab)) {
                privacy_controls$suppress(cm1, s_mat_tab)
            } else if (!is.null(s_mat)) {
                privacy_controls$suppress(cm1, s_mat)
            } else {
                cm1
            }
        )
        mat1 <- cbind(c("", rownames(tab)), mat1)

        if (table.direction == "vertical") {
            mat1 <- t(mat1)
        }

        # Right-justify first, then apply RSE markup if needed
        mat1 <- matrix(
            apply(
                mat1, 2,
                function(col) {
                    format(col, justify = "right")
                }
            ),
            nrow = nrow(mat1)
        )

        justify1 <- "right"
        if (!is.null(s_mat_tab)) {
            mat1[-1, -1] <- privacy_controls$markup(mat1[-1, -1], rse_mat_tab)
            justify1 <- "left"
        }

        sep_after1 <- if (table.direction == "vertical") {
            nrow(mat1) - 1L
        } else {
            NULL
        }

        table1_node <- out_table(mat1,
            justify = justify1,
            separator_after = sep_after1
        )

        cm2 <- cbind(perc, rowSums(tab))
        mat2 <- rbind(
            c(
                colnames(tab), "Total",
                sprintf(
                    "%s N",
                    switch(table.direction,
                        vertical = "Column",
                        horizontal = "Row"
                    )
                )
            ),
            if (!is.null(s_mat_tab)) {
                privacy_controls$suppress(
                    cm2,
                    t(apply(s_mat_tab, 1, function(x) c(x | x[length(x)], x[length(x)])))
                )
            } else if (!is.null(s_mat)) {
                privacy_controls$suppress(
                    cm2,
                    t(apply(s_mat, 1, function(x) c(x | x[length(x)], x[length(x)])))
                )
            } else {
                cm2
            }
        )
        mat2 <- cbind(c("", rownames(tab)), mat2)

        if (table.direction == "vertical") {
            mat2 <- t(mat2)
        }

        sep_after2 <- if (table.direction == "vertical") {
            nrow(mat2) - 2L
        } else {
            NULL
        }

        table2_node <- out_table(mat2,
            justify = "right",
            separator_after = sep_after2
        )

        parts <- list(
            out_text(sprintf(
                "Table of %sCounts:",
                ifelse(is.survey, "Estimated Population ", "")
            )),
            out_blank(),
            table1_node,
            out_blank(),
            out_text(sprintf(
                "Table of %sPercentages (within categories of %s):",
                ifelse(is.survey, "Estimated Population ", ""),
                vn$y
            )),
            out_blank(),
            table2_node
        )

        if (is.survey) {
            mat <- format(
                round(SE(smry_mean) * 100, opts$round_percent),
                nsmall = opts$round_percent
            )
            mat <- as.matrix(mat)
            if (!is.null(s_mat_tab)) {
                mat <- privacy_controls$suppress(mat, s_mat_tab[, -ncol(s_mat_tab)])
            } else if (!is.null(s_mat)) {
                mat <- privacy_controls$suppress(mat, s_mat_tab[, -ncol(s_mat)])
            }

            mat <- cbind(
                c("", rownames(tab)),
                rbind(colnames(tab), mat)
            )

            if (table.direction == "vertical") {
                mat <- t(mat)
            }

            se_table_node <- out_table(mat,
                justify = "right",
                na_replace = ""
            )

            parts <- c(parts, list(
                out_blank(),
                out_text("Standard errors of estimated percentages:"),
                out_blank(),
                se_table_node
            ))

            if (!isFALSE(survey.options$deff)) {
                mat <- format(deff(smry_mean), digits = opts$signif)
                mat <- as.matrix(mat)
                if (!is.null(s_mat_tab)) {
                    mat <- privacy_controls$suppress(mat, s_mat_tab[, -ncol(s_mat_tab)])
                } else if (!is.null(s_mat)) {
                    mat <- privacy_controls$suppress(mat, s_mat_tab[, -ncol(s_mat)])
                }

                mat <- cbind(
                    c("", rownames(tab)),
                    rbind(colnames(tab), mat)
                )
                if (table.direction == "vertical") {
                    mat <- t(mat)
                }

                deff_table_node <- out_table(mat,
                    justify = "right",
                    na_replace = ""
                )

                parts <- c(parts, list(
                    out_blank(),
                    out_text("Design effects:"),
                    out_blank(),
                    deff_table_node
                ))
            }
        }

        return(do.call(out_group, parts))
    } else {
        cm <- c(tab, sum(tab))
        perc <- round(perc, opts$round_percent)
        pm <- paste0(
            c(format(perc, nsmall = opts$round_percent), "100"),
            "%"
        )
        mat <- rbind(
            c(colnames(tab), "Total"),
            if (is.null(s_mat)) cm else privacy_controls$suppress(cm, s_mat),
            if (is.null(s_mat)) pm else privacy_controls$suppress(pm, s_mat)
        )

        mat <- cbind(c("", "Count", "Percent"), mat)
        if (is.survey) {
            smry_mean <- svymean(~x, des, deff = survey.options$deff, na.rm = TRUE)
            semat <- paste0(
                format(
                    round(SE(smry_mean) * 100, opts$round_percent),
                    nsmall = opts$round_percent
                ),
                "%"
            )
            if (!is.null(s_mat)) {
                semat <- privacy_controls$suppress(semat, s_mat[-ncol(s_mat)])
            }
            mat <- rbind(
                mat[1:2, ],
                "",
                mat[3, ],
                c("Standard Error", semat, NA)
            )
            if (!isFALSE(survey.options$deff)) {
                deffmat <- paste0(
                    format(
                        round(deff(smry_mean), 2L),
                        nsmall = 2L
                    ),
                    ""
                )
                if (!is.null(s_mat)) {
                    deffmat <- privacy_controls$suppress(deffmat, s_mat[-ncol(s_mat)])
                }
                mat <- rbind(
                    mat,
                    "",
                    c("Design effects", deffmat, NA)
                )
            }
        }

        if (table.direction == "vertical") {
            mat <- t(mat)
        }

        sep_after <- if (table.direction == "vertical") {
            nrow(mat) - 1L
        } else {
            NULL
        }

        table_node <- out_table(mat,
            justify = "right",
            na_replace = "",
            separator_after = sep_after
        )

        if (is.survey) {
            node <- out_group(
                out_text("Population Estimates:"),
                out_blank(),
                table_node
            )
        } else {
            node <- table_node
        }

        return(node)
    }
}



summary.inzscatter <- function(object, opts, vn, des, survey.options, ...) {
    x <- object$x
    y <- object$y
    trend <- object$trend

    is.survey <- !is.null(des)

    parts <- list()

    if ("linear" %in% trend) {
        beta <- try(
            {
                if (is.survey) {
                    signif(coef(svyglm(y ~ x, design = des)), opts$signif)
                } else {
                    signif(coef(lm(y ~ x)), opts$signif)
                }
            },
            silent = TRUE
        )

        if (inherits(beta, "try-error")) {
            parts <- c(parts, list(out_text("Unable to fit linear trend.")))
        } else {
            parts <- c(parts, list(
                out_text("Linear trend:"),
                out_blank(),
                out_text(sprintf(
                    "    %s = %s %s %s * %s",
                    vn$y,
                    beta[1],
                    ifelse(beta[2] < 0, "-", "+"),
                    abs(beta[2]),
                    vn$x
                )),
                out_text(paste0(
                    "    Linear correlation: ",
                    if (is.survey) {
                        signif(
                            cov2cor(as.matrix(svyvar(y ~ x, design = des)))[1, 2],
                            opts$signif
                        )
                    } else {
                        signif(cor(x, y), opts$signif)
                    }
                )),
                out_blank()
            ))
        }
    }

    if ("quadratic" %in% trend) {
        beta <- try(
            {
                if (is.survey) {
                    signif(
                        coef(svyglm(y ~ x + I(x^2), design = des)),
                        opts$signif
                    )
                } else {
                    signif(
                        coef(lm(y ~ x + I(x^2))),
                        opts$signif
                    )
                }
            },
            silent = TRUE
        )

        if (inherits(beta, "try-error")) {
            parts <- c(parts, list(out_text("Unable to fit quadratic trend.")))
        } else {
            parts <- c(parts, list(
                out_text("Quadratic trend:"),
                out_blank(),
                out_text(sprintf(
                    "    %s = %s %s %s * %s %s %s * %s^2",
                    vn$y,
                    beta[1],
                    ifelse(beta[2] < 0, "-", "+"),
                    abs(beta[2]),
                    vn$x,
                    ifelse(beta[3] < 0, "-", "+"),
                    abs(beta[3]),
                    vn$x
                )),
                out_blank()
            ))
        }
    }

    if ("cubic" %in% trend) {
        beta <- beta <- try(
            {
                if (is.survey) {
                    signif(
                        coef(svyglm(y ~ x + I(x^2) + I(x^3), design = des)),
                        opts$signif
                    )
                } else {
                    signif(
                        coef(lm(y ~ x + I(x^2) + I(x^3))),
                        opts$signif
                    )
                }
            },
            silent = TRUE
        )

        if (inherits(beta, "try-error")) {
            parts <- c(parts, list(out_text("Unable to fit linear trend.")))
        } else {
            parts <- c(parts, list(
                out_text("Cubic trend:"),
                out_blank(),
                out_text(sprintf(
                    "    %s = %s %s %s * %s %s %s * %s^2 %s %s * %s^3",
                    vn$y,
                    beta[1],
                    ifelse(beta[2] < 0, "-", "+"),
                    abs(beta[2]),
                    vn$x,
                    ifelse(beta[3] < 0, "-", "+"),
                    abs(beta[3]),
                    vn$x,
                    ifelse(beta[4] < 0, "-", "+"),
                    abs(beta[4]),
                    vn$x
                )),
                out_blank()
            ))
        }
    }

    if (is.survey) {
        ## rank.cor <- cov2cor(coef(svyvar(rank(y) ~ rank(x), design = des)))[1,2]
        if (!"linear" %in% trend) {
            cor <- signif(
                cov2cor(as.matrix(svyvar(y ~ x, design = des, na.rm = TRUE)))[1, 2],
                opts$signif
            )
            parts <- c(parts, list(
                out_text(paste0(
                    "Correlation: ",
                    signif(cor, opts$signif),
                    "  (using Pearson's Correlation)"
                ))
            ))
        }
    } else {
        rank.cor <- cor(x, y, method = "spearman")
        parts <- c(parts, list(
            out_text(paste0(
                "Rank correlation: ",
                signif(rank.cor, opts$signif),
                "  (using Spearman's Rank Correlation)"
            ))
        ))
    }

    do.call(out_group, parts)
}
summary.inzgrid <- function(object, opts, vn, des, survey.options, ...) {
    summary.inzscatter(object, opts, vn, des, ...)
}

summary.inzhex <- function(object, opts, vn, des, survey.options, ...) {
    summary.inzscatter(object, opts, vn, des, ...)
}
