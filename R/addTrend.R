addXYtrend <- function(obj, opts, col.args, xlim, ylim) {
    # Trend lines:
    # ------------------------------------------------------------- #
    # If the `by` variable has been set, then the points are
    # coloured by the levels of `by`. Thus, there is more than one
    # level of `unique(col)`. In this case, we need to add the
    # trend lines for each level of by (i.e., each colour). The
    # colours of these lines are darker versions of the points.
    # ------------------------------------------------------------- #

    ## decide what x and y are:
    if ("svy" %in% names(obj)) {
        if (is_survey(obj$svy)) {
            x <- obj$svy
            y <- NULL
        } else {
            # na's arent removed
            x <- obj$svy$x
            y <- obj$svy$y
            isna <- is.na(x) | is.na(y)
            x <- x[!isna]
            y <- y[!isna]
        }
    } else if ("args" %in% names(obj)) {
        x <- obj$args$df$x
        y <- obj$args$df$y
        isna <- is.na(x) | is.na(y)
        x <- x[!isna]
        y <- y[!isna]
    } else {
        x <- obj$x
        y <- obj$y
    }

    if (!is.null(opts$trend)) {
        if (length(unique(obj$col)) == 1 | !opts$trend.by) {
            lapply(opts$trend,
                function(o) {
                    # gives us 1, 2, or 3:
                    order <- which(c("linear", "quadratic", "cubic") == o)
                    addTrend(x, y,
                        order = order,
                        xlim = xlim,
                        col = opts$col.trend[[o]],
                        bs = opts$bs.inference,
                        opts = opts
                    )
                }
            )
        } else if (opts$trend.parallel) {
            byy <- as.factor(obj$col)
            lapply(opts$trend,
                function(o) {
                    order <- which(c("linear", "quadratic", "cubic") == o)
                    addParTrend(x, y, byy,
                        order = order,
                        xlim = xlim,
                        cols = col.args$f.cols,
                        opts = opts
                    )
                }
            )
        } else {
            byy <- as.factor(obj$col)  # pseudo-by-variable
            xtmp <- lapply(levels(byy),
                function(c) {
                    x[obj$col == c & !is.na(obj$col)]
                }
            )
            ytmp <- lapply(levels(byy),
                function(c) {
                    y[obj$col == c & !is.na(obj$col)]
                }
            )

            for (b in 1:length(levels(byy))) {
                lapply(opts$trend,
                    function(o) {
                        order <- which(c("linear", "quadratic", "cubic") == o)
                        addTrend(xtmp[[b]], ytmp[[b]],
                            order = order, xlim = xlim,
                            col = col.args$f.cols[b],
                            bs = opts$bs.inference,
                            opts = opts
                        )
                    }
                )
            }
        }
    }
}

addTrend <- function(x, y, order, xlim, col, bs, opts) {
    xx <- seq(xlim[1], xlim[2], length = 1001)
    is.svy <- is_survey(x)

    if (is.svy) {
        if (length(order) == 1) {
            svy <- x
            expr <- switch(order,
                formula(y ~ x),
                formula(y ~ x + I(x^2)),
                formula(y ~ x + I(x^2) + I(x^3))
            )
        }
        yy <- try(
            predict(svyglm(expr, design = svy), data.frame(x = xx)),
            silent = TRUE
        )
    } else {
        yy <- try(
            c(predict(lm(y ~ poly(x, order)), data.frame(x = xx))),
            silent = TRUE
        )
    }
    ord <- switch(order, "linear", "quadratic", "cubic")

    # Sometimes, there might not be enough data points do run poly(),
    # so in this case simply don't draw.
    if (!inherits(yy, "try-error")) {
        grid.lines(xx, yy,
            default.units = "native",
            gp = gpar(col = col, lwd = 2 * opts$lwd, lty = opts$lty.trend[[ord]]),
            name = paste(paste0("inz-trend-", ord), opts$rowNum, opts$colNum, sep = ".")
        )

        if (bs) {
            bs.lines <- vector("list", 30)

            if (is.svy)
                return(NULL)

            for (i in 1:30) {
                ## User wants bootstrap inference for this line.
                id <- sample(1:length(x), replace = TRUE)
                x2 <- x[id]
                y2 <- y[id]

                yy <- try(
                    predict(lm(y2 ~ poly(x2, order)), data.frame(x2 = xx)),
                    silent = TRUE
                )

                ## Some bootstraps can have less than `order` unique points:
                if (inherits(yy, "try-error")) next

                bs.lines[[i]] <- cbind(xx, yy, rep(i, length(yy)))
            }

            all.lines <- do.call(rbind, bs.lines)
            grid.polyline(all.lines[, 1], all.lines[, 2],
                id = all.lines[, 3],
                default.units = "native",
                gp = gpar(col = col, lwd = 1 * opts$lwd, lty = 3),
                name = paste(paste0("inz-bs-", ord), opts$rowNum, opts$colNum, sep = ".")
            )
        }
    }
}

addParTrend <- function(x, y, by, order, xlim, cols, opts) {
    lby <- levels(by)
    xx <- rep(seq(xlim[1], xlim[2], length = 1001), length(lby))
    byy <- rep(lby, each = 1001)
    if (is_survey(x)) {
        if (length(order) == 1) {
            svy <- x
            expr <- switch(order,
                formula(y ~ x + colby),
                formula(y ~ x + I(x^2) + colby),
                formula(y ~ x + I(x^2) + I(x^3) + colby)
            )
            yy <- try(
                predict(LM <- svyglm(expr, design = svy),
                    data.frame(x = xx, colby = byy)
                ),
                silent = TRUE
            )
        }
    } else {
        yy <- try(
            c(predict(LM <- lm(y ~ poly(x, order) + by), data.frame(x = xx, by = byy))),
            silent = TRUE
        )
    }
    ord <- switch(order, "linear", "quadratic", "cubic")

    # Sometimes, there might not be enough data points do run poly(),
    # so in this case simply don't draw.
    if (!inherits(yy, "try-error")) {
        for (i in 1:length(lby)) {
            grid.lines(xx[byy == lby[i]], yy[byy == lby[i]],
                default.units = "native",
                gp = gpar(col = (cols[i]), lwd = 2 * opts$lwd, lty = opts$lty.trend[[ord]]),
                name = paste(
                    paste0("inz-par-trend-", ord),
                    opts$rowNum,
                    opts$colNum,
                    sep = "."
                )
            )
        }
    }
}
