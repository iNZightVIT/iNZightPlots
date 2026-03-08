#' Generate summary or inference information for an iNZight plot
#'
#' Works much the same as \code{iNZightPlot}
#' @title iNZight Plot Summary and Inference
#' @param x a vector (numeric or factor), or the name of a column in the supplied
#' \code{data} or \code{design} object
#' @param y a vector (numeric or factor), or the name of a column in the supplied
#' \code{data} or \code{design} object
#' @param g1 a vector (numeric or factor), or the name of a column in the supplied
#' \code{data} or \code{design} object. This variable acts as a subsetting variable.
#' @param g1.level the name (or numeric position) of the level of \code{g1} that will be
#' used instead of the entire data set
#' @param g2 a vector (numeric or factor), or the name of a column in the supplied
#' \code{data} or \code{design} object. This variable acts as a subsetting variable, similar to
#' \code{g1}
#' @param g2.level same as \code{g1.level}, however takes the additional value \code{"_MULTI"},
#' which produces a matrix of \code{g1} by \code{g2}
#' @param varnames a list of variable names, with the list named using the appropriate arguments
#' (i.e., \code{list(x = "height", g1 = "gender")})
#' @param colby the name of a variable (numeric or factor) to colour points by. In the
#' case of a numeric variable, a continuous colour scale is used, otherwise each level of
#' the factor is assigned a colour
#' @param sizeby the name of a (numeric) variable, which controls the size of points
#' @param data the name of a data set
#' @param design the name of a survey object, obtained from the \code{survey} package
#' @param freq the name of a frequency variable if the data are frequencies
#' @param missing.info logical, if \code{TRUE}, information regarding missingness is
#' displayed in the plot
#' @param inzpars allows specification of iNZight plotting parameters over multiple plots
#' @param summary.type one of \code{"summary"} or \code{"inference"}
#' @param table.direction one of 'horizontal' (default) or 'vertical' (useful for many categories)
#' @param hypothesis.value H0 value for hypothesis test
#' @param hypothesis.alt alternative hypothesis (!=, <, >)
#' @param hypothesis.var.equal use equal variance assumption for t-test?
#' @param hypothesis.use.exact logical, if \code{TRUE} the exact p-value will be calculated (if applicable)
#' @param hypothesis.test in some cases (currently just two-samples) can perform multiple tests (t-test or ANOVA)
#' @param hypothesis.simulated.p.value also calculate (where available) the simulated p-value
#' @param hypothesis either NULL for no test, or missing (in which case above arguments are used)
#' @param survey.options additional options passed to survey methods
#' @param width width for the output, default is 100 characters
#' @param epi.out logical, if \code{TRUE}, then odds/rate ratios and rate differences are printed when appropriate (\code{y} with 2 levels)
#' @param privacy_controls optional, pass in confidentialisation and privacy controls (e.g., random rounding, suppression) for microdata
#' @param html logical, it \code{TRUE} output will be returned as an HTML page (if supported)
#' @param ... additional arguments, see \code{inzpar}
#' @param env compatibility argument
#' @return an \code{inzight.plotsummary} object with a print method
#' @author Tom Elliott
#' @export
#' @examples
#' getPlotSummary(Species, data = iris)
#' getPlotSummary(Species, data = iris,
#'     summary.type = "inference", inference.type = "conf")
#'
#' # perform hypothesis testing
#' getPlotSummary(Sepal.Length, data = iris,
#'     summary.type = "inference", inference.type = "conf",
#'     hypothesis.value = 5)
#'
#' # if you prefer a formula interface
#' inzsummary(Sepal.Length ~ Species, data = iris)
#' inzinference(Sepal.Length ~ Species, data = iris)
#'
#' ## confidentialisation and privacy controls
#' # random rounding and suppression:
#' HairEyeColor_df <- as.data.frame(HairEyeColor)
#' inzsummary(Hair ~ Eye, data = HairEyeColor_df, freq = Freq)
#' inzsummary(Hair ~ Eye, data = HairEyeColor_df, freq = Freq,
#'     privacy_controls = list(
#'         rounding = "RR3",
#'         suppression = 10
#'     )
#' )
getPlotSummary <- function(x, y = NULL, g1 = NULL, g1.level = NULL,
                           g2 = NULL, g2.level = NULL, varnames = list(),
                           colby = NULL, sizeby = NULL,
                           data = NULL, design = NULL, freq = NULL,
                           missing.info = TRUE, inzpars = inzpar(),
                           summary.type = "summary",
                           table.direction = c("horizontal", "vertical"),
                           hypothesis.value = 0,
                           hypothesis.alt = c("two.sided", "less", "greater"),
                           hypothesis.var.equal = FALSE,
                           hypothesis.use.exact = FALSE,
                           hypothesis.test =
                                c("default", "t.test", "anova", "chi2", "proportion"),
                           hypothesis.simulated.p.value = FALSE,
                           hypothesis = list(
                                value = hypothesis.value,
                                alternative = match.arg(hypothesis.alt),
                                var.equal = hypothesis.var.equal,
                                use.exact = hypothesis.use.exact,
                                test = match.arg(hypothesis.test),
                                simulated.p.value = hypothesis.simulated.p.value
                           ),
                           survey.options = list(),
                           width = 100,
                           epi.out = FALSE,
                           privacy_controls = NULL,
                           html = FALSE,
                           ...,
                           env = parent.frame()) {

    # if (inherits(x, "data.frame")) {
    if (missing(x)) {
        x <- data
        class(x) <- c("inzdata", class(x))
        return(summary(x, design))
    }

    ## Grab a plot object!
    m <- match.call(expand.dots = FALSE)
    table.direction <- match.arg(table.direction)

    if ("design" %in% names(m) && !is.null(m$design)) {
        md <- eval(m$design, env)
    } else {
        md <- eval(m$data, env)
    }

    ## Any varnames supplied that AREN'T needed must be removed, otherwise errors:

    # nullVars <- sapply(as.list(m)[names(varnames)], is.null)
    # varnames[nullVars] <- NULL
    varnames <- varnames[which(names(varnames) %in% names(as.list(m)))]

    ## fix up some subsetting group stuff
    if (is.null(m$g1)) {
        if (!is.null(m$g2)) {

            mc <- match.call(expand.dots = TRUE)
            mc$g1 <- NULL
            mc$g1.level <- NULL
            names(mc) <- gsub("g2", "g1", names(mc))

            if (length(varnames) > 0) {
                mc$varnames$g1 <- NULL
                names(mc$varnames) <- gsub("g2", "g1", names(mc$varnames))
            }
            return(eval(mc))
        }
    }

    ## we now want to create a data object which contains *ALL* of the necessary
    ## information, including survey design, or frequency information:

    ## remove these as they aren't necessary and cause problems with "n.missing"
    rmv <- which(names(m) %in% c("colby", "sizeby"))
    if (length(rmv) > 0)
        m <- m[-rmv]

    if (!"df" %in% ls())
        df <- inzDataframe(m, data = md, names = varnames, g1.level, g2.level, env = env)


    ### This is getting complex... so for now ignore manual use.

    ## ## Modify `inzpars` for the inference:
    if (!is.null(list(...)[["inference.type"]]) &&
        list(...)[["inference.type"]] == "comp") {
        warning("Comparison intervals not yet available for Inferential output.\n",
            "Defaulting to confidence intervals.")
    }
    dots <- list(...)
    inzpars <- modifyList(inzpars, dots)
    ## inference.type <- inference.par <- NULL
    ## bs.inference <- FALSE
    ## if (summary.type[1] == "inference") {
    ##     if (!"inference.type" %in% names(dots))
    ##         inference.type <- inzpars$inference.type
    ##     else
    ##         inference.type <- dots$inference.type

    ##     if (is.null(inference.type))
    ##         inference.type <- "conf"


    ##     if (!"inference.par" %in% names(dots))
    ##         inference.par <- inzpars$inference.par
    ##     else
    ##         inference.par <- dots$inference.par

    ##     ## Set the default to "mean" - barplots automatically use proportion
    ##     if (is.null(inference.par))
    ##         inference.par <- "mean"


    ##     ## and grab bootstrap info ...
    ##     if (!"bs.inference" %in% names(dots))
    ##         bs.inference <- inzpars$bs.inference
    ##     else
    ##         bs.inference <- dots$bs.inference
    ## }

    obj <- iNZightPlot(x = x, y = y, g1 = g1, g1.level = g1.level,
        g2 = g2, g2.level = g2.level, varnames = varnames,
        colby = NULL, sizeby = NULL,
        data = data, design = design, freq = freq,
        missing.info = missing.info, inzpars = inzpars,
        plot = FALSE, df = df, env = env, ...
    )

    ### Now we just loop over everything ...

    summary(obj,
        summary.type,
        table.direction,
        hypothesis,
        survey.options,
        width = width,
        epi.out = epi.out,
        privacy_controls = privacy_controls,
        html = html,
        inzpars = inzpars
    )
}


summary.inzplotoutput <- function(object, summary.type = "summary",
                                  table.direction = c("horizontal", "vertical"),
                                  hypothesis = NULL,
                                  survey.options = list(),
                                  privacy_controls = NULL,
                                  inzpars = inzpar(),
                                  width = 100, ...) {
    if (length(summary.type) > 1) {
        warning("Only using the first element of `summary.type`")
        summary.type <- summary.type[1]
    }
    if (!summary.type %in% c("summary", "inference"))
        stop("`summary.type` must be either `summary` or `inference`")

    obj <- object
    table.direction <- match.arg(table.direction)

    vnames <- attr(obj, "varnames")
    g.levels <- attr(obj, "glevels")
    vartypes <- attr(obj, "vartypes")
    missing <- attr(obj, "missing")
    total.missing <- attr(obj, "total.missing")
    total.obs <- attr(obj, "total.obs")
    bs <- attr(obj, "bootstrap")
    inzclass <- attr(obj, "inzclass")
    is.survey <- inzclass == "inz.survey"

    survey.options <- modifyList(default.survey.options, survey.options)

    privacy_controls <- make_privacy_controls(privacy_controls)
    if (!is.null(privacy_controls) && privacy_controls$has("seed")) {
        set.seed(privacy_controls$get("seed"))
    }

    ind <- function(x, indent = 3)
        paste0(paste0(rep(" ", indent), collapse = ""), x)

    parts <- list()

    ## --- Title ---
    title_text <- switch(summary.type,
        "summary" =
            paste0("iNZight Summary",
                ifelse(is.survey, " - Survey Design", "")),
        "inference" =
            paste("iNZight Inference using",
                ifelse(bs,
                    "the Nonparametric Bootstrap",
                    "Normal Theory"))
    )
    parts <- c(parts, list(out_h1(title_text, width)))

    ## --- Header metadata ---
    scatter <- FALSE
    if ("y" %in% names(vnames)) {
        if (vartypes[[vnames$x]] == "numeric" & vartypes[[vnames$y]] == "numeric") {
            scatter <- TRUE
        }
    }

    mat <- cbind(
        ind(
            ifelse(scatter,
                "Response/outcome variable: ",
                "Primary variable of interest: "
            )
        ),
        paste0(
            ifelse(scatter, vnames$y, vnames$x),
            " (",
            gsub("factor", "categorical",
                vartypes[[ifelse(scatter, vnames$y, vnames$x)]]
            ),
            ")"
        )
    )

    if ("y" %in% names(vnames)) {
        mat <- rbind(
            mat,
            cbind(
                ind(
                    paste0(
                        ifelse(scatter,
                            "Predictor/explanatory",
                            "Secondary"
                        ),
                        " variable: "
                    )
                ),
                paste0(
                    ifelse(scatter, vnames$x, vnames$y),
                    " (",
                    gsub("factor", "categorical",
                        vartypes[[ifelse(scatter, vnames$x, vnames$y)]]
                    ),
                    ")"
                )
            )
        )
    }

    wg <- c("g1", "g2") %in% names(vnames)

    if (is.null(g.levels$g2[1]))
        wg[2] <- FALSE

    if (any(wg)) {
        mat <- rbind(mat, "")
        mat <- rbind(
            mat,
            cbind(
                ind("Subset by: "),
                do.call(paste,
                    c(
                        vnames[c("g1", "g2")[wg]],
                        list(sep = " and ")
                    )
                )
            )
        )
    }

    mat <- rbind(mat, "", cbind("Total number of observations: ", total.obs))
    if (total.missing > 0) {
        allnames <- c("x", "y", "g1", "g2")
        nn <- allnames[allnames %in% names(missing)]
        nn <- nn[sapply(missing[nn], function(m) m > 0)]
        mat <- rbind(
            mat,
            cbind(
                ind("Number omitted due to missingness: "),
                paste0(total.missing,
                    if (length(missing) > 1) {
                        paste0(" (",
                                paste(sapply(nn, function(i) {
                                    paste0(missing[[i]], " in ", vnames[[i]])
                                }), collapse = ", "),
                                ")")
                    }
                )
            ),
            cbind(
                ind("Total number of observations used: "),
                total.obs - total.missing
            )
        )
    }
    if (is.survey) {
        des <- attr(obj, "main.design")
        mat <- rbind(
            mat,
            cbind(
                "Estimated population size: ",
                paste0(
                    round(
                        coef(
                            svytotal(matrix(rep(1, nrow(des$variables)), ncol = 1), des)
                        )
                    )
                )
            )
        )
    }
    mat <- cbind(format(mat[, 1], justify = "right"), mat[, 2])
    header_lines <- apply(mat, 1, function(row) paste0(row, collapse = ""))
    names(header_lines) <- NULL
    parts <- c(parts, list(header_lines))

    ## --- Survey design info ---
    design.list <- NULL
    if (is.survey) {
        tmpdesign <- attr(object, "main.design")
        tmpdesign$call <- NULL
        design_output <- capture.output(print(tmpdesign))
        design_lines <- character()
        for (o in design_output) {
            if (o != "NULL") {
                design_lines <- c(design_lines,
                    ind(gsub("Call: NULL", "Replicate weights design", o)))
            }
        }
        design.list <- attr(object, "design")
        if (!is.null(tmpdesign$postStrata))
            design_lines <- c(design_lines, ind("(calibrated)"))

        parts <- c(parts, list(out_rule("-", width), design_lines))
    }

    parts <- c(parts, list(out_rule("=", width), out_blank()))

    ## --- Privacy section ---
    if (!is.null(privacy_controls)) {
        parts <- c(parts, list(out_privacy_section(privacy_controls, width)))
    }

    ## --- Section header helper ---
    simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1, 1)), substring(s, 2),
            sep = "",
            collapse = " "
        )
    }
    stype <- simpleCap(summary.type)

    if (!is.null(vnames$y) &&
         vartypes[[vnames$x]] == "factor" &&
         vartypes[[vnames$y]] == "numeric") {
        tmpx <- vnames$y
        vnames$y <- vnames$x
        vnames$x <- tmpx
    }

    ## --- Main content loop ---
    epi.out <- list(...)[["epi.out"]]

    for (this in names(obj)) {
        if (this != "all") {
            parts <- c(parts, list(
                out_rule("=", width),
                paste0(ind("For the subset where ", 5),
                    vnames$g2, " = ", this)
            ))
        }

        ## CMH test (Cochran-Mantel-Haenszel)
        if (!is.null(epi.out) && isTRUE(epi.out) &&
            length(obj[[this]]) > 1) {
            g1.tabs <- lapply(obj[[this]], "[[", "tab")
            g1.arr <- array(
                as.numeric(unlist(g1.tabs)),
                dim = c(nrow(g1.tabs[[1]]), ncol(g1.tabs[[2]]),
                    length(g1.tabs))
            )

            m <- mantelhaen.test(g1.arr)

            if (all(dim(g1.arr)[1:2] == 2)) {
                cmh.stat <- c(
                    m$method,
                    ":\n",
                    sprintf(
                        "  %s = %.2f, df = %d, p = %f\n",
                        names(m$statistic),
                        m$statistic,
                        m$parameter,
                        m$p.value
                    ),
                    ifelse(
                        m$estimate == 0,
                        "  Common odds ratio unable to be estimated\n",
                        sprintf(
                            "  Common odds ratio: %.2f (95%% CI: %.2f, %.2f)\n",
                            m$estimate,
                            m$conf.int[1],
                            m$conf.int[2]
                        )
                    )
                )
            } else {
                cmh.stat <- c(
                    m$method,
                    ":\n",
                    sprintf(
                        "  %s = %.2f, df = %d, p = %f\n",
                        names(m$statistic),
                        m$statistic,
                        m$parameter,
                        m$p.value
                    )
                )
            }

            parts <- c(parts, list(paste0(cmh.stat, collapse = "")))
        }

        for (o in names(obj[[this]])) {
            pl <- obj[[this]][[o]]

            xtype <- vartypes[[vnames$x]]
            header <- switch(xtype,
                "numeric" = {
                    if ("y" %in% names(vnames)) {
                        switch(vartypes[[vnames$y]],
                            "numeric" = {
                                sprintf("%s of %s versus %s",
                                    stype, vnames$y, vnames$x
                                )
                            },
                            "factor" = {
                                sprintf("%s of %s by %s",
                                    stype, vnames$x, vnames$y
                                )
                            }
                        )
                    } else {
                        sprintf("%s of %s", stype, vnames$x)
                    }
                },
                "factor" = {
                    if ("y" %in% names(vnames)) {
                        switch(vartypes[[vnames$y]],
                            "numeric" = {
                                sprintf("%s of the distribution of %s by %s",
                                    stype, vnames$x, vnames$y
                                )
                            },
                            "factor" = {
                                sprintf(
                                    "%s of the distribution of %s (%s) by %s (%s)",
                                    stype,
                                    vnames$x,
                                    switch(table.direction,
                                        vertical = "rows",
                                        horizontal = "columns"
                                    ),
                                    vnames$y,
                                    switch(table.direction,
                                        vertical = "columns",
                                        horizontal = "rows"
                                    )
                                )
                            }
                        )
                    } else {
                        sprintf("%s of the distribution of %s", stype, vnames$x)
                    }
                }
            )

            if (o != "all") {
                parts <- c(parts, list(out_rule("-", width)))
                header <- paste0(header, ", for ", vnames$g1, " = ", o)
            }
            header <- paste0(header, ":")

            parts <- c(parts, list(out_h2(header), out_blank()))

            pl.design <- if (is.survey) design.list[[this]][[o]] else NULL

            result <- switch(summary.type,
                "summary" =
                    summary(pl, opts = inzpars,
                        vn = vnames, des = pl.design,
                        survey.options = survey.options,
                        privacy_controls = privacy_controls,
                        table.direction = table.direction
                    ),
                "inference" =
                    inference(pl, bs, inzclass,
                        opts = inzpars,
                        des = pl.design,
                        width = width,
                        vn = vnames,
                        nb = attr(obj, "nboot"),
                        hypothesis = hypothesis,
                        survey.options = survey.options,
                        privacy_controls = privacy_controls,
                        table.direction = table.direction,
                        ...
                    )
            )

            parts <- c(parts, list(result, out_blank()))
        }

        parts <- c(parts, list(out_blank()))
    }

    ## --- Footer ---
    parts <- c(parts, list(out_rule("=", width), out_blank(), out_blank()))

    doc <- do.call(out_doc, c(parts, list(width = width)))
    out <- format_plain(doc, width = width)
    attr(out, "doc") <- doc
    class(out) <- "inzight.plotsummary"
    out
}

summary.inzdata <- function(object, des, width = 100, ...) {
    dataset_name <- ifelse(
        is.null(attr(object, "name", exact = TRUE)),
        "dataset",
        paste0("\"", attr(object, "name", exact = TRUE), "\"")
    )

    n.numeric <- sum(sapply(object, is.numeric))
    n.factor <- sum(!sapply(object, is.numeric))

    ## Build numeric variables section
    num_section <- NULL
    if (n.numeric > 0) {
        numvars <- object[, sapply(object, is.numeric), drop = FALSE]
        num_mat <- do.call(rbind,
            lapply(numvars,
                function(x) {
                    c(min(x, na.rm = TRUE), max(x, na.rm = TRUE), sum(is.na(x)))
                }
            )
        )
        num_section <- out_group(
            out_h2("Numeric variables:"),
            out_blank(),
            out_table(num_mat,
                col_headers = c("min", "max", "n. missing"),
                row_headers = names(numvars)
            ),
            out_blank()
        )
    }

    ## Build categorical variables section
    cat_section <- NULL
    if (n.factor > 0) {
        catvars <- object[, !sapply(object, is.numeric), drop = FALSE]
        cat_mat <- do.call(rbind,
            lapply(catvars,
                function(x) {
                    nlev <- length(levels(x))
                    c(nlev, sum(is.na(x)))
                }
            )
        )
        cat_section <- out_group(
            out_blank(),
            out_h2("Categorical variables:"),
            out_blank(),
            out_table(cat_mat,
                col_headers = c("n. categories", "n. missing"),
                row_headers = names(catvars)
            ),
            out_blank()
        )
    }

    doc <- out_doc(
        out_h1(
            sprintf("iNZight summary of %s", dataset_name),
            width
        ),
        out_kv(
            "Number of observations (rows)" = nrow(object),
            "Number of variables (columns)" = sprintf(
                "%s (%s numeric and %s categorical)",
                ncol(object), n.numeric, n.factor
            )
        ),
        out_blank(),
        out_rule("=", width),
        num_section,
        cat_section,
        out_rule("=", width),
        width = width
    )
    class(doc) <- c("inzight.plotsummary", class(doc))
    doc
}


#' @export
print.inzight.plotsummary <- function(x, ...) {
    if (inherits(x, "out_doc")) {
        cat(format(x, format = "plain"), sep = "\n")
    } else {
        cat(x, sep = "\n")
    }
}




centerText <- function(x, width) {
    len <- nchar(x)
    pad <- floor((width - len) / 2)
    paste0(paste0(rep(" ", pad), collapse = ""), x)
}


default.survey.options <- list(
    deff = TRUE
)
