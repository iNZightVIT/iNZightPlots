##' Generate summary or inference information for an iNZight plot
##'
##' Works much the same as \code{iNZightPlot}
##' @title iNZight Plot Summary and Inference
##' @param x a vector (numeric or factor), or the name of a column in the supplied
##' \code{data} or \code{design} object
##' @param y a vector (numeric or factor), or the name of a column in the supplied
##' \code{data} or \code{design} object
##' @param g1 a vector (numeric or factor), or the name of a column in the supplied
##' \code{data} or \code{design} object. This variable acts as a subsetting variable.
##' @param g1.level the name (or numeric position) of the level of \code{g1} that will be
##' used instead of the entire data set
##' @param g2 a vector (numeric or factor), or the name of a column in the supplied
##' \code{data} or \code{design} object. This variable acts as a subsetting variable, similar to
##' \code{g1}
##' @param g2.level same as \code{g1.level}, however takes the additional value \code{"_MULTI"},
##' which produces a matrix of \code{g1} by \code{g2}
##' @param varnames a list of variable names, with the list named using the appropriate arguments
##' (i.e., \code{list(x = "height", g1 = "gender")})
##' @param colby the name of a variable (numeric or factor) to colour points by. In the
##' case of a numeric variable, a continuous colour scale is used, otherwise each level of
##' the factor is assigned a colour
##' @param sizeby the name of a (numeric) variable, which controls the size of points
##' @param data the name of a data set
##' @param design the name of a survey object, obtained from the \code{survey} package
##' @param freq the name of a frequency variable if the data are frequencies
##' @param missing.info logical, if \code{TRUE}, information regarding missingness is
##' displayed in the plot
##' @param inzpars allows specification of iNZight plotting parameters over multiple plots
##' @param summary.type one of \code{"summary"} or \code{"inference"}
##' @param hypothesis.value H0 value for hypothesis test
##' @param hypothesis.alt alternative hypothesis (!=, <, >)
##' @param hypothesis.var.equal use equal variance assumption for t-test?
##' @param hypothesis.test in some cases (currently just two-samples) can perform multiple tests (t-test or ANOVA)
##' @param hypothesis either NULL for no test, or missing (in which case above arguments are used)
##' @param ... additional arguments, see \code{inzpar}
##' @return an \code{inzight.plotsummary} object with a print method
##' @author tell029
##' @export
getPlotSummary <- function(x, y = NULL, g1 = NULL, g1.level = NULL,
                           g2 = NULL, g2.level = NULL, varnames = list(),
                           colby = NULL, sizeby = NULL,
                           data = NULL, design = NULL, freq = NULL,
                           missing.info = TRUE, inzpars = inzpar(),
                           summary.type = "summary",
                           hypothesis.value = 0,
                           hypothesis.alt = c("two.sided", "less", "greater"),
                           hypothesis.var.equal = FALSE,
                           hypothesis.test = c("default", "t.test", "anova"),
                           hypothesis = list(value = hypothesis.value,
                                             alternative = match.arg(hypothesis.alt),
                                             var.equal = hypothesis.var.equal,
                                             test = match.arg(hypothesis.test)),
                           ...) {

    ## Grab a plot object!
    m <- match.call(expand.dots = FALSE)
    env <- parent.frame()

    if ("design" %in% names(m)) {
        md <- eval(m$design, env)
    } else {
        md <- eval(m$data, env)
    }

    varnames <- varnames

    ## Any varnames supplied that AREN'T needed must be removed, otherwise errors:
    nullVars <- sapply(as.list(m)[names(varnames)], is.null)
    varnames[nullVars] <- NULL
    
    ## fix up some subsetting group stuff
    if (is.null(m$g1)) {
        if (!is.null(m$g2)) {
            if (length(varnames) > 0) {
                varnames$g1 <- varnames$g2
                varnames$g2 <- NULL
            }
            
            getPlotSummary(x = x, y = y, g1 = g2, g1.level = g2.level, g2 = NULL, g2.level = NULL,
                           varnames = varnames, colby = colby, sizeby = sizeby, data = data,
                           design = design, freq = freq, missing.info = missing.info,
                           new = new, inzpars = inzpars,
                           env = env,
                           summary.type = summary.type, hypothesis = hypothesis, ...)
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
    ## dots <- list(...)
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
                       plot = FALSE, df = df, ...)

    ### Now we just loop over everything ...

    summary(obj, summary.type, hypothesis)
}


summary.inzplotoutput <- function(object, summary.type = "summary", hypothesis = NULL, width = 100) {
    if (length(summary.type) > 1) {
        warning("Only using the first element of `summary.type`")
        summary.type <- summary.type[1]
    }
    if (!summary.type %in% c("summary", "inference"))
        stop("`summary.type` must be either `summary` or `inference`")

    obj <- object  ## same typing ... but match default `summary` method arguments

    ## set up some variables/functions to make text processing easier ...
    
    out <- character()
    rule <- function(char, width)
        paste0(rep(char, width), collapse = "")
    Hrule <- rule("=", width)
    hrule <- rule("-", width)
    srule <- rule("*", width)
    center <- centerText
    ind <- function(x, indent = 3)
        paste0(paste0(rep(" ", indent), collapse = ""), x)
    
    add <- function(..., underline = FALSE) {
        x <- paste0(..., collapse = "")
        out <<- c(out, x)
        if (underline)
            out <<- c(out, rule("-", width = nchar(x)))
    }

    vnames <- attr(obj, "varnames")
    g.levels <- attr(obj, "glevels")
    vartypes <- attr(obj, "vartypes")
    missing <- attr(obj, "missing")
    total.missing <- attr(obj, "total.missing")
    total.obs <- attr(obj, "total.obs")
    bs <- attr(obj, "bootstrap")
    inzclass <- attr(obj, "inzclass")
    
    is.survey <- attr(obj, "inzclass") == "inz.survey"

    #if (is.survey & summary.type == "inference")
    #    return("Inference for Survey Designs not yet implemented.")
    
    add(Hrule)
    add(center(switch(summary.type,
                      "summary" =
                          paste0("iNZight Summary",
                                 ifelse(is.survey, " - Survey Design", "")),
                      "inference" =
                      paste("iNZight Inference using",
                            ifelse(bs,
                                   "the Nonparametric Bootstrap",
                                   "Normal Theory"))), width))
    add(hrule)

    scatter <- FALSE
    if ("y" %in% names(vnames)) {
        if (vartypes[[vnames$x]] == "numeric" & vartypes[[vnames$y]] == "numeric") {
            scatter <- TRUE
        }
    }

    ## A tidy header that formats the vames of the variables
    mat <- cbind(ind(ifelse(scatter, "Response/outcome variable: ", "Primary variable of interest: ")),
                 paste0(ifelse(scatter, vnames$y, vnames$x),
                        " (", gsub("factor", "categorical", vartypes[[ifelse(scatter, vnames$y, vnames$x)]]), ")"))
    
    if ("y" %in% names(vnames))
        mat <- rbind(mat, cbind(ind(paste0(ifelse(scatter,
                                                  "Predictor/explanatory", "Secondary"),
                                           " variable: ")),
                                paste0(ifelse(scatter, vnames$x, vnames$y),
                                       " (", gsub("factor", "categorical", vartypes[[ifelse(scatter, vnames$x, vnames$y)]]), ")")))

    wg <- c("g1", "g2") %in% names(vnames)

    if (is.null(g.levels$g2[1]))
        wg[2] <- FALSE
    
    if (any(wg)) {
        mat <- rbind(mat, "")
        mat <- rbind(mat, cbind(ind("Subset by: "),
                                do.call(paste, c(vnames[c("g1", "g2")[wg]], list(sep = " and ")))))
        #if (is.survey)
        #    mat <- rbind(mat, c("NOTE: ", "survey summaries are not yet reliable for subsets."))
    }

    mat <- rbind(mat, "",
                 cbind("Total number of observations: ", total.obs))
    if (total.missing > 0) {
        allnames <- c("x", "y", "g1", "g2")
        nn <- allnames[allnames %in% names(missing)]
        nn <- nn[sapply(missing[nn], function(m) m > 0)]
        mat <- rbind(mat,
                     cbind(ind("Number omitted due to missingness: "),
                           paste0(total.missing,
                                  if (length(missing) > 1) {
                                      paste0(" (",
                                             paste(sapply(nn, function(i) {
                                                 paste0(missing[[i]], " in ", vnames[[i]])
                                             }), collapse = ", "),
                                             ")")
                                  })),
                     cbind(ind("Total number of observations used: "),
                           total.obs - total.missing))
    }
    if (is.survey) {
        des <- attr(obj, "main.design")
        mat <- rbind(mat, cbind("Estimated population size: ",
                                paste0(round(coef(svytotal(matrix(rep(1, nrow(des$variables)), ncol = 1), des))),
                                       "  [standard error = ",
                                       signif(SE(svytotal(matrix(rep(1, nrow(des$variables)), ncol = 1), des))),
                                       "]")))
    }
    mat <- cbind(format(mat[, 1], justify = "right"), mat[, 2])
    apply(mat, 1, add)

    if (is.survey) {
        add(hrule)
        sapply(capture.output(attr(object, "main.design")), function(o) add(ind(o)))
        design.list <- attr(object, "design")
    }

    add(Hrule)
    add("")

    simpleCap <- function(x) {
        s <- strsplit(x, " ")[[1]]
        paste(toupper(substring(s, 1,1)), substring(s, 2),
              sep="", collapse=" ")
    }
    stype <- simpleCap(summary.type)

    if (!is.null(vnames$y) && vartypes[[vnames$x]] == "factor" && vartypes[[vnames$y]] == "numeric") {
        tmpx <- vnames$y
        vnames$y <- vnames$x
        vnames$x <- tmpx
    }
    
    ## Cycle through G2 first
    lapply(names(obj), function(this) {
        if (this != "all") {
            add(Hrule)
            add(ind("For the subset where ", 5), vnames$g2, " = ", this)
        }

        lapply(names(obj[[this]]), function(o) {
            pl <- obj[[this]][[o]]

            xtype <- vartypes[[vnames$x]]
            header <- switch(xtype,
                             "numeric" = {
                                 if ("y" %in% names(vnames)) {
                                     switch(vartypes[[vnames$y]],
                                            "numeric" = {
                                                sprintf("%s of %s versus %s",
                                                        stype, vnames$y, vnames$x)
                                            },
                                            "factor" = {
                                                sprintf("%s of %s by %s",
                                                        stype, vnames$x, vnames$y)
                                            })
                                 } else {
                                     sprintf("%s of %s", stype, vnames$x)
                                 }
                             },
                             "factor" = {
                                 if ("y" %in% names(vnames)) {
                                     switch(vartypes[[vnames$y]],
                                            "numeric" = {
                                                sprintf("%s of the distribution of %s by %s",
                                                        stype, vnames$x, vnames$y)
                                            },
                                            "factor" = {
                                                sprintf("%s of the distribution of %s (columns) by %s (rows)",
                                                        stype, vnames$x, vnames$y)
                                            })
                                 } else {
                                     sprintf("%s of the distribution of %s", stype, vnames$x)
                                 }
                             })

            if (o != "all") {
                add(hrule)
                header <- paste0(header, paste0(", for ", vnames$g1, " = ", o))
            }
            header <- paste0(header, ":")
            
            add(header, underline = TRUE)
            add("")

            pl.design <- if (is.survey) design.list[[this]][[o]] else NULL
            if (is.survey) hypothesis <- NULL  ## no hypothesis testing in survey designs (yet)

            sapply(switch(summary.type,
                          "summary" = summary(pl, vn = vnames, des = pl.design),
                          "inference" = inference(pl, bs, inzclass, des = pl.design,
                                                  width = width,
                                                  vn = vnames, nb = attr(obj, "nboot"),
                                                  hypothesis = hypothesis)),
                   add)
            
            add("")
        })

        add("")
    })

    add(Hrule)

    ## Notes:
    add("")
    add("")

    
    
    class(out) <- "inzight.plotsummary"
    out
}


##' @export
print.inzight.plotsummary <- function(x, ...) {
    cat(x, sep = "\n")
}




centerText <- function(x, width) {
    len <- nchar(x)
    pad <- floor((width - len) / 2)
    paste0(paste0(rep(" ", pad), collapse = ""), x)
}
