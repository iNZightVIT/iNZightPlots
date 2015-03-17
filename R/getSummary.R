getPlotSummary <- function(x, y = NULL, g1 = NULL, g1.level = NULL,
                           g2 = NULL, g2.level = NULL, varnames = list(),
                           colby = NULL, sizeby = NULL,
                           data = NULL, design = NULL, freq = NULL,
                           missing.info = TRUE, inzpars = inzpar(), ...) {

    ## Grab a plot object!
    m <- match.call(expand.dots = FALSE)
    env <- parent.frame()

    if ("design" %in% names(m)) {
        md <- eval(m$design, env)
    } else {
        md <- eval(m$data, env)
    }

    ## fix up some subsetting group stuff
    if (is.null(m$g1)) {
        if (!is.null(g2)) {
            if (length(varnames) > 0) {
                varnames$g1 <- varnames$g2
                varnames$g2 <- NULL
            }
            iNZightPlot(x = x, y = y, g1 = g2, g1.level = g2.level, g2 = NULL, g2.level = NULL,
                        varnames = varnames, colby = colby, sizeby = sizeby, data = data,
                        design = design, freq = freq, missing.info = missing.info,
                        xlab = xlab, ylba= ylab, new = new, inzpars = inzpars,
                        layout.only = layout.only, plot = plot, env = env, ...)
        }
    }
    
    ## we now want to create a data object which contains *ALL* of the necessary
    ## information, including survey design, or frequency information:
    if (!"df" %in% ls())
        df <- inzDataframe(m, data = md, names = varnames, g1.level, g2.level, env = env)
    
    obj <- iNZightPlot(x = x, y = y, g1 = g1, g1.level = g1.level,
                       g2 = g2, g2.level = g2.level, varnames = varnames,
                       colby = colby, sizeby = sizeby,
                       data = data, design = design, freq = freq,
                       missing.info = missing.info, inzpars = inzpars,
                       plot = FALSE, df = df, ...)


    ### Now we just loop over everything ...

    summary(obj)
}


summary.inzplotoutput <- function(object, width = 100) {
    obj <- object  ## same typing ... but match default `summary` method arguments

    ## set up some variables/functions to make text processing easier ...
    
    out <- character()
    rule <- function(char, width)
        paste0(rep(char, width), collapse = "")
    Hrule <- rule("=", width)
    hrule <- rule("-", width)
    srule <- rule("*", width)
    center <- function(x, width) {
        len <- nchar(x)
        pad <- floor((width - len) / 2)
        paste0(paste0(rep(" ", pad), collapse = ""), x)
    }
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

    
    add(Hrule)
    add(center("iNZight Summary", width))
    add(hrule)

    ## A tidy header that formats the vames of the variables
    mat <- cbind(ind("Primary variable of interest: "),
                 paste0(vnames$x, " (", vartypes[[vnames$x]], ")"))
    if ("y" %in% names(vnames))
        mat <- rbind(mat, cbind(ind("Secondary variable: "),
                                paste0(vnames$y,  " (", vartypes[[vnames$y]], ")")))

    wg <- c("g1", "g2") %in% names(vnames)

    if (is.null(g.levels$g2[1]))
        wg[2] <- FALSE
    
    if (any(wg)) {
        mat <- rbind(mat, "")
        mat <- rbind(mat, cbind(ind("Subset by: "),
                                do.call(paste, c(vnames[c("g1", "g2")[wg]], list(sep = " and ")))))
    }

    mat <- rbind(mat, "",
                 cbind("Total number of observations: ", total.obs))
    if (total.missing > 0) {
        allnames <- c("x", "y", "g1", "g2")
        nn <- allnames[allnames %in% names(missing)]
        nn <- nn[sapply(missing[nn], function(m) m > 0)]
        mat <- rbind(mat,
                     cbind(ind("Number ommitted due to missingness: "),
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
    mat <- cbind(format(mat[, 1], justify = "right"), mat[, 2])
    apply(mat, 1, add)


    add(Hrule)
    add("")
    
    
    ## Cycle through G2 first
    lapply(names(obj), function(this) {
        if (this != "all") {
            add(Hrule)
            add(ind("For the subset where ", 5), vnames$g2, " = ", this)
            #add(Hrule)
        }
        
        lapply(names(obj[[this]]), function(o) {
            pl <- obj[[this]][[o]]

            xtype <- vartypes[[vnames$x]]
            header <- switch(xtype,
                             "numeric" = {
                                 if ("y" %in% names(vnames)) {
                                     switch(vartypes[[vnames$y]],
                                            "numeric" = {
                                                sprintf("Summary of %s versus %s",
                                                        vnames$y, vnames$x)
                                            },
                                            "factor" = {
                                                sprintf("Summary of %s by %s",
                                                        vnames$x, vnames$y)
                                            })
                                 } else {
                                     sprintf("Summary of %s", vnames$x)
                                 }
                             },
                             "factor" = {
                                 if ("y" %in% names(vnames)) {
                                     switch(vartypes[[vnames$y]],
                                            "numeric" = {
                                                sprintf("Summary of the distribution of %s by %s",
                                                        vnames$x, vnames$y)
                                            },
                                            "factor" = {
                                                sprintf("Summary of the distribution of %s by %s",
                                                        vnames$x, vnames$y)
                                            })
                                 } else {
                                     sprintf("Summary of the distribution of %s", vnames$x)
                                 }
                             })

            if (o != "all") {
                add(hrule)
                header <- paste0(header, paste0(", for ", vnames$g1, " = ", o))
            }
            header <- paste0(header, ":")
            
            add(header, underline = TRUE)
            add("")

            sapply(summary(pl, vnames), add)
            
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



print.inzight.plotsummary <- function(text) {
    cat(text, sep = "\n")
}
