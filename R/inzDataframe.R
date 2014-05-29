inzDataframe <- function(m, data = NULL, names = list(), g1.level, g2.level,
                         structure = NULL, env) {
  # This function takes the given arguments and converts them into a
  # data frame for easy use by iNZightPlot.

    # the variables we want to look for in argument list (m)
    vars <- c("", "x", "y", "g1", "g2", "colby", "sizeby")
    mw <- names(m) %in% vars
    mw[1] <- FALSE  # the function name

    # take the names and replace if specified
    varnames <- modifyList(as.list(m[mw]), names)
    
    df <- as.data.frame(lapply(m[mw], eval, data, env))
    if (!is.null(structure)) {
        if (structure$type == "freq") {
            if (length(f <- structure$freqs) != nrow(df))
                stop("Structural information doesn't match the data (different lengths).")

            df$`(freqs)` <- f
            attr(df, "max.freq") <- max(f)
            if ("sizeby" %in% colnames(df)) {
                df$sizeby <- NULL
            }
        } else {
            
        }
    }

    if (!is.null(df$sizeby)) {
        df$sizeby <- rescale(df$sizeby)
    }

    # convert anything that isn't a numeric variable to a factor
    # NOTE: this is just precautionary; as.data.frame should set any
    # character strings to factors by default.
    makeF <- sapply(df, function(x) !is.numeric(x) & !is.factor(x))
    if (any(makeF))
        for (i in colnames(df)[makeF])
            df[[i]] <- as.factor(df[[i]])

    # convert any -Inf/Inf values to NA's
    # this is likely to occur if the user supplies a transformed value such as 1 / x, or
    # log(x) (which give Inf and -Inf respectively). Because we can't plot these values,
    # it is easier just to replace them with missing.
    for (i in colnames(df))
        df[[i]][is.infinite(df[[i]])] <- NA


    # convert numeric grouping variables to factors
    if ("g2" %in% colnames(df))
        if (is.numeric(df$g2))
            df$g2 <- convert.to.factor(df$g2)
    if ("g1" %in% colnames(df)) {
        if (is.numeric(df$g1))
            df$g1 <- convert.to.factor(df$g1)
    } else {
        if (!is.null(g2.level)) {
            # g2.level can only be of length 1
            if (length(g2.level) > 1)
                stop("g2.level must be of length 1 or NULL")
        
            if (g2.level %in% c(length(levels(df$g2)) + 1, "_MULTI")) {
                # need to replace g1 with g2
                varnames$g1 <- varnames$g2
                varnames$g2 <- NULL
                df$g1 <- df$g2
                df$g2 <- NULL
            }
        }
    }

    ## another check that there aren't too many levels of colby:
    if ("colby" %in% colnames(df)) {
        if (is.factor(df$colby)) {
            if (length(levels(df$colby)) > 10) {
                warning("Ignoring colby argument: too many factor levels.")
                df$colby <- NULL
                varnames$colby <- NULL
            }
        }
    }

    # fix a bug that ensures colby grouping variable is the same as g2 if both specified
    if ("g2" %in% colnames(df) & "colby" %in% colnames(df))
        if (varnames$g2 == varnames$colby)
            df$colby <- df$g2

    attr(df, "varnames") <- sapply(varnames,
                                   function(x) ifelse(!is.character(x), deparse(x), x))
    df
}
    
