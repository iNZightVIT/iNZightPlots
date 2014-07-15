inzDataframe <- function(m, data = NULL, names = list(), g1.level, g2.level, env) {

  # This function takes the given arguments and converts them into a
  # data frame for easy use by iNZightPlot.
  # It returns an object with a class: `inz.(simple|freq|survey)`

    # the variables we want to look for in argument list (m)
    vars <- c("", "x", "y", "g1", "g2", "colby", "sizeby")
    mw <- names(m) %in% vars
    mw[1] <- FALSE  # the function name
    mw <- mw & !sapply(as.list(m), is.null)

    # take the names and replace if specified
    names <- names[names != ""]
    varnames <- modifyList(as.list(m[mw]), names)

    df <- list()  # initialise the object

    ## ----- DATA TYPES:
    # here, it is possible to add new data types (add the necessary conditions, etc,
    # and then simply add the appropriate class)
    # e.g., in future we might want to add a TimeSeries data type ...

    if (inherits(data, "survey.design")) {
        df$data <- as.data.frame(lapply(m[mw], eval, data$variables, env))
        df$design <- eval(data, env)
        class(df) <- "inz.survey"
    } else if ("freq" %in% names(m)) {
        df$data <- as.data.frame(lapply(m[mw & names(m) != "sizeby"], eval, data, env))
        df$freq <- eval(m$freq, data, env)
        df$max.freq <- max(df$freq)
        class(df) <- "inz.freq"
    } else {
        df$data <- as.data.frame(lapply(m[mw], eval, data, env))
        class(df) <- "inz.simple"
    }

    if (!is.null(df$data$sizeby)) {
        df$data$sizeby <- rescale(df$data$sizeby)
    }

    # convert anything that isn't a numeric variable to a factor
    # NOTE: this is just precautionary; as.data.frame should set any
    # character strings to factors by default.
    makeF <- sapply(df$data, function(x) !is.numeric(x) & !is.factor(x))
    if (any(makeF))
        for (i in colnames(df$data)[makeF])
            df$data[[i]] <- as.factor(df$data[[i]])

    # convert any -Inf/Inf values to NA's
    # this is likely to occur if the user supplies a transformed value such as 1 / x, or
    # log(x) (which give Inf and -Inf respectively). Because we can't plot these values,
    # it is easier just to replace them with missing.
    for (i in colnames(df$data))
        df$data[[i]][is.infinite(df$data[[i]])] <- NA

    # convert numeric grouping variables to factors
    if ("g2" %in% colnames(df$data))
        if (is.numeric(df$data$g2))
            df$data$g2 <- convert.to.factor(df$data$g2)
    if ("g1" %in% colnames(df$data)) {
        if (is.numeric(df$data$g1))
            df$data$g1 <- convert.to.factor(df$data$g1)
    } else {
        if (!is.null(g2.level)) {
            # g2.level can only be of length 1
            if (length(g2.level) > 1)
                stop("g2.level must be of length 1 or NULL")
        
            if (g2.level %in% c(length(levels(df$data$g2)) + 1, "_MULTI")) {
                # need to replace g1 with g2
                varnames$g1 <- varnames$g2
                varnames$g2 <- NULL
                df$data$g1 <- df$data$g2
                df$data$g2 <- NULL
                g1.level <- g2.level
                g2.level <- NULL
            }
        }
    }

    ## another check that there aren't too many levels of colby:
    if ("colby" %in% colnames(df$data)) {
        if (is.factor(df$data$colby)) {
            if (length(levels(df$data$colby)) > 10) {
                warning("Ignoring colby argument: too many factor levels.")
                df$data$colby <- NULL
                varnames$data$colby <- NULL
            }
        }
    }

    # fix a bug that ensures colby grouping variable is the same as g2 if both specified
    if ("g2" %in% colnames(df$data) & "colby" %in% colnames(df$data))
        if (varnames$g2 == varnames$colby)
            df$data$colby <- df$data$g2

    df$varnames <- sapply(varnames,
                          function(x) ifelse(!is.character(x), deparse(x), x))
    df$glevels <- list(g1.level = g1.level, g2.level = g2.level)

    df
}
