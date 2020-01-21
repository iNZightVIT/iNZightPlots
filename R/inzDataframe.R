inzDataframe <- function(m, data = NULL, names = list(),
                         g1.level, g2.level, env) {

    # This function takes the given arguments and converts them into a
    # data frame for easy use by iNZightPlot.
    # It returns an object with a class: `inz.(simple|freq|survey)`

    extra.info <- list()

    if ("g2" %in% names(m) & (!("g1" %in% names(m)) | is.null(m$g1))) {
        if (!is.null(m$g2)) {
            if (is.null(g2.level) || g2.level == "_ALL") {
                m$g1 <- NULL
                m$g2 <- NULL

                m$g1.level <- NULL
                m$g2.level <- NULL

                g1.level <- NULL
                g2.level <- NULL

                names$g1 <- NULL
                names$g2 <- NULL
            } else {
                m$g1.level <- NULL
                names(m) <- gsub("^g2", "g1", names(m))

                g1.level <- g2.level
                g2.level <- NULL

                names$g1 <- names$g2
                names$g2 <- NULL
            }
        }
    }

    names <- names[!sapply(names, is.null)]

    # the variables we want to look for in argument list (m)
    vars <- c(
        "",
        "x",
        "y",
        "g1",
        "g2",
        "colby",
        "sizeby",
        "symbolby",
        "locate"
    )
    mw <- names(m) %in% vars
    mw[1] <- FALSE  # the function name
    mw <- mw & !sapply(as.list(m), is.null)

    # take the names and replace if specified
    names <- names[names != ""]
    varnames <- utils::modifyList(as.list(m[mw]), as.list(names))

    df <- list()  # initialise the object

    ## ----- DATA TYPES:
    # here, it is possible to add new data types (add the necessary conditions,
    # etc, and then simply add the appropriate class)
    # e.g., in future we might want to add a TimeSeries data type ...

    if (is_survey(data)) {
        # print(data$variables)
        df$data <- as.data.frame(lapply(m[mw], eval, data$variables, env))
        newDat <- cbind(data$variables, df$data)
        # newcall <- modifyCall(data$call, "data", "newDat")
        data$variables <- newDat
        df$design <- data# (eval(parse(text = newcall)))
        class(df) <- "inz.survey"
    } else if ("freq" %in% names(m)) {
        df$data <- as.data.frame(
            lapply(m[mw & names(m) != "sizeby"], eval, data, env)
        )
        df$freq <- eval(m$freq, data, env)
        df$max.freq <- max(df$freq)
        class(df) <- "inz.freq"
    } else {
        df$data <- as.data.frame(lapply(m[mw], eval, data, env))
        class(df) <- "inz.simple"
    }

    if (!is.null(m$locate.id)) {
        label <- character(nrow(df$data))
        if (is.null(df$data$locate)) {
            if (is.null(m$locate.col))
                locCol <- "default"
            else
                locCol <- m$locate.col
            label[eval(m$locate.id)] <- paste(" ")
        } else {
            locVar <- as.character(df$data$locate)
            locVar[is.na(locVar)] <- "missing"
            label[eval(m$locate.id)] <- locVar[eval(m$locate.id)]
        }
        df$data$locate <- label
    } else if (!is.null(m$locate.extreme)) {
        label <- character(nrow(df$data))
        if (!is.null(df$data$locate)) {
            locVar <- as.character(df$data$locate)
            locVar[is.na(locVar)] <- "missing"
            label <- locVar
        } else {
            label <- rep(" ", nrow(df$data))
        }
        df$data$extreme.label <- label
        df$data$pointIDs <- 1:nrow(df$data)
    }

    if (!is.null(m$highlight)) {
        df$data$highlight <- (1:nrow(df$data)) %in% m$highlight
    }



    # convert anything that isn't a numeric variable to a factor
    # NOTE: this is just precautionary; as.data.frame should set any
    # character strings to factors by default.
    needs_transform <- function(x) {
        if (is.factor(x)) return(FALSE)

        if (is.numeric(x) && class(x) %in% c("integer", "numeric"))
            return(FALSE)

        ## anything else
        TRUE
    }
    makeF <- sapply(df$data, needs_transform)
    trans <- list()
    trans.extra <- list()
    if (any(makeF))
        for (i in colnames(df$data)[makeF]) {
            if (inherits(df$data[[i]], "Date")) {
                trans[[i]] <- "date"
                if (i %in% c("g1", "g2")) {
                    if (length(unique(df$data[[i]])) < 10) {
                        df$data[[i]] <- as.factor(df$data[[i]])
                    } else {
                        lvls <- scales::pretty_breaks(8)(df$data[[i]])
                        labs <- names(lvls)
                        labs <- paste(labs[-length(labs)], labs[-1], sep = " to ")
                        df$data[[i]] <- cut(df$data[[i]], lvls, labs)
                    }
                } else if (i == "colby" && length(unique(df$data[[i]]) < 10)) {
                    df$data[[i]] <- as.factor(df$data[[i]])
                } else if (i == "symbolby" &&
                           length(unique(df$data[[i]] < 6))) {
                    df$data[[i]] <- as.factor(df$data[[i]])
                } else {
                    df$data[[i]] <- as.numeric(df$data[[i]])
                }
            } else if (inherits(df$data[[i]], "POSIXct") ||
                       inherits(df$data[[i]], "times")) {
                trans[[i]] <-
                    ifelse(
                        inherits(df$data[[i]], "POSIXct"),
                        "datetime", "time"
                    )
                trans.extra[[i]]$tz <- attr(df$data[[i]], "tzone")
                if (i %in% c("g1", "g2")) {
                    ## convert datetime to factor ...
                    lvls <- scales::pretty_breaks(4)(df$data[[i]])
                    labs <- names(lvls)
                    labs <- paste(labs[-length(labs)], labs[-1], sep = " to ")
                    df$data[[i]] <- cut(df$data[[i]], lvls, labs)
                } else {
                    df$data[[i]] <- as.numeric(df$data[[i]])
                }
            } else {
                df$data[[i]] <- as.factor(df$data[[i]])
            }
        }
    if (length(trans.extra)) trans$extra <- trans.extra
    if (length(trans)) {
        # switch x and y axis transform if x is factor and y is numeric
        if (all(c("x", "y") %in% colnames(df$data))) {
            if (is_cat(df$data$x) && is_num(df$data$y)) {
                tr_x <- trans$x
                trans$x <- trans$y
                trans$y <- tr_x
            }
        }

        df$transformations <- trans
    }

    # convert any -Inf/Inf values to NA's
    # this is likely to occur if the user supplies a transformed value
    # such as 1 / x, or log(x) (which give Inf and -Inf respectively).
    # Because we can't plot these values, it is easier just to replace them
    # with missing.
    for (i in colnames(df$data))
        df$data[[i]][is.infinite(df$data[[i]])] <- NA

    # convert numeric grouping variables to factors
    if ("g2" %in% colnames(df$data))
        if (!is.factor(df$data$g2))
            df$data$g2 <- convert.to.factor(df$data$g2)
    if ("g1" %in% colnames(df$data)) {
        if (!is.factor(df$data$g1))
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

    if ("colby" %in% colnames(df$data)) {
        if (is.factor(df$data$colby)) {
            if (length(levels(df$data$colby)) == 1)
                df$data$colby <- varnames$data$colby <- NULL
        } else {
            if (length(unique(df$data$colby)) == 1)
                df$data$colby <- varnames$data$colby <- NULL
        }

    }
    if ("symbolby" %in% colnames(df$data)) {
        df$data$symbolby <- convert.to.factor(df$data$symbolby)
        if (length(levels(df$data$symbolby)) == 1 |
            length(levels(df$data$symbolby)) > 5) {
            df$data$symbolby <- varnames$data$symbolby <- NULL
        }
    }


    if ("extra.vars" %in% names(m)) {
        fun.list <- attr(m$extra.vars, "fun")
        if (is.character(m$extra.vars))
            sapply(m$extra.vars, function(v) {
                       tmp <- data[v]
                       if (!is.null(fun.list))
                           if (!is.null(fun.list[[v]]))
                               tmp <- fun.list[[v]](tmp)
                       df$data[v] <<- tmp
                   })
        else
            warning("`extra.vars` must be supplied as a character vector.")
    }

    # fix a bug that ensures colby grouping variable is the same as g2
    # if both specified
    if ("g2" %in% colnames(df$data) & "colby" %in% colnames(df$data))
        if (varnames$g2 == varnames$colby)
            df$data$colby <- df$data$g2

    df$varnames <-
        sapply(
            varnames,
            function(x) ifelse(!is.character(x), deparse(x), x)
        )
    df$glevels <- list(g1.level = g1.level, g2.level = g2.level)

    if (!is.null(df$design)) {
        if ("g1" %in% colnames(df$data))
            df$design <- update(df$design, g1 = df$data$g1)
        if ("g2" %in% colnames(df$data))
            df$design <- update(df$design, g2 = df$data$g2)
    }

    df
}
