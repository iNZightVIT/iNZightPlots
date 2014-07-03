create.inz.dotplot <- function(obj, hist = FALSE) {
    df <- obj$df
    opts <- obj$opts
    xattr <- obj$xattr
    
    v <- colnames(df)
    vn <- xattr$varnames

    if (xattr$class != "inz.simple")
        hist <- TRUE

    if (xattr$class == "inz.survey")
        df <- df$variables

    # May need to switch around X and Y:
    if (all(c("x", "y") %in% v)) {
        if (is.factor(df$x) & is.numeric(df$y)) {
            X <- df$y
            df$y <- df$x
            df$x <- X
            Xn <- vn$y
            vn$y <- vn$x
            vn$x <- Xn
            xattr$xrange <- xattr$yrange
            xattr$yrange <- NULL
        }
    }
    
    # first need to remove missing values
    missing <- is.na(df$x)
    if ("y" %in% colnames(df)) {
        y.levels <- levels(df$y) # need to save these before we remove missing ...
        missing <- missing | is.na(df$y)
    }
    
    n.missing <- sum(missing)
    df <- df[!missing, , drop = FALSE]
    
    ## Return a LIST for each level of y
    if ("y" %in% colnames(df)) {
        out <- vector("list", length(levels(df$y)))
        names(out) <- levels(df$y)
        id <- df$y
    } else {
        out <- list(all = NULL)
        id <- rep("all", nrow(df))
    }

    for (i in unique(id)) {
        dfi <- subset(df, id = i)
        dfi$y <- NULL
        
        if (xattr$class == "inz.freq")
            di <- svydesign(ids=~1, weights = df$freq, data = df)
        else if (xattr$class == "inz.survey") {
            di <- eval(parse(text = modifyData(obj$df$call, "df")))
        } else {
            di <- dfi
        }

        out[[i]] <- di
    }


    makeHist <- function(d, nbins, xlim) {
        if (is.null(d)) return(NULL)
        
      # Create even cut points in the given data range:
        range <- xlim
        range <- extendrange(range, f = 0.01) ## is this necessary?
        cuts <- seq(range[1] - 0.1, range[2] + 0.1, length = nbins + 1)
        bin.min <- cuts[-(nbins + 1)]
        bin.max <- cuts[-1]

        # Cut the data and calculate counts:
        if (inherits(d, "survey.design")) {
            ## To do this, we will pretty much grab stuff from the `survey` package, however it
            ## cannot be used separately to produce the bins etc without plotting it; so copyright
            ## for the next few lines goes to Thomas Lumley.
            h <- hist(x <- d$variables$x, breaks = cuts, plot = FALSE)

            # We can run into problems with PSUs have single clusters, so:
            oo <- options()$survey.lonely.psu
            options(survey.lonely.psu = "certainty")
            probs <- coef(svymean(~cut(d$variables$x, h$breaks, include.lowest = TRUE,
                                       deff = FALSE, estimate.only = TRUE), d, na.rm = TRUE))
            options(survey.lonely.psu = oo)
            
            h$density <- probs / diff(h$breaks)
            h$counts <- probs * sum(weights(d))
        } else {
            h <- hist(x <- d$x, breaks = cuts, plot = FALSE)
        }

        ret <- list(breaks = cuts,
                    counts = as.numeric(h$counts),
                    density = as.numeric(h$density),
                    mids = h$mids,
                    x = sort(x))
        
        if (!hist) {
            ret$y <- unlist(sapply(h$counts[h$counts != 0], function(c) 1:c))
            if ("colby" %in% colnames(d)) {
                ret$colby <- d$colby[order(d$x)]
            }
        }
        
        ret
    }

    plist <- lapply(out, makeHist, nbins = 20, xlim = xattr$xrange)
    
    out <- list(objs = plist, n.missing = n.missing,
                nacol = if ("colby" %in% v) any(sapply(plist, function(T) is.na(T$colby))) else FALSE,
                xlim = if (nrow(df) > 0) range(df$x, na.rm = TRUE) else c(-Inf, Inf),
                ylim = c(0, max(sapply(plist, function(p) if (is.null(p)) 0 else max(p$counts)))))

    class(out) <- ifelse(hist, "inzhist", "inzdot")
    
    out
}

plot.inzdot <- function(obj, gen) {
    
}
