create.inz.dotplot <- function(obj, hist = FALSE) {
    df <- obj$df
    opts <- obj$opts
    xattr <- obj$xattr

    boxplot <- opts$boxplot
    
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
        dfi <- subset(df, id == i)
        dfi$y <- NULL
        
        if (xattr$class == "inz.freq")
            di <- svydesign(ids=~1, weights = dfi$freq, data = dfi)
        else if (xattr$class == "inz.survey") {
            di <- eval(parse(text = modifyData(obj$df$call, "dfi")))
        } else {
            di <- dfi
        }

        out[[i]] <- di
    }

    boxinfo <- if (boxplot) boxSummary(out, opts) else NULL

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
    nbins <- if (hist) {
        ## some option here to adjust the number of bins (e.g., sample size < 100?)
        
        opts$hist.bins
    } else {
        wd <- convertWidth(unit(1.2 * opts$cex.dotpt, "char"),
                           "npc", valueOnly = TRUE)
        floor(1 / (wd))
    }

    plist <- lapply(out, makeHist, nbins = nbins, xlim = xattr$xrange)
    
    out <- list(toplot = plist, n.missing = n.missing, boxinfo = boxinfo,
                nacol = if ("colby" %in% v) any(sapply(plist, function(T) is.na(T$colby))) else FALSE,
                xlim = if (nrow(df) > 0) range(df$x, na.rm = TRUE) else c(-Inf, Inf),
                ylim = c(0, max(sapply(plist, function(p) if (is.null(p)) 0 else max(p$counts)))))

    class(out) <- ifelse(hist, "inzhist", "inzdot")
    
    out
}

plot.inzdot <- function(obj, gen, hist = FALSE) {
    # First step is to grab stuff:
    xlim <- current.viewport()$xscale
    ylim <- current.viewport()$yscale
    opts <- gen$opts
    mcex <- gen$mcex
    col.args <- gen$col.args
    boxplot <- opts$boxplot

    toplot <- obj$toplot
    boxinfo <- obj$boxinfo

    nlev <- length(toplot)
    pushViewport(viewport(layout = grid.layout(nrow = nlev),
                          name = "VP:dotplot-levels"))
    Hgts <- if (boxplot) c(3, 1) else c(1, 0)
    dpLayout <- grid.layout(nrow = 2, heights = unit(Hgts, "null"))

    # we need to make the dots stack nicely, if they fit
    maxcount <- gen$maxcount
    seekViewport("VP:dotplot-levels")
    pushViewport(viewport(layout.pos.row = 1))
    pushViewport(viewport(layout = dpLayout))
    pushViewport(viewport(layout.pos.row = 1))  # this is where dots will go

    ht <- convertHeight(unit(opts$cex.dotpt, "char"),
                        "npc", valueOnly = TRUE)
    ny <- floor(convertHeight(unit(1, "npc"),
                              "npc", valueOnly = TRUE) / ht)

    maxdots <- max(ny, maxcount)
    ylim <- c(0, maxdots * 1.05)

    for (i in 1:nlev) {
        pp <- toplot[[i]]
        
        seekViewport("VP:dotplot-levels")
        pushViewport(viewport(layout.pos.row = i))
        pushViewport(viewport(layout = dpLayout))
        
        if (boxplot) {
            pushViewport(viewport(layout.pos.row = 2, xscale = xlim))
            addBoxplot(boxinfo[[i]])
            upViewport()
        }
        
        pushViewport(viewport(layout.pos.row = 1,
                              xscale = xlim, yscale = ylim,
                              name = paste0("VP:plotregion-", i)))

        if (length(pp$x) > 0)
            grid.points(pp$x, pp$y,
                        gp =
                        gpar(col = colourPoints(obj$colby, col.args, opts),
                             cex = opts$cex.dotpt, lwd = opts$lwd.pt,
                             alpha = opts$alpha, fill = obj$fill.pt))
    }

    seekViewport("VP:dotplot-levels")
    upViewport()
}





boxSummary <- function(obj, opts) {
    lapply(obj, function(o) {
        if (is.null(o))
            return(NULL)

        if (!inherits(o, "survey.design")) {
            if (nrow(o) < 5) return(NULL)
            svyobj <- try(svydesign(ids=~1, weights = rep(1, nrow(o)), data = o))
        } else {
            if (nrow(o$variables) < 5) return(NULL)
            svyobj <- o
        }

        if (inherits(svyobj, "try-error"))
            return(NULL)

        list(quantiles = svyquantile(~x, svyobj, quantiles = c(0.25, 0.5, 0.75)),
             min = min(svyobj$variables$x, na.rm = TRUE),
             max = max(svyobj$variables$x, na.rm = TRUE),
             inference = FALSE, opts = opts)
    })
}



addBoxplot <- function(x) {
    opts <- x$opts
    if (is.null(x))
        return()
    
    xx <- rep(x$quantiles, each = 4)[3:10]
    yy <- rep(c(0.2, 0.8, 0.8, 0.2), 2)
    id <- rep(1:2, each = 4)
    grid.polygon(unit(xx, "native"), unit(yy, "npc"), id = id,
                 gp = gpar(lwd = opts$box.lwd[1]))
    grid.polyline(unit(c(x$min, x$quantiles[1], x$quantiles[3], x$max), "native"),
                  rep(0.5, 4), id = rep(1:2, each = 2),
                  gp = gpar(lwd = opts$box.lwd[2]))
                  
}
