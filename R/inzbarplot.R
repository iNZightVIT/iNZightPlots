create.inz.barplot <- function(obj) {
    # take the dataframe and settings from the object
    df <- obj$df
    opts <- obj$opts
    xattr <- obj$xattr

    ZOOM <- xattr$zoom

    inf.type <- opts$inference.type
    inf.par <- "proportion"
    bs <- opts$bs.inference

    if (xattr$class == "inz.survey")
        df <- df$variables

    ynull <- !"y" %in% colnames(df)

    # first need to remove missing values
    missing <- is.na(df$x)
    if ("y" %in% colnames(df)) {
        y.levels <- levels(df$y) # need to save these before we remove missing ...
        missing <- missing | is.na(df$y)
    }
    
    n.missing <- sum(missing)
    df <- df[!missing, , drop = FALSE]

    svy <- switch(xattr$class,
                  "inz.survey" = {
                      eval(parse(text = modifyData(obj$df$call, "df")))
                  }, "inz.freq" = {
                      svydesign(ids=~1, weights = ~freq, data = df)
                  }, "inz.simple" = {
                      NULL
                  })

    SEG <- FALSE
    if (ynull) {
        if (is.null(svy))
            tab <- table(df$x)
        else
            tab <- svytable(~x, design = svy)
        phat <- matrix(tab / sum(tab), nrow = 1)
        widths <- rep(1, length(tab))
        edges <- c(0, 1)

        ## colby: (segmented bar plot)
        SEG <- "colby" %in% colnames(df)
        if (SEG & !is.factor(df$colby))
            SEG <- FALSE
        
        if (SEG) {
            tab2 <-
                if (is.null(svy))
                    table(df$colby, df$x)
                else
                    svytable(~colby + x, design = svy)
            p2 <- sweep(tab2, 2, colSums(tab2), "/")
        }
    } else {
        if (is.null(svy))
            tab <- table(df$y, df$x)
        else
            tab <- svytable(~y + x, design = svy)
        phat <- sweep(tab, 1, nn <- rowSums(tab), "/")
        widths <- nn / sum(nn)
        edges <- c(0, cumsum(widths))
    }

    ## Cannot have inference on segmented plot (too complicated for now)
    inflist <- if (!SEG) barinference(obj, tab, phat) else NULL

    if (!is.null(ZOOM)) {
        if (ZOOM[1] > ncol(phat))
            next
        
        ww <- ZOOM[1]:min(sum(ZOOM) - 1, ncol(phat))
        phat <- phat[, ww, drop = FALSE]
        if (ynull) {
            tab <- tab[ww]
            widths <- widths[ww]
        } else {
            tab <- tab[, ww, drop = FALSE]
        }
    }

    out <- list(phat = phat, tab = tab, widths = widths, edges = edges, nx = ncol(phat),
                full.height = opts$full.height, inference.info = inflist,
                xlim = c(0, if (ynull) length(tab) else ncol(tab)),
                ylim = c(0, max(phat, if (!is.null(inflist)) attr(inflist, "max"), na.rm = TRUE)))
    if (SEG) out$p.colby <- p2[nrow(p2):1, ]
    if (!is.null(ZOOM)) out$zoom.index <- ww
    
    class(out) <- "inzbar"

    out
}

plot.inzbar <- function(obj, gen) {
    opts <- gen$opts
    p <- obj$phat
    nx <- obj$nx

    inflist <- obj$inference.info

    if (SEG <- !is.null(obj$p.colby)) {
        seg.cols <- gen$col.args$f.cols
    }

    edges <- rep(obj$edges * 0.9 + 0.05, each = 4)
    edges <- edges[3:(length(edges) - 2)]
    xx <- rep(edges, nx) + rep(1:nx - 1, each = 4 * nrow(p))
    
    if (SEG) {
        xx <- rep(xx, length(seg.cols))
        tops <- apply(p, 2, function(x) rbind(0, x, x, 0))
        
        yy <- rep(tops, length(seg.cols))
        ps <- rep(c(t(obj$p.colby)), each = 4)
        pT <- rep(c(t(apply(obj$p.colby, 2, cumsum))), each = 4)
        yy <- yy * pT
        
        ## reverse the order, so the short ones are drawn last!       
        id <- rev(rep(1:prod(dim(obj$p.colby)), each = 4))
        colz <- rep(seg.cols, each = nx)
        
        grid.polygon(unit(xx, "native"), unit(yy, "native"), id = id,
                     gp =
                     gpar(fill = colz, col = "transparent",
                          lwd = 0))

        ## separating lines
        mat <- apply(sweep(obj$p.colby, 2, tops[2, ], "*"), 2, cumsum)
        mat <- mat[-nrow(mat), , drop = FALSE]  # drop the last one

        yl <- rep(c(mat), each = 2)
        xl <- rep(edges[2:3], length = length(yl)) + rep(1:nx - 1, each = 2 * nrow(mat))
        id <- rep(1:length(c(mat)), each = 2)

        grid.polyline(xl, yl, default.units = "native", id = id,
                      gp = gpar(col = opts$bar.col, lwd = opts$bar.lwd))                          
    }
    
    xx <- rep(edges, nx) + rep(1:nx - 1, each = 4 * nrow(p))
    tops <- apply(p, 2, function(x) rbind(0, x, x, 0))
    yy <- c(tops)
    
    id <- rep(1:prod(dim(p)), each = 4)
    colz <- if (is.null(gen$col.args$b.cols)) opts$bar.fill else rep(gen$col.args$b.cols, nx)
   
    grid.polygon(unit(xx, "native"), unit(yy, "native"), id = id,
                     gp =
                     gpar(fill = if (SEG) "transparent" else colz, col = opts$bar.col,
                          lwd = opts$bar.lwd))    

    center <- apply(matrix(xx, ncol = 4, byrow = TRUE), 1, function(x) x[2] + (x[3] - x[2]) / 2)
    bounds <- apply(matrix(xx, ncol = 4, byrow = TRUE), 1, function(x) x[2:3])
    
    if (!is.null(inflist)) {
        addBarInference(inflist, center, opts, obj$zoom.index)
        if (!is.null(inflist$comp))
            addBarCompLines(inflist$comp, bounds, p, opts, obj$zoom.index)
    }
}

barinference <- function(obj, tab, phat) {
    ## obj: list of data broken down by subsets
    ## opts: various options (inzpar)

    opts <- obj$opts
    xattr <- obj$xattr
    inf.par <- "proportion"
    inf.type <- opts$inference.type
    bs <- opts$bs.inference
    dat <- obj$df
    

    if (is.null(inf.type)) {
        return(NULL)
    }

    twoway <- length(dim(tab)) == 2  # two way comparison (two factors ...)
    svy <- obj$xattr$class != "inz.simple"

    if (length(dim(tab)) == 1) {
        twoway <- FALSE
        tab <- t(tab)
        phat <- t(phat)

    } else {
        twoway <- TRUE
    }

    lapply(inf.type, function(type) {
        switch(type,
               "conf" = {
                   if (bs) {
                       if (svy) {
                           NULL
                       } else {
                           if (twoway) {
                               ## For now, we will just all over and not return intervals
                               ## IN FUTURE: might want to bootstrap some other way?
                               inf <- try({
                                   n <- rowSums(tab)
                                   b <- boot(dat, function(d, f) {
                                       tt <- t(table(d[f, 1], d[f, 2]))
                                       sweep(tt, 1, n, "/")
                                   }, R = 1000)
                                   cis <- apply(b$t, 2, function(x) c(quantile(x, probs = c(0.025, 0.975)), mean(x)))
                               }, silent = TRUE)
                               if (inherits(inf, "try-error"))
                                   NULL
                               else
                                   list(lower = matrix(cis[1, ], nrow = nrow(tab), byrow = FALSE),
                                        upper = matrix(cis[2, ], nrow = nrow(tab), byrow = FALSE),
                                        estimate = matrix(cis[3, ], nrow = nrow(tab), byrow = FALSE))
                           } else {
                               n <- sum(tab)
                               if (n == 0) return(NULL)
                               b <- boot(dat, function(d, f) table(d[f, 1]) / n, R = opts$n.boot)
                               cis <- apply(b$t, 2, function(x) c(quantile(x, probs = c(0.025, 0.975)), mean(x)))
                               list(lower = cis[1, , drop = FALSE], upper = cis[2, , drop = FALSE], estimate = cis[3, , drop = FALSE])
                           }
                       }
                   } else {
                       if (svy) {
                           NULL
                       } else {
                           ## Standard confidence interval:
                           t(apply(tab, 1, function(x) {
                               n <- sum(x)
                               p <- ifelse(x >= opts$min.count, x / n, NA)
                               se <- sqrt(p * (1 - p) / n)
                               se * 1.96
                           })) -> size
                           if (!twoway) phat <- t(phat)
                           list(lower = phat - size, upper = phat + size, estimate = phat)
                       }
                   }
               },
               "comp" = {
                   if (bs) {
                       if (svy) {
                           NULL
                       } else {
                           NULL
                       }
                   } else {
                       if (svy) {
                           NULL
                       } else {
                           if (twoway) {
                               ## several ways in which this can fall over; rather than testing for
                               ## each, just try and iNZightMR will fail with an error
                               int <- try({
                                   n <- rowSums(tab)
                                   
                                   ## ii - only use rows that have at least 1 count
                                   ## (due to subsetting, can be 0 counts for a row)
                                   ii <- n > 0
                                   
                                   lapply(1:ncol(tab), function(i) {
                                       if(sum(tab[ii, ] > 0) < 2) return(list(compL = NA, compU = NA))
                                       
                                       suppressWarnings(
                                           moecalc(
                                               seBinprops(n[ii], phat[ii, i]), est = phat[ii, i]
                                           )
                                       )
                                   }) -> out
                                   
                                   low <- upp <- phat * 0
                                   low[ii, ] <- sapply(out, function(x) x$compL)
                                   upp[ii, ] <- sapply(out, function(x) x$compU)
                               }, silent = TRUE)
                               if (inherits(int, "try-error"))
                                   NULL
                               else
                                   list(lower = low, upper = upp)
                           } else {
                               if (sum(tab) == 0) return(NULL)
                               phat <- c(phat) # don't want matrix
                               with(suppressWarnings(moecalc(seMNprops(sum(tab), phat), est = phat)),
                                    list(lower = t(compL), upper = t(compU))) -> res
                               lapply(res, function(r) {
                                   colnames(r) <- colnames(tab)
                                   r[tab < opts$min.count] <- NA
                                   r
                               })
                           }
                       }
                   }
               })
    }) -> result
    names(result) <- inf.type

    attr(result, "bootstrap") <- bs
    attr(result, "max") <- max(sapply(result, function(r)
                                      if (is.null(r)) 0 else max(sapply(r, function(x)
                                                                        if(is.null(x)) 0 else max(c(0, x), na.rm = TRUE)),
                                                                 na.rm = TRUE)
                                      ),
                               na.rm = TRUE)

    result    
}
