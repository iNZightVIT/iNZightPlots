create.inz.barplot <- function(obj) {
    # take the dataframe and settings from the object
    df <- obj$df
    opts <- obj$opts
    xattr <- obj$xattr

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
                      svydesign(ids=~1, weights = rep(1, nrow(df)), data = df)
                  })

    if (ynull) {
        tab <- svytable(~x, design = svy)
        phat <- matrix(tab / sum(tab), nrow = 1)
        widths <- rep(1, length(tab))
        edges <- c(0, 1)
    } else {
        tab <- svytable(~y + x, design = svy)
        phat <- sweep(tab, 1, nn <- rowSums(tab), "/")
        widths <- nn / sum(nn)
        edges <- c(0, cumsum(widths))
    }
    
    inflist <- barinference(obj, tab, phat)
    print(inflist)
    
    out <- list(phat = phat, widths = widths, edges = edges, nx = ncol(phat),
                full.height = opts$full.height,
                xlim = c(0, if (ynull) length(tab) else ncol(tab)),
                ylim = c(0, max(phat)))
    class(out) <- "inzbar"

    out
}

plot.inzbar <- function(obj, gen) {
    opts <- gen$opts
    p <- obj$phat
    nx <- obj$nx
    
    inflist <- obj$inference.info
    
    edges <- rep(obj$edges * 0.9 + 0.05, each = 4)
    edges <- edges[3:(length(edges) - 2)]
    xx <- rep(edges, nx) + rep(1:nx - 1, each = 4 * nrow(p))

    tops <- apply(p, 2, function(x) rbind(0, x, x, 0))
    yy <- c(tops)
    
    id <- rep(1:prod(dim(p)), each = 4)
    colz <- if (is.null(gen$col.args$b.cols)) opts$bar.fill else rep(gen$col.args$b.cols, nx)
    
    grid.polygon(unit(xx, "native"), unit(yy, "native"), id = id,
                 gp =
                 gpar(fill = colz, col = opts$bar.col,
                      lwd = opts$bar.lwd))

    if (!is.null(inflist)) {
        addBarInference(inflist)
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
                           NULL
                       }
                   } else {
                       if (svy) {
                           NULL
                       } else {
                           ## Standard confidence interval:
                           t(apply(tab, 1, function(x) {
                               n <- sum(x)
                               p <- ifelse(x >= 5, x / n, NA)
                               se <- sqrt(p * (1 - p) / n)
                               se * 1.96
                           })) -> size
                           if (!twoway) phat <- t(phat)
                           list(lower = phat - size, upper = phat + size)
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
                           NULL
                       }
                   }
               })
    }) -> result
    names(result) <- inf.type

    attr(result, "bootstrap") <- bs
    result    
}
