create.inz.barplot <- function(obj) {
    # take the dataframe and settings from the object
    df <- obj$df
    opts <- obj$opts
    xattr <- obj$xattr

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
        phat <- tab / sum(tab)
        widths <- rep(1, length(tab))
        centers <- rep(0.5, length(tab))
    } else {
        tab <- svytable(~y + x, design = svy)
        phat <- sweep(tab, 1, nn <- rowSums(tab), "/")
        widths <- nn / sum(nn)
        centers <- cumsum(widths) - widths[1] / 2
    }

    out <- list(phat = phat, widths = widths, centers = centers,
                xlim = c(0, if (ynull) length(tab) else ncol(tab)),
                ylim = c(0, max(phat)))
    class(out) <- "inzbar"

    out
}

plot.inzbar <- function(obj, gen) {
    
}
