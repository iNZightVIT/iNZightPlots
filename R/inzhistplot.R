create.inz.histplot <- function(obj) {
    df <- obj$df
    opts <- obj$opts
    xattr <- obj$xattr
    
    v <- colnames(df)
    vn <- xattr$varnames

    if (xattr$class == "inz.survey")
        df <- df$variables
    
    # first need to remove missing values
    missing <- is.na(df$x)
    n.missing <- sum(missing)
    df <- df[!missing, ]

    if (xattr$class == "inz.freq")
        svy <- svydesign(ids=~1, weights = df$freq, data = df)
    else {        
        svy <- eval(parse(text = modifyData(obj$df$call, "df")))
    }

    ## To do this, we will pretty much grab stuff from the `survey` package, however it
    ## cannot be used separately to produce the bins etc without plotting it;
    ## So copyright for most of this goes to Thomas Lumley.

    h <- hist(svy$variables$x, plot = FALSE)
    props <- coef(svymean(~cut(svy$variables$x, h$breaks, include.lowest = TRUE),
                          svy, na.rm = TRUE))
    h$density <- props / diff(h$breaks)
    h$counts <- props * sum(weights(svy))

    out <- list(hist = h, n.missing = n.missing,
                xlim = if (nrow(df) > 0) range(df$x, na.rm = TRUE) else c(-Inf, Inf),
                ylim = c(0, max(h$counts)))
}

plot.inzhist <- function(obj, gen) {

}
