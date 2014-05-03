createPlot <- function(df, opts) {
  # This function takes a data.frame object and creates the necessary object which will have a
  # `plot` method.
    
    v <- colnames(df)
    if (!"y" %in% v) {
        type <- ifelse(is.factor(df$x), "barplot", "dotplot")
    } else {
        type <- ifelse(is.factor(df$x),
                       ifelse(is.factor(df$y), "barplot", "dotplot"),
                       ifelse(is.factor(df$y), "dotplot", "scatterplot"))
    }

    # Here, we create a class for the object to be plotted, then we use a generic function `create`
    # which will use the correct method, and create the required plot.
    
    pclass <- paste("inz", type, sep = ".")
    obj <- structure(.Data = list(df = df, opts = opts))
    class(obj) <- pclass

    create(obj)
}

create <- function(obj)
    UseMethod("create")
