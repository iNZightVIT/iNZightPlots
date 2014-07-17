create.inz.histplot <- function(obj) {
    create.inz.dotplot(obj, hist = TRUE)
}

plot.inzhist <- function(obj, gen) {
    plot.inzdot(obj, gen, hist = TRUE) # this will set up the plot

    # now draw the bars
}
