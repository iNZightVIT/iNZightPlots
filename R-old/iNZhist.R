iNZhist <-
function(x, nbins = 20, xlim = range(x, na.rm = TRUE), scale.width = TRUE) {

  # ----------------------------------------------------------------------- #
  # Rather than using the hist() function in the base R installation,
  # we are defining out own so we have more precise control over the
  # number of bins, and therefore make it look as close as possible to the
  # alternative dot plot.
  # ----------------------------------------------------------------------- #
  # Return a list containing the necessary information for plotting (similar
  # to that of the hist() function).
  # ----------------------------------------------------------------------- #

  # Create even cut points in the given data range
    range <- xlim
    range <- range + c(-1, 1) * 0.01 * diff(range)  # include boundary values!
    cuts <- seq(range[1] - 0.1, range[2] + 0.1,
                length = nbins + 1)
    bin.min <- cuts[-(nbins + 1)]
    bin.max <- cuts[-1]

  # Cut the data and calculate values
    x.bin <- cut(x, cuts)
    widths <- bin.max - bin.min
    mids <- bin.min + widths / 2

    tab <- table(x.bin)
    den <- tab / sum(tab)

    list(breaks = cuts,
         counts = as.numeric(tab),  
         density = as.numeric(den),
         mids = mids,
         x = sort(x))
}
