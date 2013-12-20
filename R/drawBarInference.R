drawBarInference <- function(hgt, i, n, opts) {
  # A function which adds inference information to bar plots

  # Some error checking ... but only send the error message once.
    if (!is.null(opts$inference.par))
        if (opts$inference.par != "proportion" & i == 1)
            warning('Invalid inference parameter: please remove inference.par, or set it equal to "proportion".')
    
    if (length(dim(hgt)) == 1) {
      # dealing with a single X factor
        phat <- hgt[i]
    } else {
      # dealing with an X and Y factor combination
        phat <- hgt[i, ]  #, ]
    }
    
    for (i in 1:length(phat)) {
        p <- phat[i]
        se <- sqrt(p * (1 - p) / n)
        x <- i / (length(phat) + 1)
        y <- p + c(-1, 1) * se * 1.96

        grid.lines(x, unit(y, "native"),
                   gp =
                   gpar(col = opts$inf.col.comp,
                        lwd = opts$inf.lwd.comp / sqrt(length(phat))))
    }
}
