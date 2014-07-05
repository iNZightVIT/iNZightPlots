addSmoother <-
function(x, y = NULL, f, col, bs, lty = 1) {
    if (is.null(y) & inherits(x, "survey.design")) {
        sm <- svysmooth(y ~ x, design = x, method = "locpoly")[[1]]
    } else {
        sm <- lowess(x, y, f = f)
    }
    grid.lines(sm$x, sm$y,
               default.units = "native",
               gp = gpar(col = col, lwd = 2, lty = lty))

    if (bs) {
        for (i in 1:30) {
          # User wants bootstrap inference for the smoother:
            id <- sample(1:length(x), replace = TRUE)
            x2 <- x[id]
            y2 <- y[id]
            sm <- lowess(x2, y2, f = f)
            grid.lines(sm$x, sm$y,
                       default.units = "native",
                       gp = gpar(col = col, lwd = 1, lty = 3))
        }
    }
}

addQuantileSmoother <-
function(x, y = NULL, quantile, col, lty, lwd) {    
  # Draws quantiles on a plot.
    if (quantile < 0.5)  # symmetry
        quantile <- c(quantile, 1 - quantile)

  # Because we are using the `svysmooth()` function from the `survey` package,
  # we need to supply a design (here, everything is IID)
    if (is.null(y) & inherits(x, "survey.design"))
        des <- x
    else
        des <- suppressWarnings(svydesign(ids = ~1, data = data.frame(x = x, y = y)))
    
    invisible(sapply(quantile,
                     function(a) {
                         s <- svysmooth(y ~ x, design = des,
                                        method = "quantreg", quantile = a)$x
                         grid.lines(s$x, s$y, default.units = "native",
                                    gp = gpar(col = col, lty = lty, lwd = lwd))
                     }))
}


calcQSmooth <- function(xy, q, opts) {
    if (inherits(xy, "survey.design")) {
        x <- xy$variables[, c("x", "y")]
        x <- x[!apply(x, 1, function(y) any(is.na(y))), ]
    } else {
        x <- xy[!apply(xy, 1, function(y) any(is.na(y))), ]
    }
    
  # check quantiles are correct:
    if (q[1] == "default") {
        qp <- 0.5
        if (nrow(x) > opts$quant.cutoff[1]) qp <- c(qp, 0.25)
        if (nrow(x) > opts$quant.cutoff[2]) qp <- c(qp, 0.1)
    } else {
        qp <- q
    }

    if (any(qp < 1 & qp > 0)) {
        qp <- qp[qp > 0 & qp < 1]  # remove invalid quantiles
        
        qp[qp > 0.5] <- qp[qp > 0.5] - 0.5  # symmetry!
      # incase user gives c(0.25, 0.75), remove duplicates
        qp <- sort(unique(qp), decreasing = TRUE)
        
      # Sort out the line type and width:
        nn <- length(qp)
      # bb: the base number of repeats for each unit
        bb <- rep(nn %/% 3, 3)
        be <- nn %% 3  # which units repeated once more
        q.reps <- bb
        if (be != 0) q.reps[1:be] <- q.reps[1:be] + 1
        lty <- rep(1:3, q.reps)

      # Line width (less complicated! ...)
        lwd <- rep(1, length(qp))
        lwd[1] <- 2
        if (length(x) > opts$large.sample.size)
            lwd <- lwd + 1
        
        qs <- list(qp = qp,
                   lty = lty,
                   lwd = lwd)
    } else {
        qs <- NULL
    }

    qs
}
