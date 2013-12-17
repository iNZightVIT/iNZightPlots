## A suite of generic functions used by the plotting functions.

stacking <- function(x, bins) {
  # Takes a univariate, continuous variable and a binning
  # factor, and produces a list.
    
    oo <- order(x)
    x <- x[oo]
    bins <- bins[oo]
    bins <- factor(bins)
    xg <- split(x, bins)
    xo <- lapply(xg, seq_along)
    x <- unlist(xg, use.names = FALSE)
    du <- unlist(xo, use.names = FALSE)
    
    list(x = x, du = du)
}


drawLegend <- function(legend) {
  # USES GRID
  # Draws a legend in the center of the current viewport.

    lab       <- legend$lab
    n         <- length(lab)
    col       <- legend$col
    pch       <- rep(legend$pch, length = n)
    cex       <- legend$cex
    cex.title <- ifelse(is.null(legend$cex.title), 1.2 * cex,
                        legend$cex.title)
    cex.pt    <- legend$cex.pt
    title     <- legend$title

    

  # Set up a viewport in the middle of the plotting area

  # Line position for each label
  # *** This is modified from the grid.legend() function in the Grid package ***
    gap <- unit(0.5, "lines")
    
    leg.width <-
        max(sapply(lab, function(x)
                   convertWidth(grobWidth(textGrob(x, gp =
                                                   gpar(cex = cex))), "mm")))
    legend.layout <-
        grid.layout(n, 4,
                    widths = unit.c(unit(0, "mm"), unit(2, "lines"),
                        unit(leg.width, "mm"), unit(0.5, "lines")), 
                    heights =
                    unit.pmax(unit(2, "lines"),
                              gap + unit(rep(1, n), "strheight", as.list(lab))))

    fg <- frameGrob(layout = legend.layout)#, gp = gpar(cex = cex))
    
    for (i in 1L:n) {
        fg <- placeGrob(fg, pointsGrob(0.5, 0.5, pch = pch[i],
                                       gp = gpar(col = col[i], cex = cex.pt, lwd = 2)),
                        col = 2, row = i)
       
        fg <- placeGrob(fg, textGrob(lab[i], x = 0 , y = 0.5,
                                     just = c("left", "center"),
                                     gp = gpar(cex = cex)),
                        col = 3, row = i)
    }

    fg <- placeGrob(fg, textGrob(title, x = 0.5, y = 1,
                                 just = c("center", "bottom"),
                                 gp = gpar(cex = cex.title)))

    fg
}


drawBoxPlot <- function(x, opts) {
  # Draws a box plot in a prepared grid layout.
  # Assumes it can fill the entire region.

  # Calculate quantiles for box
    q <- quantile(x, c(0, 0.25, 0.5, 0.75, 1))

  # Draw the box
    lwd <- rep(opts$box.lwd, length = 2)
    grid.rect(x = q[2], y = unit(0.8, "npc"),
              width = q[4] - q[2],
              height = unit(0.6, "npc"),
              default.units = "native",
              just = c("left", "top"),
              gp =
              gpar(lwd  = lwd[1],
                   col  = opts$box.col,
                   fill = opts$box.fill))

  # Draw the median
    grid.lines(x = unit(q[3], "native"),
               y = unit(c(0.2, 0.8), "npc"),
               gp = gpar(lwd = lwd[1], col = opts$box.col))

  # Draw the whiskers
    grid.lines(x = unit(q[1:2], "native"), y = 0.5,
               gp = gpar(lwd = lwd[2], col = opts$box.col))
    grid.lines(x = unit(q[4:5], "native"), y = 0.5,
               gp = gpar(lwd = lwd[2], col = opts$box.col))
}




convert.to.factor <- function(x) {
  # converts a 
    if (length(unique(x)) < 5)
        x.fact <- factor(x)
    else {  
        x.quantiles <- round((quantile(x, na.rm = TRUE)), 0)  
        x.fact <- try(cut(x, c(-Inf, ifelse(unique(x.quantiles[2:4]) == 3,
                                            x.quantiles[2:4],
                                            unique(x.quantiles[2:4])),
                               Inf)))
        
        if (inherits(x.fact, "try-error")) {
            eps <- .Machine$double.eps
            x.quantiles <- round((quantile(x, na.rm = TRUE)), 2) + eps * (0:10)
            x.fact <- cut(x, c(-Inf, ifelse(unique(x.quantiles[2:4]) == 3,
                                            x.quantiles[2:4],
                                            unique(x.quantiles[2:4])),
                               Inf))
        }
        
        if ((x.quantiles[2] == x.quantiles[3]) && (x.quantiles[3] == x.quantiles[4]))
            levels(x.fact) <-
                c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
                  paste(c("(", x.quantiles[2], " - ", x.quantiles[5], "]"), collapse = ""))
        else if (x.quantiles[2] == x.quantiles[3])
            levels(x.fact) <-
                c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
                  paste(c("(", x.quantiles[2], " - ", x.quantiles[4], "]"), collapse = ""),
                  paste(c("(", x.quantiles[4], " - ", x.quantiles[5], "]"), collapse = ""))
        else if (x.quantiles[3] == x.quantiles[4])
            levels(x.fact) <-
                c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
                  paste(c("(", x.quantiles[2], " - ", x.quantiles[3], "]"), collapse = ""),
                  paste(c("(", x.quantiles[3], " - ", x.quantiles[5], "]"), collapse = ""))
        else
            levels(x.fact) <-
                c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
                  paste(c("(", x.quantiles[2], " - ", x.quantiles[3], "]"), collapse = ""),
                  paste(c("(", x.quantiles[3], " - ", x.quantiles[4], "]"), collapse = ""),
                  paste(c("(", x.quantiles[4], " - ", x.quantiles[5], "]"), collapse = ""))
    }
    x.fact
}


addTrend <- function(x, y, order, xlim, col, bs) {
    xx <- seq(xlim[1], xlim[2], length = 1001)
    yy <- predict(lm(y ~ poly(x, order)), data.frame(x = xx))
    grid.lines(xx, yy,
               default.units = "native",
               gp = gpar(col = col, lwd = 2))

    if (bs) {
        for (i in 1:30) {
          # User wants bootstrap inference for this line.
            id <- sample(1:length(x), replace = TRUE)
            x2 <- x[id]
            y2 <- y[id]
            yy <- predict(lm(y2 ~ poly(x2, order)), data.frame(x2 = xx))
            grid.lines(xx, yy,
                       default.units = "native",
                       gp = gpar(col = col, lwd = 1, lty = 3))
        }
    }
}

addSmoother <- function(x, y, f, col, bs) {
    sm <- lowess(x, y, f = f)
    grid.lines(sm$x, sm$y,
               default.units = "native",
               gp = gpar(col = col, lwd = 2))

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

addLOE <- function(x, y, col, lty, xlim, ylim) {
    xx <- c(min(xlim, ylim), max(xlim, ylim))
    grid.lines(xx, xx, default.units = "native",
               gp = gpar(col = col, lty = lty))
}

inzDefault <- function(par)
    inzplotDefaults[[par]]

inzPlotDefaults <- function() {
  # Houses all of the default settings for plotting. Alternatively,
  # could edit them (i.e., points to a list in global environment)
    list(pch            = 1,
         col            = "red",
         col.pt         = "grey50",
         cex            = 1,
         cex.pt         = 0.6,
         cex.lab        = 1,
         cex.axis       = 0.8,
         cex.main       = 1.2,
         cex.text       = 1,
         bg             = "white",
         fill.pt        = "transparent",
         lwd            = 1,
         lty            = 1,
         lwd.pt         = 2,
         col.line       = "blue",
         col.sub        = "wheat",
         jitter         = "",
         trend          = NULL,
         smooth         = NULL,
         LOE            = NULL,
         join           = FALSE,
         col.trend      = list(linear = "blue",
                               quadratic = "red",
                               cubic ="green4"),
         col.smooth     = c("magenta"),
         col.LOE        = "black",
         lty.LOE        = 2,
         bs.inference   = FALSE,
         box            = TRUE,
         box.lwd        = c(2, 0.7),
         box.col        = "black",
         box.fill       = "grey90")
}
