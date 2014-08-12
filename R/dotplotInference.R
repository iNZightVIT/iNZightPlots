drawMedianInference <- function(x, opts, guides = NULL) {
  # Adds median inference to the plot

    if ("conf" %in% opts$inference.type) {
      # Not yet implemented ...
    }

    if ("comp" %in% opts$inference.type) {
        if (opts$bs.inference) {
            b <- boot(x, function(x, d) median(x[d]), R = opts$n.boot)
            inf.x <- boot.ci(b, type = "perc")$percent[1, c(4, 5)]
            comp.col <- "darkgreen"
        } else {
            q <- quantile(x, c(0.25, 0.5, 0.75))
            inf.wd <- 1.5 * (abs(q[3] - q[1]) / sqrt(length(x)))
            inf.x <- q[2] + c(-1, 1) * inf.wd
            comp.col <- "blue"
        }

        grid.lines(x = unit(inf.x, "native"), y = 0.5,
                   gp =
                   gpar(col = comp.col,
                        lwd = opts$inf.lwd.comp,
                        lineend = "butt"))

        if (!is.null(guides))
            guides <- c(guides, inf.x)
    }

    guides
}

drawMeanInference <- function(x, opts, guides = NULL) {
  # Adds mean inference to the plot
    mean.col <- "black"

    if ("conf" %in% opts$inference.type) {
      # Quick fix because currently if bootstrap, only show ci
        if (opts$bs.inference) {
            b <- boot(x, function(x, d) mean(x[d]), R = opts$n.boot)
            inf.x <- boot.ci(b, type = "norm")$norm[1, 2:3]
            inf.mean <- mean(b$t)
        } else {
            inf.wd <- qt(0.975, df = length(x) - 1,) * sd(x) / sqrt(length(x))
            inf.mean <- mean(x)
            inf.x <- inf.mean + c(-1, 1) * inf.wd
        }

        grid.lines(x = unit(inf.x, "native"),
                   y = 0.5,
                   gp =
                   gpar(col = "red",
                        lwd = opts$inf.lwd.conf,
                        lineend = "butt"))
    }

    if ("comp" %in% opts$inference.type) {
        if (opts$bs.inference) {
            b <- boot(x, function(x, d) mean(x[d]), R = opts$n.boot)
            inf.x <- boot.ci(b, type = "perc")$percent[1, c(4, 5)]
            inf.mean <- mean(b$t)
            comp.col <- "darkgreen"
            mean.col <- "white"
        } else {
            inf.wd <- (1 / sqrt(2)) * (sd(x) / sqrt(length(x)))
            inf.x <- mean(x) + c(-1, 1) * inf.wd
            inf.mean <- mean(x)
            comp.col <- "blue"
            mean.col <- "black"
        }

        grid.lines(x = unit(inf.x, "native"),
                   y = 0.5,
                   gp =
                   gpar(col = comp.col,
                        lwd = opts$inf.lwd.comp,
                        lineend = "butt"))
        
        if (!is.null(guides))
            guides <- c(guides, inf.x)
    }
    
    guides
}

