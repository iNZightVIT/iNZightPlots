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

        grid.rect(x = unit(inf.x[1], "native"),
                  y = unit(0.5, "npc"),
                  width = unit(inf.x[2] - inf.x[1], "native"),
                  height = unit.pmax(unit(0.1, "npc"), unit(0.3, "lines")),
                  just = "left",
                  gp =
                  gpar(fill = comp.col, lwd = 0))
        
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
        if (!opts$bs.inference) {
            inf.wd <- qt(0.975, df = length(x) - 1,) * sd(x) / sqrt(length(x))
            inf.mean <- mean(x)
            inf.x <- inf.mean + c(-1, 1) * inf.wd
          # Draw the inference "bar"
            grid.rect(x = unit(inf.x[1], "native"),
                      y = unit(0.5, "npc"),
                      width = unit(inf.x[2] - inf.x[1], "native"),
                      height = unit.pmax(unit(0.03, "npc"), unit(0.1, "lines")),
                      just = "left",
                      gp =
                      gpar(fill = "blue", lwd = 0))

           # if (!is.null(guides))
           #     guides <- NULL  #c(guides, inf.x)
        }

        opts$inference.type <- c("comp", "conf")
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
            comp.col <- "red"
            mean.col <- "black"
        }

      # Draw the inference "bar"
        grid.rect(x = unit(inf.x[1], "native"),
                  y = unit(0.5, "npc"),
                  width = unit(inf.x[2] - inf.x[1], "native"),
                  height = unit.pmax(unit(0.1, "npc"), unit(0.3, "lines")),
                  just = "left",
                  gp =
                  gpar(fill = comp.col, lwd = 0))
        
        if (!is.null(guides))# & (!"conf" %in% opts$inference.type | opts$bs.inference == TRUE))
            guides <- c(guides, inf.x)
    }
  # add the mean line
    grid.rect(x = unit(inf.mean, "native"),
              y = unit(0.5, "npc"),
              width = unit(1, "points"),
              height = unit.pmax(unit(0.1, "npc"), unit(0.3, "lines")) + unit(2, "points"),
              gp =
              gpar(lwd = 0, fill = mean.col))

    guides
}

