addTrend <-
function(x, y, order, xlim, col, bs) {
    xx <- seq(xlim[1], xlim[2], length = 1001)
    if (inherits(x, "survey.design")) {
        svy <- x
        expr <- switch(order,
                       formula(y ~ x),
                       formula(y ~ x + I(x^2)),
                       formula(y ~ x + I(x^2) + I(x^3)))
        yy <- try(predict(svyglm(expr, design = svy), data.frame(x = xx)),
                  silent = TRUE)
    } else {
        yy <- try(c(predict(lm(y ~ poly(x, order)), data.frame(x = xx))),
                  silent = TRUE)
    }

  # Sometimes, there might not be enough data points do run poly(),
  # so in this case simply don't draw.
    if (!inherits(yy, "try-error")) {
        grid.lines(xx, yy,
                   default.units = "native",
                   gp = gpar(col = col, lwd = 2, lty = order))

        if (bs) {
            bs.lines <- vector("list", 30)
            
            for (i in 1:30) {
              # User wants bootstrap inference for this line.
                id <- sample(1:length(x), replace = TRUE)
                x2 <- x[id]
                y2 <- y[id]
                
                yy <- try(predict(lm(y2 ~ poly(x2, order)), data.frame(x2 = xx)),
                          silent = TRUE)

              # Some bootstraps can have less than `order` unique points:
                if (inherits(yy, "try-error")) next

                bs.lines[[i]] <- cbind(xx, yy, rep(i, length(yy)))
            }

            all.lines <- do.call(rbind, bs.lines)
            grid.polyline(all.lines[, 1], all.lines[, 2], id = all.lines[, 3],
                          default.units = "native",
                          gp = gpar(col = col, lwd = 1, lty = 3))
        }
    }
}

addParTrend <- function(x, y, by, order, xlim, cols) {
    xx <- rep(seq(xlim[1], xlim[2], length = 1001), length(lby <- levels(by)))
    byy <- rep(lby, each = 1001)
    if (inherits(x, "survey.design")) {
        svy <- x
        expr <- switch(order,
                       formula(y ~ x + by),
                       formula(y ~ x + I(x^2) + by),
                       formula(y ~ x + I(x^2) + I(x^3) + by))
        yy <- try(predict(LM <- svyglm(expr, design = svy), data.frame(x = xx, by = byy)),
                  silent = TRUE)
    } else {
        yy <- try(c(predict(LM <- lm(y ~ poly(x, order) + by), data.frame(x = xx, by = byy))),
                  silent = TRUE)
    }

  # Sometimes, there might not be enough data points do run poly(),
  # so in this case simply don't draw.
    if (!inherits(yy, "try-error")) {
        for (i in 1:length(lby)) {
            grid.lines(xx[byy == lby[i]], yy[byy == lby[i]],
                       default.units = "native",
                       gp = gpar(col = (cols[i]), lwd = 2, lty = order))
        }
    }
}
