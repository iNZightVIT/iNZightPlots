# Temporary. Delete this file when the `plot` argument is removed.
# Until then, `plot = TRUE` and `plot = FALSE` must return the same object.

context("plot = TRUE matches plot = FALSE")

expect_same_plot <- function(expr) {
    seed <- if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
        get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    } else {
        NULL
    }
    on.exit(
        {
            if (!is.null(seed)) {
                assign(".Random.seed", seed, envir = .GlobalEnv)
            }
        },
        add = TRUE
    )

    draw <- function(plot) {
        f <- tempfile(fileext = ".pdf")
        pdf(f, width = 7, height = 7)
        on.exit(
            {
                while (dev.cur() > 1 && names(dev.cur()) == "pdf") {
                    dev.off()
                }
                unlink(f)
            },
            add = TRUE
        )
        set.seed(1)
        expr(plot)
    }

    p1 <- draw(TRUE)
    p2 <- draw(FALSE)
    expect_null(attr(p2, "plotargs"))
    expect_named(p2$gen, c("opts", "mcex", "col.args", "maxcount"))
    expect_equal(p1, p2)
}

test_that("each plot type matches", {
    iris2 <- iris
    iris2$wide <- factor(ifelse(iris2$Petal.Width > 1, "wide", "narrow"))

    expect_same_plot(function(plot) {
        iNZightPlot(Species, data = iris, plot = plot)
    })
    expect_same_plot(function(plot) {
        iNZightPlot(Species, wide, data = iris2, plot = plot)
    })
    expect_same_plot(function(plot) {
        iNZightPlot(Sepal.Width, data = iris, plot = plot)
    })
    expect_same_plot(function(plot) {
        iNZightPlot(Sepal.Width, Species, data = iris, plot = plot)
    })
    expect_same_plot(function(plot) {
        iNZightPlot(Sepal.Width, data = iris, plottype = "hist", plot = plot)
    })
    expect_same_plot(function(plot) {
        inzplot(Sepal.Width ~ Sepal.Length, data = iris, plot = plot)
    })
    expect_same_plot(function(plot) {
        inzplot(Sepal.Width ~ Sepal.Length, data = iris, plottype = "hex", plot = plot)
    })
    expect_same_plot(function(plot) {
        suppressWarnings(
            inzplot(Sepal.Width ~ Sepal.Length, data = iris, plottype = "grid", plot = plot)
        )
    })
})
