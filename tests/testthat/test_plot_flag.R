# Equality checks are temporary: delete them when the `plot` argument is removed.
# Device and visibility checks describe the default print behaviour and stay.

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
        suppressWarnings(expr(plot))
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

test_that("omitting plot returns a visible object and does not draw", {
    f <- tempfile(fileext = ".pdf")
    pdf(f)
    on.exit(
        {
            while (dev.cur() > 1 && names(dev.cur()) == "pdf") {
                dev.off()
            }
            unlink(f)
        },
        add = TRUE
    )
    n <- length(dev.list())
    v <- withVisible(iNZightPlot(Species, data = iris))
    expect_true(v$visible)
    expect_s3_class(v$value, "inzplotoutput")
    expect_equal(length(dev.list()), n)

    set.seed(1)
    p_default <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris)
    set.seed(1)
    p_false <- suppressWarnings(
        iNZightPlot(Sepal.Width, Sepal.Length, data = iris, plot = FALSE)
    )
    expect_equal(p_default, p_false)

    rlang::reset_warning_verbosity("iNZightPlots_plot_arg")
    expect_warning(
        withVisible(iNZightPlot(Species, data = iris, plot = FALSE)),
        "deprecated"
    )
    v_true <- suppressWarnings(withVisible(
        iNZightPlot(Species, data = iris, plot = TRUE)
    ))
    expect_false(v_true$visible)
    v_false <- suppressWarnings(withVisible(
        iNZightPlot(Species, data = iris, plot = FALSE)
    ))
    expect_false(v_false$visible)
})

test_that("assigning a plot opens no device", {
    while (!is.null(dev.list())) {
        dev.off()
    }
    on.exit(
        {
            while (!is.null(dev.list())) {
                dev.off()
            }
            unlink("Rplots.pdf")
        },
        add = TRUE
    )

    expect_devices_unchanged <- function(expr) {
        before <- dev.list()
        force(expr)
        expect_identical(dev.list(), before)
    }

    expect_devices_unchanged(p <- iNZightPlot(Species, data = iris))
    expect_null(dev.list())
    expect_devices_unchanged(p <- inzplot(Sepal.Width ~ Sepal.Length, data = iris))
    expect_null(dev.list())
    expect_devices_unchanged(suppressWarnings(
        p <- iNZightPlot(Species, data = iris, plot = FALSE)
    ))
    expect_null(dev.list())

    ## A bare call auto-prints once and opens one device.
    options(inz_nprint = 0L)
    trace(
        print.inzplotoutput,
        quote(options(inz_nprint = getOption("inz_nprint") + 1L)),
        where = asNamespace("iNZightPlots"),
        print = FALSE
    )
    on.exit(
        {
            untrace(print.inzplotoutput, where = asNamespace("iNZightPlots"))
            options(inz_nprint = NULL)
        },
        add = TRUE
    )
    source(
        textConnection("iNZightPlot(Species, data = iris)"),
        print.eval = TRUE,
        local = TRUE
    )
    expect_equal(getOption("inz_nprint"), 1L)
    expect_length(dev.list(), 1L)
    dev.off()

    options(inz_nprint = 0L)
    source(
        textConnection("p <- iNZightPlot(Species, data = iris)"),
        print.eval = TRUE,
        local = TRUE
    )
    expect_equal(getOption("inz_nprint"), 0L)
    expect_null(dev.list())

    ## print() draws on the current device and does not open another.
    f <- tempfile(fileext = ".pdf")
    pdf(f, width = 7, height = 7)
    cur <- dev.cur()
    n <- length(dev.list())
    p <- iNZightPlot(Species, data = iris)
    expect_equal(dev.cur(), cur)
    expect_length(dev.list(), n)
    expect_false("inz-ylab" %in% grid.ls(print = FALSE)$name)

    print(p)
    expect_equal(dev.cur(), cur)
    expect_length(dev.list(), n)
    expect_equal(grid.get("inz-ylab")$label, "Percentage (%)")

    ## Explicit plot=TRUE still draws immediately, on the device already open.
    suppressWarnings(p <- iNZightPlot(Species, data = iris, plot = TRUE))
    expect_equal(dev.cur(), cur)
    expect_length(dev.list(), n)
    expect_equal(grid.get("inz-ylab")$label, "Percentage (%)")
})
