context("Financial Times plots")

test_that("Inversing x/y doesn't affect graph", {
    p1 <- iNZightPlot(Species, Sepal.Length, data = iris, plottype = "gg_violin")
    p2 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_violin")
    expect_equal(attr(p1, "code"), attr(p2, "code"))
})

test_that("Y-axis label for one-way numeric is blank", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode")
    expect_equal(p1$labels$y, "")
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode2")
    expect_equal(p1$labels$y, "")
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode3")
    expect_equal(p1$labels$y, "")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_boxplot")
    expect_equal(p1$labels$x, "")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin")
    expect_equal(p1$labels$x, "")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_dotstrip")
    expect_equal(p1$labels$y, "")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_beeswarm")
    expect_equal(p1$labels$x, "")
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_quasirandom")
    expect_equal(p1$labels$x, "")
})
