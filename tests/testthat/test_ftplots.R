context("Financial Times plots")

test_that("Inversing x/y doesn't affect graph", {
    p1 <- iNZightPlot(Species, Sepal.Length, data = iris, plottype = "gg_violin")
    p2 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_violin")
    expect_equal(attr(p1, "code"), attr(p2, "code"))
})
