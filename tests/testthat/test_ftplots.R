context("Financial Times plots")

gg_pkgs <- c(
    "ggplot2",
    "dplyr",
    "tidyr",
    "forcats",
    "ggmosaic",
    "waffle",
    "ggthemes",
    "ggbeeswarm",
    "ggridges"
)
gg_pkgs_check <- sapply(gg_pkgs, requireNamespace, quietly = TRUE)
skip_if(any(gg_pkgs_check), "Unable to check FT plots as some packages are missing.")

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

test_that("Plots can be rotated", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin",
        rotation = FALSE)
    expect_match(attr(p1, "code"), "coord_flip")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin",
        rotation = TRUE)
    expect_false(grepl("coord_flip", attr(p1, "code")))

    # p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin",
    #     rotation = TRUE, rotate_labels = list(x = TRUE))
    # expect_match(attr(p1, "code"), "coord_flip")
})
