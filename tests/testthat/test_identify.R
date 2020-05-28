context("Identify points")

test_that("Points can be labelled by another variable", {
    p <- iNZPlot(Sepal.Length ~ Sepal.Width, data = iris, locate = Species,
        locate.id = 1:5, plot.features = list(order.first = -1), plot = FALSE)
    expect_equal(
        p$all$all$text.labels,
        ifelse(seq_len(nrow(iris)) > 5, "", "setosa")
    )
})

test_that("Points can be labelled by their row id", {
    p <- iNZPlot(Sepal.Length ~ Sepal.Width, data = iris, locate = "id",
        locate.id = 1:5, plot.features = list(order.first = -1), plot = FALSE)
    expect_equal(
        p$all$all$text.labels,
        ifelse(seq_len(nrow(iris)) > 5, "", seq_len(nrow(iris)))
    )
})

test_that("Points with same level of X are identified", {
    p <- iNZPlot(Sepal.Length ~ Sepal.Width, data = iris, locate = NULL,
        locate.id = c(1), locate.same.level = Species,
        locate.col = "red", highlight = 1,
        plot.features = list(order.first = -1), plot = FALSE)
    expect_equal(
        p$all$all$text.labels,
        ifelse(iris$Species == "setosa", " ", "")
    )
})

test_that("Locating extreme points", {
    p <- iNZPlot(Sepal.Width, data = iris, locate.extreme = c(1, 0),
        locate = Species, locate.same.level = Species, plot = FALSE)
})
