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
    p <- iNZPlot(Sepal.Width, data = iris, colby = Species, locate.extreme = c(1, 4),
        locate = Species, plot = FALSE)
    expect_equal(p$all$all$toplot$all$extreme.ids, c(61, 15, 33, 34, 16))
    expect_equal(
        p$all$all$toplot$all$text.labels,
        c("versicolor", rep("", 145), rep("setosa", 4))
    )

    p <- iNZPlot(Sepal.Width, data = iris, colby = Species,
        locate.extreme = c(1, 0), locate = Species, locate.same.level = Species,
        plot = FALSE, locate.col = "red")
    expect_equal(
        sort(p$all$all$toplot$all$extreme.ids),
        which(iris$Species == "versicolor")
    )
    expect_equal(
        p$all$all$toplot$all$text.labels,
        ifelse(iris$Species == "versicolor", "versicolor", "")[order(iris$Sepal.Width)]
    )
})
