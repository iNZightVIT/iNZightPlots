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
