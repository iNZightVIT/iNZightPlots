context("Scatter plots")

test_that("Colour by works", {
    expect_is(
        iNZightPlot(Sepal.Width, Sepal.Length,
            colby = Species, data = iris),
        "inzplotoutput"
    )
    expect_is(
        iNZightPlot(Sepal.Width, Sepal.Length,
            colby = Petal.Length, data = iris),
        "inzplotoutput"
    )
    expect_is(
        iNZightPlot(Sepal.Width, Sepal.Length,
            colby = Petal.Length, col.method = "rank", data = iris),
        "inzplotoutput"
    )
})
