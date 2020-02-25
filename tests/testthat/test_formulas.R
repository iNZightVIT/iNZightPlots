context("Formula interface")

test_that("Formula interface works", {
    expect_is(
        iNZPlot(Sepal.Width, data = iris),
        "inzplotoutput"
    )
    expect_is(
        iNZPlot(Sepal.Width~Sepal.Length, data = iris),
        "inzplotoutput"
    )

    expect_is(
        iNZPlot(Sepal.Width~Sepal.Length|Species, data = iris),
        "inzplotoutput"
    )
})

test_that("Formula yields same results", {
    expect_equal(
        iNZPlot(Species, data = iris),
        iNZightPlot(Species, data = iris)
    )
    expect_equal(
        iNZPlot(Sepal.Width ~ Species, data = iris),
        iNZightPlot(Species, Sepal.Width, data = iris)
    )
    expect_equal(
        iNZPlot(Sepal.Width ~ Sepal.Length | Species, data = iris,
            plot.features = list(order.first = -1)),
        iNZightPlot(Sepal.Length, Sepal.Width, g1 = Species, data = iris,
            plot.features = list(order.first = -1))
    )
})
