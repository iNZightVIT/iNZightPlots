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
