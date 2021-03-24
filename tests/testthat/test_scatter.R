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

test_that("Adding inference information", {
    p <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris,
        trend = c("linear", "quadratic", "cubic"),
        inference.type = "conf"
    )
    expect_is(p, "inzplotoutput")

    p <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris,
        colby = Species,
        trend = "linear",
        trend.by = TRUE, trend.parallel = FALSE,
        inference.type = "conf"
    )
    expect_is(p, "inzplotoutput")

    p <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris,
        colby = Species,
        trend = "linear",
        trend.by = TRUE, trend.parallel = TRUE,
        inference.type = "conf"
    )
    expect_is(p, "inzplotoutput")


    p <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris,
        trend = "linear",
        inference.type = "conf",
        bs.inference = TRUE
    )
    expect_is(p, "inzplotoutput")

    p <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris,
        colby = Species,
        trend = "linear",
        trend.by = TRUE, trend.parallel = FALSE,
        inference.type = "conf",
        bs.inference = TRUE
    )
    expect_is(p, "inzplotoutput")

    p <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris,
        colby = Species,
        trend = "linear",
        trend.by = TRUE, trend.parallel = TRUE,
        inference.type = "conf",
        bs.inference = TRUE
    )
    expect_is(p, "inzplotoutput")
})

test_that("Scatter plot with single unique x/y value", {
    d <- data.frame(x = rep(10, 10), y = rnorm(10))
    expect_silent(inzplot(y ~ x, data = d))
    expect_silent(inzplot(x ~ y, data = d))
})
