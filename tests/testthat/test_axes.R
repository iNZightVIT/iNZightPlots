context("Plot axes")

test_that("Axis label formatting is consistent for large values", {
    d <- data.frame(
        x = runif(100, 0, 150000),
        y = runif(100, -1e5, 1e5)
    )

    expect_silent(iNZightPlot(x, y, data = d))
    labs <- grid.get("inz-xaxis-bottom.1.1")$label
    expect_equal(
        labs,
        format(seq(0, 150000, length = 4), scientific = FALSE)
    )
    expect_false(any(grepl("1e+", labs, fixed = TRUE)))
})