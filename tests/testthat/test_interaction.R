context("Plot interactivity function")

x <- rnorm(100)
y <- rnorm(100)
a <- factor(sample(LETTERS[1:4], 100, replace = TRUE), levels = LETTERS[1:4])
b <- factor(sample(LETTERS[1:4], 100, replace = TRUE), levels = LETTERS[1:4])

test_that("Supported iNZightPlots return TRUE", {
    expect_true(can.interact(iNZightPlot(x)))
    expect_true(can.interact(iNZightPlot(x, a)))
    expect_true(can.interact(iNZightPlot(a)))
    expect_true(can.interact(iNZightPlot(a, b)))
    expect_true(can.interact(iNZightPlot(x, y)))
})

test_that("Unsupported plots return FALSE", {
    expect_false(can.interact(plot(x, y)))
    expect_false(can.interact(
        ggplot2::ggplot() +
            ggplot2::geom_point(ggplot2::aes(x, y))
    ))
})

df <- data.frame(x=x, y=y, a=a, b=b)
test_that("FT plots return the correct response", {
    # expect_true(can.interact(
    #     iNZightPlot(a, plottype = "gg_column",
    #         varnames = list(x = "a"), data_name = "df")
    # ))
    expect_false(can.interact(
        iNZightPlot(a, plottype = "gg_pie",
            varnames = list(x = "a"), data_name = "df")
    ))
})

try(unlink("Rplot.pdf"), silent = TRUE)
