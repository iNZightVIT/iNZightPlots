context("Bar plots")

set.seed(1)
df <- data.frame(
    x = sample(LETTERS[1:3], 100, replace = TRUE, prob = c(0.5, 0.3, 0.2))
)
tab <- table(df$x)
pr <- tab / nrow(df)
# CI width
wd <- as.numeric(1.96 * sqrt(pr * (1 - pr) / nrow(df)))

bar1 <- iNZightPlot(x, data = df,
    plot = FALSE,
    inference.type = "conf",
    inference.par = "proportion"
)

bar1_counts <- iNZightPlot(x, data = df,
    plot = FALSE,
    inference.type = "conf",
    inference.par = "proportion",
    bar.counts = TRUE
)

test_that("Inference information is correct", {
    inf <- list(
        lower = unclass(t(pr - wd)),
        upper = unclass(t(pr + wd)),
        estimate = t(as.numeric(pr))
    )
    expect_equal(bar1$all$all$inference.info$conf, inf)
    expect_equal(bar1_counts$all$all$inference.info$conf, inf)
})

test_that("Y axis limits computed correctly", {
    expect_equal(bar1$all$all$ylim, c(0, max(pr + wd)))
    expect_equal(bar1_counts$all$all$ylim, c(0, max(tab)))
})
