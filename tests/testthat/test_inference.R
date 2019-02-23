context("Plot inference information")

test_that("Single-sample dot plots", {
    set.seed(1)
    x <- rnorm(100, sd = 10)
    pl <- iNZightPlot(x, 
        plot = FALSE, 
        inference.par = "mean", 
        inference.type = "conf"
    )    
    xbar <- mean(x)
    wd <- qt(0.975, length(x) - 1) * sd(x) / sqrt(length(x))
    expect_equal(
        as.numeric(pl$all$all$inference.info$mean$conf),
        c(xbar - wd, xbar + wd, xbar)
    )

    pl <- iNZightPlot(x, 
        plot = FALSE, 
        inference.par = "median", 
        inference.type = "conf"
    )
    # 1.5 * IQR / sqrt(N)
    xbar <- median(x)
    wd <- 1.5 * as.numeric(diff(quantile(x, c(0.25, 0.75)))) / sqrt(length(x))
    expect_equal(
        as.numeric(pl$all$all$inference.info$median$conf),
        c(xbar - wd, xbar + wd, xbar)
    )

    pl <- iNZightPlot(x, 
        plot = FALSE, 
        inference.par = "iqr", 
        inference.type = "conf"
    )

})
