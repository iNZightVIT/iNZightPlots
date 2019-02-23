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

    ### Bootstraps
    # pl <- iNZightPlot(x, 
    #     plot = FALSE, 
    #     inference.par = "iqr", 
    #     inference.type = "conf"
    # )

})

test_that("One-way table barplots", {
    set.seed(1)
    df <- data.frame(
        x = sample(LETTERS[1:3], 100, replace = TRUE, prob = c(0.5, 0.3, 0.2))
    )
    pl <- iNZightPlot(x, data = df,
        plot = FALSE,
        inference.type = "conf",
        inference.par = "proportion"
    )
    tab <- table(df$x)
    pr <- tab / nrow(df)
    wd <- as.numeric(1.96 * sqrt(pr * (1 - pr) / nrow(df)))
    expect_equal(
        pl$all$all$inference.info$conf,
        list(
            lower = unclass(t(pr - wd)),
            upper = unclass(t(pr + wd)),
            estimate = t(as.numeric(pr))
        )
    )
})
