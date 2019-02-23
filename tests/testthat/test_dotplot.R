context("Dot plots")

test_that("Inference is valid", {
    x <- 1:10
    pl <- iNZightPlot(x, 
        plot = FALSE, 
        inference.par = "mean", 
        inference.type = c("conf")
    )
    expect_equal(
        round(as.numeric(pl$all$all$inference.info$mean$conf), 2),
        c(3.33, 7.67, 5.5)
    )
})