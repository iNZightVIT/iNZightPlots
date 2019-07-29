context("Get plot inference")

set.seed(100)
d <- data.frame(
    x = rnorm(100, c(150, 155), c(10, 20)),
    y = factor(c("A", "B"))
)
# iNZightPlot(x, y, data = d)

test_that("Two-sample tests use appropriate CI", {
    pTRUE <- getPlotSummary(x, y, data = d,
        summary.type = "inference", 
        inference.type = "conf",
        hypothesis.var.equal = TRUE
    )
    pFALSE <- getPlotSummary(x, y, data = d,
        summary.type = "inference", 
        inference.type = "conf",
        hypothesis.var.equal = FALSE
    )

    pvals <- sapply(list(pTRUE, pFALSE), function(p) {
        as.numeric(
            gsub(".+=", "", 
                strsplit(p[grep("p-value = ", p)[1]], ",")[[1]][3]
            )
        )
    })
    expect_equal(pvals, 
        c(
            t.test(x ~ y, data = d, var.equal = TRUE)$p.value,
            t.test(x ~ y, data = d)$p.value
        )
    )

    cis <- lapply(list(pTRUE, pFALSE), function(p) {
        scan(text = gsub("A - B", "", p[grep("A - B", p)]), what = double(),
            quiet = TRUE)[-2]
    })
    expect_equal(cis,
        list(
            as.numeric(round(t.test(x ~ y, data = d, var.equal = TRUE)$conf.int, 3)),
            as.numeric(round(t.test(x ~ y, data = d, var.equal = FALSE)$conf.int, 3))
        )
    )
})
