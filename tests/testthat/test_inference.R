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

d <- data.frame(x = sample(c("A", "B"), 100, replace = TRUE, c(0.3, 0.8)))
ptest <- prop.test(table(d$x), )
btest <- binom.test(table(d$x), p = 0.4, alternative = "less")
ctest <- chisq.test(table(d$x))
s1 <- getPlotSummary(x, data = d, summary.type = "inference",
    inference.type = "conf",
    hypothesis = list(
        test = "proportion",
        use.exact = FALSE,
        value = 0.5,
        alternative = "two.sided"
    ))
s2 <- getPlotSummary(x, data = d, summary.type = "inference",
    inference.type = "conf",
    hypothesis = list(
        test = "proportion",
        use.exact = TRUE,
        value = 0.4,
        alternative = "less"
    ))
s3 <- getPlotSummary(x, data = d, summary.type = "inference",
    inference.type = "conf",
    hypothesis = list(
        test = "chi2"
    ))

test_that("One-sample tests give correct p-value", {
    expect_match(
        paste(s1, collapse = "\n"),
        sprintf("p-value = %s", format.pval(ptest$p.value, digits = 5))
    )
    expect_match(
        paste(s2, collapse = "\n"),
        sprintf("p-value = %s", format.pval(btest$p.value, digits = 5))
    )
    expect_match(
        paste(s3, collapse = "\n"),
        sprintf("p-value = %s", format.pval(ctest$p.value, digits = 5))
    )
})

test_that("One-sample tests display correct hypotheses", {
    expect_match(
        paste(s1, collapse = "\n"),
        "Null Hypothesis: true proportion of x = A is 0.5"
    )
    expect_match(
        paste(s1, collapse = "\n"),
        "Alternative Hypothesis: true proportion of x = A is not equal to 0.5"
    )

    expect_match(
        paste(s2, collapse = "\n"),
        "Null Hypothesis: true proportion of x = A is 0.4"
    )
    expect_match(
        paste(s2, collapse = "\n"),
        "Alternative Hypothesis: true proportion of x = A is less than 0.4"
    )

    expect_match(
        paste(s3, collapse = "\n"),
        "Null Hypothesis: true proportions in each category are equal"
    )
    expect_match(
        paste(s3, collapse = "\n"),
        "Alternative Hypothesis: true proportions in each category are not equal"
    )  
})

