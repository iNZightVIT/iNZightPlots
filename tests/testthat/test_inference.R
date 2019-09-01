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

set.seed(400)
d <- data.frame(x = sample(c("A", "B"), 100, replace = TRUE, c(0.3, 0.8)))
ptest <- list(
    p.value = pnorm(
        abs((table(d$x)[[1]] / 100 - 0.5) / 0.05),
        lower.tail = FALSE
    )
)
btest <- binom.test(table(d$x), p = 0.4, alternative = "less")
ctest <- chisq.test(table(d$x))
s1 <- getPlotSummary(x, data = d, summary.type = "inference",
    inference.type = "conf",
    hypothesis.test = "proportion",
    hypothesis.use.exact = FALSE,
    hypothesis.value = 0.5,
    hypothesis.alt = "two.sided"
)
s2 <- getPlotSummary(x, data = d, summary.type = "inference",
    inference.type = "conf",
    hypothesis.test = "proportion",
    hypothesis.use.exact = TRUE,
    hypothesis.value = 0.4,
    hypothesis.alt = "less"
)
s3 <- getPlotSummary(x, data = d, summary.type = "inference",
    inference.type = "conf",
    hypothesis.test = "chi2"
)
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


# small counts
d <- expand.grid(
    Machine = c("Desktop of tablet", "Mobile"),
    Course = c("STATS101/G", "STATS108")
)
d <- d[rep(1:4, c(4, 3, 1, 2)),]
rownames(d) <- NULL
ctest <- chisq.test(table(d$Course, d$Machine), simulate = TRUE)
s1 <- getPlotSummary(Machine, Course, data = d, summary.type = "inference",
    inference.type = "conf",
    hypothesis.test = "chi2"
)
test_that("Simulated p-value is included when small expected values", {
    expect_match(
        paste(s1, collapse = "\n"), 
        "Simulated p-value (since some expected counts < 5) =",
        fixed = TRUE
    )
})

test_that("Simulated p-value is included when requested", {
    cas <- read.csv("cas.csv")
    s <- getPlotSummary(cellsource, gender, data = cas, 
        summary.type = "inference",
        inference.type = "conf",
        hypothesis.test = "chi2", 
        hypothesis.simulated.p.value = TRUE
    )
    expect_match(paste(s, collapse = "\n"), "Simulated p-value =")
})


# a giant table

# 2000?
# tab <- matrix(sample(2000, replace = TRUE), ncol = 50)
# system.time(chisq.test(tab, simulate = TRUE))[3]
