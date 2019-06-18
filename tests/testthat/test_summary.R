context("Get Summary")

cas <- read.csv("cas.csv")

test_that("One-way table summaries are correct", {
    p <- getPlotSummary(travel, data = cas)
    p <- p[which(grepl("Summary of the distribution", p)) + 3:5]
    expect_equal(
        scan(text = p[1], what = character(), quiet = TRUE),
        c(levels(cas$travel), "Total")
    )
    expect_equivalent(
        scan(text = gsub("Count", "", p[2]), what = integer(), quiet = TRUE),
        c(table(cas$travel), nrow(cas))
    )
    expect_equivalent(
        scan(text = gsub("Percent|%", "", p[3]), what = double(), quiet = TRUE),
        c(table(cas$travel), nrow(cas)) / nrow(cas) * 100
    )
})

