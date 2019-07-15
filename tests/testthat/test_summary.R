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


data(api, package = "survey")
dclus2<-svydesign(id=~dnum+snum, fpc=~fpc1+fpc2, data=apiclus2)

test_that("Survey summaries are correct", {
    expect_is(getPlotSummary(api00, design = dclus2), 
        "inzight.plotsummary")
    expect_is(getPlotSummary(api00, sch.wide, design = dclus2), 
        "inzight.plotsummary")
    expect_is(getPlotSummary(api00, api99, design = dclus2),
        "inzight.plotsummary")
    expect_is(getPlotSummary(sch.wide, design = dclus2),
        "inzight.plotsummary")
    expect_is(getPlotSummary(sch.wide, awards, design = dclus2),
        "inzight.plotsummary")
    
})

chis <- iNZightTools::smart_read("chis.csv")
dchis <- suppressWarnings(svrepdesign(
    data = chis,
    repweights = "rakedw[1-9]",
    weights = ~rakedw0,
    type = "other", scale = 1, rscales = 1
))
test_that("Survey replicate design summaries are correct", {
    expect_is(getPlotSummary(bmi_p, design = dchis), "inzight.plotsummary")
    expect_is(getPlotSummary(bmi_p, sex, design = dchis), "inzight.plotsummary")
    expect_is(suppressWarnings(getPlotSummary(bmi_p, marit, design = dchis)), 
        "inzight.plotsummary")
    expect_is(getPlotSummary(sex, design = dchis), "inzight.plotsummary")
    expect_is(getPlotSummary(sex, smoke, design = dchis), "inzight.plotsummary")
})
