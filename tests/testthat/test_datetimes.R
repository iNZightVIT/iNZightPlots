context("Dates and times")

quakes <- iNZightTools::smart_read("quakes.csv")
# quakes <- iNZightTools::smart_read("tests/testthat/quakes.csv")
quakes$felt <- as.factor(sample(c("yes", "no"), nrow(quakes), replace = TRUE))

test_that("Datetimes plot OK", {
    expect_is(iNZightPlot(origintime, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(origintime, magnitude, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(magnitude, origintime, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(origintime, felt, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(felt, origintime, data = quakes), "inzplotoutput")

    expect_is(iNZightPlot(magnitude, g1 = origintime, data = quakes),
        "inzplotoutput")
    expect_is(
        iNZightPlot(magnitude, 
            g1 = felt,
            g2 = origintime, 
            g2.level = "_MULTI", 
            data = quakes
        ),
        "inzplotoutput"
    )
})

quakes <- iNZightTools::extract_part(quakes, "origintime", "Date only", "date")
test_that("Dates plot OK", {
    expect_is(iNZightPlot(date, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(date, magnitude, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(magnitude, date, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(date, felt, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(felt, date, data = quakes), "inzplotoutput")

    expect_is(iNZightPlot(magnitude, g1 = date, data = quakes),
        "inzplotoutput")
    expect_is(
        iNZightPlot(magnitude, 
            g1 = felt,
            g2 = date, 
            g2.level = "_MULTI", 
            data = quakes
        ),
        "inzplotoutput"
    )
})

quakes <- iNZightTools::extract_part(quakes, "origintime", "Time only", "time")
test_that("Times plot OK", {
    expect_is(iNZightPlot(time, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(time, magnitude, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(magnitude, time, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(time, felt, data = quakes), "inzplotoutput")
    expect_is(iNZightPlot(felt, time, data = quakes), "inzplotoutput")

    expect_is(iNZightPlot(magnitude, g1 = time, data = quakes),
        "inzplotoutput")
    expect_is(
        iNZightPlot(magnitude, 
            g1 = felt,
            g2 = time, 
            g2.level = "_MULTI", 
            data = quakes
        ),
        "inzplotoutput"
    )
})
