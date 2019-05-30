context("Frequency counts (one- and two-way tables)")

cas <- suppressMessages(iNZightTools::smart_read("cas.csv"))
cas_freq <- suppressMessages(iNZightTools::smart_read("cas_freq.csv"))

test_that("One way tables give the same results", {
    expect_silent(
        p0 <- iNZightPlot(travel, data = cas, inference.type = "conf")
    )
    expect_silent(
        p1 <- iNZightPlot(travel, data = cas_freq, freq = count,
            inference.type = "conf")
    )

    expect_equivalent(p0$all$all$phat, p1$all$all$phat)

    expect_equivalent(
        p0$all$all$inference$conf,
        p1$all$all$inference$conf
    )
})


test_that("Two way tables give the same results", {
    expect_silent(
        p0 <- iNZightPlot(travel, gender, data = cas, inference.type = "conf")
    )
    expect_silent(
        p1 <- iNZightPlot(travel, gender, data = cas_freq, freq = count,
            inference.type = "conf")
    )

    expect_equivalent(
        unclass(p0$all$all$phat),
        unclass(p1$all$all$phat)
    )

    expect_equivalent(
        lapply(p0$all$all$inference$conf, unclass),
        lapply(p1$all$all$inference$conf, unclass)
    )
})

