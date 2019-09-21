context("Survey design inference functions")

data(api, package = "survey")
dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

test_that("Dot plot inference tests", {
    svy_mean <- svymean(~api00, dclus1)
    svy_ci <- confint(svy_mean)
    svy_test <- svyttest(api00~1, dclus1)

    inz_test <- capture.output(
        getPlotSummary(api00,
            design = dclus1,
            summary.type = "inference",
            inference.type = "conf"
        )
    )
    est_tbl <- 
        read.table(
            textConnection(
                inz_test[grep("Population Mean with", inz_test) + c(2:3)]
            ),
            header = TRUE
        )
    expect_equal(
        est_tbl,
        data.frame(
            Lower = round(svy_ci[[1]], 1), 
            Mean = round(svy_mean[[1]], 1),
            Upper = round(svy_ci[[2]], 1)
        )
    )
})
