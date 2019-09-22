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
            # hypothesis.value = 600
        )
    )
    inz_inf <- 
        eval(parse(text = 
            gsub("p-", "p", 
                sprintf("list(%s)", inz_test[grep("p-value = ", inz_test)])
            )
        ))
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
    expect_equal(
        inz_inf,
        list(
            t = as.numeric(format(svy_test$statistic[[1]], digits = 5)),
            df = svy_test$parameter[[1]],
            pvalue = as.numeric(format.pval(svy_test$p.value[[1]]))
        )
    )

    ## test hypothesis vars are used
})

test_that("Two sample tests (dot plot by binary factor)", {
    svy_mean <- svyby(~api00, ~awards, dclus1, svymean)
    svy_ci <- confint(svy_mean)
    svy_test <- svyttest(api00~awards, dclus1)

    inz_test <- capture.output(
        getPlotSummary(api00, awards,
            design = dclus1,
            summary.type = "inference",
            inference.type = "conf"
        )
    )
    inz_inf <- 
        eval(parse(text = 
            gsub("p-", "p", 
                sprintf("list(%s)", inz_test[grep("p-value = ", inz_test)])
            )
        ))
    est_tbl <- 
        read.table(
            textConnection(
                inz_test[grep("Population Means with", inz_test) + c(2:4)]
            ),
            header = TRUE
        )

    expect_equal(
        est_tbl,
        data.frame(
            Lower = round(svy_ci[,1], 1), 
            Mean = round(svy_mean[,2], 1),
            Upper = round(svy_ci[,2], 1)
        )
    )
    expect_equal(
        inz_inf,
        list(
            t = as.numeric(format(svy_test$statistic[[1]], digits = 5)),
            df = svy_test$parameter[[1]],
            pvalue = as.numeric(format.pval(svy_test$p.value[[1]]))
        )
    )
})


test_that("ANOVA tests (dot plot by factor)", {
    svy_mean <- svyby(~growth, ~stype, dclus1, svymean)
    svy_ci <- confint(svy_mean)
    svy_test <- svyglm(growth ~ stype, dclus1)
    svy_ftest <- regTermTest(svy_test, ~stype)

    inz_test <- capture.output(
        getPlotSummary(growth, stype,
            design = dclus1,
            summary.type = "inference",
            inference.type = "conf"
        )
    )
    inz_inf <- 
        eval(parse(text = 
            gsub("and", ", ddf =", gsub("p-", "p", 
                sprintf("list(%s)", inz_test[grep("p-value = ", inz_test)])
            ))
        ))
    est_tbl <- 
        read.table(
            textConnection(
                inz_test[grep("Population Means with", inz_test) + c(2:5)]
            ),
            header = TRUE
        )

    expect_equal(
        est_tbl,
        data.frame(
            Lower = round(svy_ci[,1], 2), 
            Mean = round(svy_mean[,2], 2),
            Upper = round(svy_ci[,2], 2)
        )
    )
    expect_equal(
        inz_inf,
        list(
            F = as.numeric(format(svy_ftest$Ftest[[1]], digits = 5)),
            df = svy_ftest$df,
            ddf = svy_ftest$ddf,
            pvalue = as.numeric(format.pval(svy_ftest$p[[1]]))
        )
    )
})

