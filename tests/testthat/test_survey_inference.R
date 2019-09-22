context("Survey design inference functions")

data(api, package = "survey")
dclus1 <- svydesign(id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc)

test_that("One sample t-test", {
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

test_that("Two sample t-test", {
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


test_that("ANOVA (equivalent)", {
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

test_that("Survey regression", {
    svy_test <- svyglm(api00 ~ api99, dclus1)
    svy_ci <- confint(svy_test)
    svy_coef <- summary(svy_test)$coef

    inz_test <- capture.output(
        getPlotSummary(api99, api00,
            design = dclus1,
            summary.type = "inference",
            inference.type = "conf",
            trend = "linear"
        )
    )

    est_tbl <- 
        read.table(
            textConnection(
                inz_test[grep("Linear Trend Coefficients", inz_test) + c(2:4)]
            ),
            header = TRUE
        )

    svy_tbl <- 
        data.frame(
            Estimate = as.numeric(sprintf("%.5g", svy_coef[, 1])),
            Lower = as.numeric(sprintf("%.5g", svy_ci[, 1])),
            Upper = as.numeric(sprintf("%.5g", svy_ci[, 2])),
            p.value = as.numeric(format.pval(svy_coef[, 4], digits = 2))
        )
    rownames(svy_tbl) <- c("Intercept", "api99")

    expect_equal(est_tbl, svy_tbl)
})

## These don't have a survey:: package function, so will have to investigate ...
# test_that("Chi-square survey (one way bar plots)", {
#     svy_prop <- svytotal(~stype, dclus1)
#     svy_ci <- confint(svy_prop)
#     svy_test <- svychisq(~stype, dclus1)

#     inz_test <- capture.output(
#         getPlotSummary(awards, stype,
#             # data = apiclus1,
#             design = dclus1,
#             summary.type = "inference",
#             inference.type = "conf"
#         )
#     )

# })

test_that("Two way Chi-square contingency tables", {
    svy_prop <- svyby(~stype, ~awards, dclus1, svymean)
    svy_ci <- confint(svy_prop)
    svy_test <- suppressWarnings(svychisq(~awards+stype, dclus1))

    inz_test <- suppressWarnings(capture.output(
        getPlotSummary(stype, awards,
            design = dclus1,
            summary.type = "inference",
            inference.type = "conf"
        )
    ))

    est_tbl <- 
        read.table(
            textConnection(
                inz_test[grep("Estimated Proportions", inz_test) + c(3:4)]
            ),
            header = FALSE,
            col.names = c("Level", "E", "H", "M", "Sums")
        )
    inz_inf <-         
        eval(parse(text = 
            gsub("\\^", "", gsub("and", ", df2 =", gsub("p-", "p", 
                sprintf("list(%s)", inz_test[grep("p-value = ", inz_test)])
            )))
        ))

    expect_equal(
        est_tbl,
        data.frame(
            Level = c("No", "Yes"),
            E = as.numeric(format(svy_prop[, 2], digits = 3)),
            H = as.numeric(format(svy_prop[, 3], digits = 3)),
            M = as.numeric(format(svy_prop[, 4], digits = 3)),
            Sums = rep(1, 2)
        )
    )

    expect_equal(
        inz_inf,
        list(
            X2 = signif(svy_test$statistic[[1]], 5),
            df = round(svy_test$parameter[[1]], 3),
            df2 = round(svy_test$parameter[[2]], 2),
            pvalue = as.numeric(format.pval(svy_test$p.value, digits = 5))
        )
    )
})




test_that("Subset inference - one sample t-test", {
    svy_list <- lapply(levels(apiclus1$stype),
        function(st) 
            subset(dclus1, stype == st)
    )
    svy_means <- lapply(svy_list, 
        function(d) 
            svymean(~api00, d)
    )
    # svy_cis <- lapply(svy_means, confint)
    svy_tests <- lapply(svy_list, 
        function(d) 
            svyttest(api00~1, d)
    )

    inz_test <- capture.output(
        getPlotSummary(api00,
            design = dclus1,
            g1 = stype,
            summary.type = "inference",
            inference.type = "conf"
        )
    )

    inz_tabs <- lapply(grep("Population Mean", inz_test),
        function(i) 
            est_tbl <- 
                read.table(
                    textConnection(
                        inz_test[i + 2:3]
                    ),
                    header = TRUE
                )
    )
    svy_tabs <- lapply(svy_means,
        function(svy_mean) {
            svy_ci <- confint(svy_mean)
            data.frame(
                Lower = round(svy_ci[[1]], 1), 
                Mean = round(svy_mean[[1]], 1),
                Upper = round(svy_ci[[2]], 1)
            )
        }
    )
    expect_equal(inz_tabs, svy_tabs)

    inz_ests <- lapply(grep("p-value", inz_test),
        function(i)
            eval(parse(text = 
                gsub("p-", "p", 
                    sprintf("list(%s)", inz_test[i])
                )
            ))
    )
    svy_ests <- lapply(svy_tests, 
        function(svy_test)
            list(
                t = as.numeric(format(svy_test$statistic[[1]], digits = 5)),
                df = svy_test$parameter[[1]],
                pvalue = as.numeric(format.pval(svy_test$p.value[[1]]))
            )
    )
    expect_equal(inz_ests, svy_ests)
})

