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


test_that("Single proportion survey (binary variable)", {
    svy_prop <- svymean(~awards, dclus1)
    svy_ci <- confint(svy_prop)
    svy_Z <- (coef(svy_prop)[[1]] - 0.25) / SE(svy_prop)[[1]]
    svy_p <- pnorm(abs(svy_Z), lower.tail = FALSE)

    inz_test <- capture.output(
        getPlotSummary(awards,
            design = dclus1,
            summary.type = "inference",
            inference.type = "conf",
            hypothesis.test = "proportion",
            hypothesis.value = 0.25
        )
    )

    inz_inf <-
        eval(parse(text =
            gsub("Z-", "Z", gsub("p-", "p",
                sprintf("list(%s)", inz_test[grep("p-value = ", inz_test)])
            ))
        ))
    expect_equal(
        inz_inf,
        list(
            Zscore = as.numeric(format(svy_Z, digits = 5)),
            pvalue = as.numeric(format.pval(svy_p, digits = 5))
        )
    )
})

# test_that("Chi-square survey (one way bar plots)", {
#     svy_tab <- svytotal(~stype, dclus1)
#     svy_ci <- confint(svy_prop)

#     count <- coef(svy_tab)
#     svyglm(I(length(pw)) ~ 1, design = dclus1, family = quasipoisson())
#     svyglm(I(sum(pw)) ~ stype, design = dclus1, family = quasipoisson())

#     f1 <- glm(coef(svy_tab) ~ 1, family = quasipoisson())
#     f2 <- glm(coef(svy_tab) ~ names(svy_tab), family = quasipoisson())
#     survey:::anova.svyglm(f1, f2)

#     svy_test <- svychisq(~stype, dclus1)

#     inz_test <- capture.output(
#         getPlotSummary(awards, stype,
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


test_that("Subset inference - two sample t-test", {
    svy_list <- lapply(levels(apiclus1$stype),
        function(st)
            subset(dclus1, stype == st)
    )
    svy_means <- lapply(svy_list,
        function(d)
            svyby(~api00, ~awards, d, svymean)
    )
    # svy_cis <- lapply(svy_means, confint)
    svy_tests <- lapply(svy_list,
        function(d)
            svyttest(api00~awards, d)
    )

    inz_test <- capture.output(
        getPlotSummary(api00, awards,
            design = dclus1,
            g1 = stype,
            summary.type = "inference",
            inference.type = "conf"
        )
    )

    inz_tabs <- lapply(grep("Population Means", inz_test),
        function(i)
            est_tbl <-
                read.table(
                    textConnection(
                        inz_test[i + 2:4]
                    ),
                    header = TRUE
                )
    )
    svy_tabs <- lapply(svy_means,
        function(svy_mean) {
            svy_ci <- confint(svy_mean)
            data.frame(
                Lower = round(svy_ci[,1], 1),
                Mean = round(svy_mean[,2], 1),
                Upper = round(svy_ci[,2], 1)
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

    ## Add tests for the difference
})




test_that("Subset twice inference - one sample t-test", {
    svy_list <- lapply(levels(apiclus1$stype),
        function(g2)
            lapply(levels(apiclus1$awards),
                function(g1)
                    subset(dclus1, stype == g2 & awards == g1)
            )
    )
    svy_means <- lapply(svy_list,
        function(dl)
            lapply(dl,
                function(d)
                    svymean(~api00, d)
            )
    )
    svy_tests <- lapply(svy_list,
        function(dl)
            lapply(dl, function(d)
                svyttest(api00~1, d)
            )
    )

    inz_test <- capture.output(
        getPlotSummary(api00,
            design = dclus1,
            g1 = awards,
            g2 = stype, g2.level = "_MULTI",
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
    svy_tabs <- lapply(do.call(c, svy_means),
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
    svy_ests <- lapply(do.call(c, svy_tests),
        function(svy_test)
            list(
                t = as.numeric(format(svy_test$statistic[[1]], digits = 5)),
                df = svy_test$parameter[[1]],
                pvalue = as.numeric(format.pval(svy_test$p.value[[1]]))
            )
    )
    expect_equal(inz_ests, svy_ests)
})



test_that("Subset (only g2) inference - two sample t-test", {
    svy_list <- lapply(levels(apiclus1$stype),
        function(st)
            subset(dclus1, stype == st)
    )
    svy_means <- lapply(svy_list,
        function(d)
            svyby(~api00, ~awards, d, svymean)
    )
    # svy_cis <- lapply(svy_means, confint)
    svy_tests <- lapply(svy_list,
        function(d)
            svyttest(api00~awards, d)
    )

    assign("dclus1", dclus1, envir = .GlobalEnv)
    inz_test <- capture.output(
        getPlotSummary(api00, awards,
            design = dclus1,
            g2 = stype, g2.level = "_MULTI",
            summary.type = "inference",
            inference.type = "conf"
        )
    )
    rm(dclus1, envir = .GlobalEnv)

    inz_tabs <- lapply(grep("Population Means", inz_test),
        function(i)
            est_tbl <-
                read.table(
                    textConnection(
                        inz_test[i + 2:4]
                    ),
                    header = TRUE
                )
    )
    svy_tabs <- lapply(svy_means,
        function(svy_mean) {
            svy_ci <- confint(svy_mean)
            data.frame(
                Lower = round(svy_ci[,1], 1),
                Mean = round(svy_mean[,2], 1),
                Upper = round(svy_ci[,2], 1)
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


############################################ Replicate weight designs

chis <- iNZightTools::smart_read("chis.csv")
# chis <- iNZightTools::smart_read("tests/testthat/chis.csv")
dchis <- suppressWarnings(svrepdesign(
    data = chis,
    repweights = "rakedw[1-9]",
    weights = ~rakedw0,
    type = "other", scale = 1, rscales = 1
))

test_that("Replicate weight designs - one sample t-test", {
    z <- getPlotSummary(bmi_p,
        design = dchis,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Design-based One Sample t-test")

    z <- getPlotSummary(bmi_p,
        g1 = race,
        design = dchis,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Design-based One Sample t-test")
})

test_that("Replicate weight designs - two sample t-test", {
    z <- getPlotSummary(bmi_p, srsex,
        design = dchis,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Design-based Two Sample T-test")

    z <- getPlotSummary(bmi_p, srsex,
        g1 = race,
        design = dchis,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Design-based Two Sample T-test")
})

test_that("Replicate weight designs - ANOVA", {
    z <- getPlotSummary(bmi_p, race,
        design = dchis,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Wald test for race")

    z <- getPlotSummary(bmi_p, race,
        g1 = srsex,
        design = dchis,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Wald test for race")
})

test_that("Replicate weight designs - regression", {
    z <- suppressWarnings(getPlotSummary(bmi_p, ab22,
        design = dchis,
        summary.type = "inference",
        inference.type = "conf",
        trend = "linear"
    ))
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Linear Trend Coefficients")

    z <- suppressWarnings(getPlotSummary(bmi_p, ab22,
        g1 = race,
        design = dchis,
        summary.type = "inference",
        inference.type = "conf",
        trend = "linear"
    ))
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Linear Trend Coefficients")
})

test_that("Replicate weight designs - single proportion", {
    z <- getPlotSummary(srsex,
        design = dchis,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    # expect_output(print(z), "")

    z <- getPlotSummary(srsex,
        g1 = race,
        design = dchis,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    # expect_output(print(z), "")
})

test_that("Replicate weight designs - one way table", {
    z <- getPlotSummary(race,
        design = dchis,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    # expect_output(print(z), "")

    z <- getPlotSummary(race,
        g1 = srsex,
        design = dchis,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    # expect_output(print(z), "")
})

test_that("Replicate weight designs - two way table", {
    z <- getPlotSummary(race, srsex,
        design = dchis,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Chi-square test for equal distributions")

    z <- getPlotSummary(race, srsex,
        g1 = smoke,
        design = dchis,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Chi-square test for equal distributions")
})

############################################ Poststratified designs

data(api, package = "survey")
dclus1 <- svydesign(id = ~dnum, weights = ~pw,
    data = apiclus1, fpc = ~fpc)
pop.types <- data.frame(stype = c("E","H","M"), Freq = c(4421,755,1018))
dclus1p <- postStratify(dclus1, ~stype, pop.types)

test_that("Post-strat designs - one sample t-test", {
    z <- getPlotSummary(api00,
        design = dclus1p,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Design-based One Sample t-test")

    z <- getPlotSummary(api00,
        g1 = awards,
        design = dclus1p,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Design-based One Sample t-test")
})

test_that("Post-strat designs - two sample t-test", {
    z <- getPlotSummary(api00, sch.wide,
        design = dclus1p,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Design-based Two Sample T-test")

    z <- suppressWarnings(getPlotSummary(api00, awards,
        g1 = yr.rnd,
        design = dclus1p,
        summary.type = "inference",
        inference.type = "conf"
    ))
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Design-based Two Sample T-test")
})

test_that("Post-strat designs - ANOVA", {
    z <- getPlotSummary(api00, stype,
        design = dclus1p,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Wald test for stype")

    z <- suppressWarnings(getPlotSummary(api00, stype,
        g1 = awards,
        design = dclus1p,
        summary.type = "inference",
        inference.type = "conf"
    ))
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Wald test for stype")
})

test_that("Post-strat designs - regression", {
    z <- getPlotSummary(api99, api00,
        design = dclus1p,
        summary.type = "inference",
        inference.type = "conf",
        trend = "linear"
    )
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Linear Trend Coefficients")

    z <- suppressWarnings(getPlotSummary(api99, api00,
        g1 = awards,
        design = dclus1p,
        summary.type = "inference",
        inference.type = "conf",
        trend = "linear"
    ))
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Linear Trend Coefficients")
})

test_that("Post-strat designs - single proportion", {
    z <- getPlotSummary(awards,
        design = dclus1p,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    # expect_output(print(z), "")

    z <- getPlotSummary(awards,
        g1 = yr.rnd,
        design = dclus1p,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    # expect_output(print(z), "")
})

test_that("Post-strat designs - one way table", {
    z <- getPlotSummary(stype,
        design = dclus1p,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    # expect_output(print(z), "")

    z <- getPlotSummary(stype,
        g1 = awards,
        design = dclus1p,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    # expect_output(print(z), "")
})

test_that("Post-strat designs - two way table", {
    z <- getPlotSummary(stype, both,
        design = dclus1p,
        summary.type = "inference",
        inference.type = "conf"
    )
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Chi-square test for equal distributions")

    z <- suppressWarnings(
        getPlotSummary(stype, yr.rnd,
            g1 = awards,
            design = dclus1p,
            summary.type = "inference",
            inference.type = "conf"
        )
    )
    expect_is(z, "inzight.plotsummary")
    expect_output(print(z), "Chi-square test for equal distributions")
})
