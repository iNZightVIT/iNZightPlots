context("Replicate weights")

skip_if_offline()

chis <- try(iNZightTools::smart_read("https://inzight.nz/testdata/chis2.csv"), silent = TRUE)
skip_if(inherits(chis, "try-error"), "Unable to load resource")

dchis <- suppressWarnings(svrepdesign(
    data = chis,
    repweights = "rakedw[1-9]",
    weights = ~rakedw0,
    type = "other", scale = 1, rscales = 1
))

# data(api, package = "survey")
# dclus2<-svydesign(id=~dnum+snum, fpc=~fpc1+fpc2, data=apiclus2)

# r2 <- suppressWarnings(as.svrepdesign(dclus2))

test_that("Replicate weight designs supported - basic plots", {
    expect_is(iNZightPlot(bmi_p, design = dchis), "inzplotoutput")
    expect_is(iNZightPlot(bmi_p, smoke, design = dchis), "inzplotoutput")
    expect_is(iNZightPlot(smoke, sex, design = dchis), "inzplotoutput")
    expect_is(iNZightPlot(smoke, design = dchis), "inzplotoutput")
    expect_is(iNZightPlot(sex, design = dchis), "inzplotoutput")
    expect_is(
        suppressWarnings(iNZightPlot(bmi_p, rakedw0, design = dchis)),
        "inzplotoutput"
    )
})

test_that("Replicate weight designs supported - plot inference - hist", {
    expect_equivalent(
        as.numeric(
            iNZightPlot(bmi_p, design = dchis,
                inference.type = "conf", inference.par = "mean")$all$all$inference.info$mean$conf
        ),
        c(
            svymean(~bmi_p, design = dchis)[1],
            confint(svymean(~bmi_p, design = dchis))[1:2]
        )
    )

    expect_equivalent(
        as.matrix(
            iNZightPlot(bmi_p, sex, design = dchis,
                inference.type = "conf", inference.par = "mean")$all$all$inference.info$mean$conf
        ),
        cbind(
            svyby(~bmi_p, ~sex, dchis, svymean)[,2],
            confint(svyby(~bmi_p, ~sex, dchis, svymean))
        )
    )
})

test_that("Replicate weight designs supported - plot inference - scatter", {
    expect_silent(
        suppressWarnings(
            iNZightPlot(bmi_p, marit, design = dchis, trend = "linear")
        )
    )

    expect_silent(
        suppressWarnings(iNZightPlot(bmi_p, marit, design = dchis,
            colby = smoke)
        )
    )
})

test_that("Replicate weight designs supported - plot inference - bar", {
    ## one way
    inf <- iNZightPlot(smoke, design = dchis,
        inference.type = "conf", inference.par = "prop")$all$all$inference$conf
    expect_equivalent(
        inf$estimate,
        coef(svymean(~smoke, dchis))
    )
    expect_equivalent(
        rbind(inf$lower, inf$upper),
        t(confint(svymean(~smoke, dchis)))
    )

    ## two way
    inf <- iNZightPlot(smoke, sex, design = dchis,
        inference.type = "conf")$all$all$inference$conf
    sinf <- svyby(~smoke, ~sex, dchis, svymean)
    expect_equivalent(
        inf$estimate,
        coef(sinf)
    )
    expect_equivalent(
        cbind(
            as.numeric(inf$lower),
            as.numeric(inf$upper)
        ),
        confint(sinf)
    )
})
