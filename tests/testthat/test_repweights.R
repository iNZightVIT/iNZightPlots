context("Replicate weights")

data(scd, package = "survey")
repweights <- 2 *
    data.frame(
        weights.1 = c(1,0,1,0,1,0),
        weights.2 = c(1,0,0,1,0,1),
        weights.3 = c(0,1,1,0,0,1),
        weights.4 = c(0,1,0,1,1,0)
    )
scd$ESAcat <- as.factor(scd$ESA)
scd$ambulancecat <- as.factor(scd$ambulance)
scd <- cbind(scd, repweights)
scdrep <- suppressWarnings(
    survey::svrepdesign(data = scd, type = "BRR", repweights = "weights.*",
        combined.weights = FALSE)
)

data(api, package = "survey")
dclus2<-svydesign(id=~dnum+snum, fpc=~fpc1+fpc2, data=apiclus2)

r2 <- suppressWarnings(as.svrepdesign(dclus2))

# load_all()
# iNZightPlot(api00, api99, data = apiclus2)
# iNZightPlot(api00, api99, design = dclus2)

test_that("Replicate weight designs supported - basic plots", {
    expect_is(iNZightPlot(alive, design = scdrep), "inzplotoutput")
    expect_is(iNZightPlot(alive, ESAcat, design = scdrep), "inzplotoutput")
    expect_is(iNZightPlot(alive, arrests, design = scdrep), "inzplotoutput")
    expect_is(iNZightPlot(ESAcat, design = scdrep), "inzplotoutput")
    expect_is(iNZightPlot(ambulancecat, design = scdrep), "inzplotoutput")
    expect_is(
        suppressWarnings(iNZightPlot(ESAcat, ambulancecat, design = scdrep)),
        "inzplotoutput"
    )
})

test_that("Replicate weight designs supported - plot inference - hist", {
    expect_equivalent(
        as.numeric(
            iNZightPlot(alive, design = scdrep,
                inference.type = "conf", inference.par = "mean")$all$all$inference.info$mean$conf
        ),
        c(
            svymean(~alive, design = scdrep)[1],
            confint(svymean(~alive, design = scdrep))[1:2]
        )
    )

    expect_equivalent(
        as.matrix(
            iNZightPlot(alive, ESAcat, design = scdrep,
                inference.type = "conf", inference.par = "mean")$all$all$inference.info$mean$conf
        ),
        cbind(
            svyby(~alive, ~ESAcat, scdrep, svymean)[,2],
            confint(svyby(~alive, ~ESAcat, scdrep, svymean))
        )
    )
})

test_that("Replicate weight designs supported - plot inference - scatter", {
    expect_silent(
        iNZightPlot(alive, arrests, design = scdrep, trend = "linear")
    )

    expect_silent(
        iNZightPlot(alive, arrests, design = scdrep,
            colby = ambulancecat)
    )
})

test_that("Replicate weight designs supported - plot inference - bar", {
    ## one way
    inf <- iNZightPlot(ambulancecat, design = scdrep,
        inference.type = "conf", inference.par = "prop")$all$all$inference$conf
    expect_equivalent(
        inf$estimate,
        svymean(~ambulancecat, scdrep)[1:2]
    )
    expect_equivalent(
        rbind(inf$lower, inf$upper),
        t(confint(svymean(~ambulancecat, scdrep)))
    )

    ## two way
    inf <- iNZightPlot(ambulancecat, ESAcat, design = scdrep,
        inference.type = "conf")$all$all$inference$conf
    sinf <- svyby(~ambulancecat, ~ESAcat, scdrep, svymean)
    expect_equivalent(
        inf$estimate,
        as.matrix(sinf[,2:3])
    )
    expect_equivalent(
        cbind(
            as.numeric(inf$lower),
            as.numeric(inf$upper)
        ),
        confint(sinf)
    )
})

test_that("Surveys converted to replicate designs fail", {
    expect_error(
        iNZightPlot(api00, api99, design = r2),
        "not yet supported"
    )
})
