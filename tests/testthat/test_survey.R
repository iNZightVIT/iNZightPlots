context("Survey designs")

data(api, package = "survey")
dclus1 <- svydesign(id = ~dnum, weights = ~pw, 
    data = apiclus1, fpc = ~fpc)

test_that("Survey designs work", {
    expect_is(
        iNZightPlot(api00, api99, design = dclus1),
        "inzplotoutput"
    )
})

test_that("Summary information is correct - dot plot", {
    x <- getPlotSummary(enroll, design = dclus1)
    pe <- which(grepl("Population estimates", x)) + 3

    xpe <- gsub("\\|", "", x[pe])
    expect_equal(
        round(scan(textConnection(xpe), quiet = TRUE)),
        round(c(
            as.numeric(
                svyquantile(~enroll, 
                    design = dclus1, quantiles = c(0.25, 0.5, 0.75)
                )
            ),
            as.numeric(svymean(~enroll, design = dclus1)),
            sqrt(as.numeric(svyvar(~enroll, design = dclus1))),
            as.numeric(svytotal(~enroll, design = dclus1)),
            sum(weights(dclus1)),
            nrow(apiclus1),
            min(apiclus1$enroll),
            max(apiclus1$enroll)
        ))
    )

    ## standard errors ...
    # ...

    x <- getPlotSummary(enroll, stype, design = dclus1)
    pe <- which(grepl("Population estimates", x)) + 3:5
    xpe <- gsub("\\||[A-Z]", "", x[pe])
    expect_equivalent(
        round(do.call(
            rbind,
            lapply(xpe, function(z) round(scan(textConnection(z), quiet = TRUE)))
        )),
        round(cbind(
            as.matrix(svyby(~enroll, ~stype, dclus1, svyquantile, keep.var = FALSE,
                quantiles = c(0.25, 0.5, 0.75))[,-1]),
            mean=coef(svyby(~enroll, ~stype, dclus1, svymean)),
            sd=sqrt(coef(svyby(~enroll, ~stype, dclus1, svyvar))),
            total=coef(svyby(~enroll, ~stype, dclus1, svytotal)),
            pop=tapply(weights(dclus1), dclus1$variables$stype, sum),
            n=table(dclus1$variables$stype),
            min=tapply(dclus1$variables$enroll, dclus1$variables$stype, min),
            max=tapply(dclus1$variables$enroll, dclus1$variables$stype, max)
        ))
    )
})


