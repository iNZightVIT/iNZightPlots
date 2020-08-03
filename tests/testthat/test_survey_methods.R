context("Survey methods")

data(api, package = "survey")
dclus2 <- svydesign(id=~dnum+snum, fpc=~fpc1+fpc2, data=apiclus2)

data(scd, package = "survey")
repweights <- 2 *
    cbind(
        c(1,0,1,0,1,0),
        c(1,0,0,1,0,1),
        c(0,1,1,0,0,1),
        c(0,1,0,1,1,0)
    )
scdrep <- suppressWarnings(
    svrepdesign(data = scd, type = "BRR", repweights = repweights,
        combined.weights = FALSE)
)

test_that("Survey calls are correctly modified", {
    expect_equal(
        modifyCall(dclus2$call, "data", "newData"),
        "svydesign(id = ~dnum + snum, fpc = ~fpc1 + fpc2, data = newData)"
    )

    expect_equal(
        modifyCall(scdrep$call, "data", "newData"),
        "survey:::svrepdesign.default(data = newData, type = \"BRR\", repweights = repweights, combined.weights = FALSE)"
    )
})

test_that("Mean indicator uses correct weights", {
    expect_is(
        p <- inzplot(~api00, design = dclus2, mean_indicator = TRUE, plot = FALSE),
        "inzplotoutput"
    )
    expect_equivalent(p$all$all$meaninfo$all$mean, svymean(~api00, dclus2))
})
