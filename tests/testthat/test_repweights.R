context("Replicate weights")

data(scd, package = "survey")
repweights <- 2 *
    cbind(
        c(1,0,1,0,1,0),
        c(1,0,0,1,0,1),
        c(0,1,1,0,0,1),
        c(0,1,0,1,1,0)
    )
scdrep <- suppressWarnings(
    survey::svrepdesign(data = scd, type = "BRR", repweights = repweights,
        combined.weights = FALSE)
)
r <- svyratio(~alive, ~arrests, scdrep)$ratio[1]

data(api, package = "survey")
dclus2<-svydesign(id=~dnum+snum, fpc=~fpc1+fpc2, data=apiclus2)

r2 <- suppressWarnings(as.svrepdesign(dclus2))

# load_all()
# iNZightPlot(api00, api99, data = apiclus2)
# iNZightPlot(api00, api99, design = dclus2)

test_that("Replicate weight designed supported", {
    expect_is(
        iNZightPlot(alive, design = scdrep),
        "inzplotoutput"
    )
})

test_that("Surveys converted to replicate designs fail", {
    expect_error(
        iNZightPlot(api00, api99, design = r2),
        "not yet supported"
    )
})
