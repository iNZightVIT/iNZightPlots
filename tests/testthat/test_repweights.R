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

# iNZightPlot(api00, api99, design = dclus2)
# iNZightPlot(api00, api99, design = r2)


test_that("a thing", {
    # iNZightPlot(alive, design = scdrep)
})
