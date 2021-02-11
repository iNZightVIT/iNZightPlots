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


chis <- iNZightTools::smart_read("chis.csv")
# chis <- iNZightTools::smart_read("tests/testthat/chis.csv")
dchis <- suppressWarnings(svrepdesign(
    data = chis,
    repweights = "rakedw[1-9]",
    weights = ~rakedw0,
    type = "other", scale = 1, rscales = 1
))

test_that("Subsetting replicate weight surveys is correct", {
    dchis_male <- subset(dchis, sex=="male")
    p0 <- inzplot(~race | sex, data = dchis, g1.level = "male")
    p1 <- inzplot(~race, data = dchis_male)

    expect_equal(p0$all$male$tab, p1$all$all$tab)
})


# ## Missing values:
# apijk <- smart_read("apiclus2-jk1.csv")
# # apijk <- iNZightTools::smart_read("tests/testthat/apiclus2-jk1.csv")

# apides <- svrepdesign(weights = ~pw, repweights = "repw[0-9]+",
#     data = apijk, type = "JK1")
# svymean(~api00, des=apides)
# svymean(~enroll, des=apides, na.rm = TRUE)

# inzplot(~enroll, design = apides, inference.par="mean", inference.type="conf")

# tt <- iNZightTools::import_survey("~/postdoc/data/tekupenga/TeKupenga.svydesign")
# tt <- iNZightTools::convertToCat(tt$design, c("qTTTBeenToAncestral", "qTTTBeenToAncestral12Mths"))
# inzplot(~qTTTBeenToAncestral12Mths.cat | qTTTBeenToAncestral.cat, design = tt)

# table(tt$variables$qTTTBeenToAncestral.cat, tt$variables$qTTTBeenToAncestral12Mths.cat)

# inzplot(~qTTTBeenToAncestral12Mths.cat | qTTTBeenToAncestral.cat, design = tt, g1.level = 1)
