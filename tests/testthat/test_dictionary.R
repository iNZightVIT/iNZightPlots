library(iNZightTools)

cas_raw <- smart_read("cas500_coded.csv")
cas_dict <- read_dictionary("casdict.csv",
    name = "variable",
    title = "friendly_name"
)

cas <- apply_dictionary(cas_raw, cas_dict)

test_that("Variable lables used if present", {
    # devtools::load_all()
    expect_silent(inzplot(~height, cas))
    expect_silent(inzplot(~height, cas, plottype = "gg_barcode"))

    expect_silent(inzplot(~travel, cas, plottype = "gg_column"))
})
