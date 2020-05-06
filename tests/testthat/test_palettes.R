context("Colour palettes")

test_that("Names are valid", {
    expect_equal(
        names(viridis_palette_names()),
        c("viridis", "magma", "plasma", "inferno")
    )
    expect_equal(
        names(cat_palette_names()),
        c(
            "contrast", "bright", "light",
            names(viridis_palette_names()),
            "colourblind", "rainbow"
        )
    )
    expect_warning(const_palette_names())
    expect_equal(
        names(cont_palette_names()),
        c(
            names(viridis_palette_names()),
            "rainbow", "blue", "green", "red",
            "greenyellow", "redblue",
            "terrain", "heat",
            "bluewhitepink", "bluewhitered"
        )
    )
})

test_that("Palettes have the correct length", {
    n <- 3
    ok <- sapply(names(cat_palette_names()), function(x) {
        length(inzpalette(x)(n)) == n
    })
    expect_equal(sum(ok), length(ok))
})

test_that("Discrete palattes default when n is too large", {
    pal <- inzpar()$col.default$cat
    expect_equal(inzpalette("contrast")(10), pal(10))
    expect_equal(inzpalette("bright")(10), pal(10))
    expect_equal(inzpalette("light")(15), pal(15))
})

test_that("Emphasize works", {
    pal <- inzpalette("light")
    cols <- pal(5)
    ecols <- emphasize_pal_colour(5, 2, fn = pal)
    expect_equal(cols[2], ecols[2])
    expect_equal(as.character(shade(cols[-2], 0.7)), ecols[-2])
})
