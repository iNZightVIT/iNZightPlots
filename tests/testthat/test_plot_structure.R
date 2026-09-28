context("plot structure dump")

first_panel <- function(s) {
    groups <- s[setdiff(names(s), c("gen", "xlim", "ylim", "meta"))]
    groups[[1L]][[1L]]
}

expect_json_safe <- function(x) {
    if (is.function(x) || is.environment(x) || isS4(x) ||
        inherits(x, "survey.design")) {
        fail("dump contains a value that is not JSON-safe")
    }
    if (is.list(x)) {
        lapply(x, expect_json_safe)
    }
    invisible(TRUE)
}

has_name <- function(x, nm) {
    if (!is.list(x)) {
        return(FALSE)
    }
    if (nm %in% names(x)) {
        return(TRUE)
    }
    any(vapply(x, has_name, logical(1), nm = nm))
}

expect_mirror <- function(p) {
    opts_names <- names(p$gen$opts)
    pal <- p$gen$opts$col.default
    s <- as_plot_structure(p)

    expect_identical(names(p$gen$opts), opts_names)
    expect_true(is.function(pal$cat))
    expect_true(is.function(p$gen$opts$col.default$cat))
    expect_null(attr(s, "._print_ctx"))
    expect_false("._print_ctx" %in% names(s))
    expect_false("design" %in% names(s))
    expect_false("main.design" %in% names(s))

    expect_named(
        s$meta,
        c(
            "varnames", "vartypes", "plottype", "glevels",
            "missing", "total.missing", "total.obs"
        ),
        ignore.order = TRUE
    )
    expect_false("col.default" %in% names(s$gen$opts))
    expect_false(any(vapply(s$gen$opts, is.function, logical(1))))
    expect_false(has_name(s, "svy"))
    expect_false(has_name(s, "args"))
    expect_false(has_name(s, "makeRects"))

    panel <- first_panel(s)
    expect_type(panel$class, "character")
    expect_length(panel$class, 1L)

    expect_json_safe(s)
    expect_type(jsonlite::toJSON(s), "character")
    s
}

test_that("scatter mirror keeps row ids and drops palette functions", {
    p <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris)
    s <- expect_mirror(p)
    panel <- first_panel(s)
    expect_equal(panel$class, "inzscatter")
    expect_equal(s$meta$plottype, "scatter")
    expect_true("point.order" %in% names(panel))
    expect_true("pch" %in% names(s$gen$opts))
    expect_false("hex" %in% names(panel))
})

test_that("bar mirror keeps matrices", {
    p <- iNZightPlot(Species, data = iris)
    s <- expect_mirror(p)
    panel <- first_panel(s)
    expect_equal(panel$class, "inzbar")
    expect_true(is.matrix(panel$phat))
    expect_true(is.matrix(panel$tab))
    expect_false("pch" %in% names(s$gen$opts))
})

test_that("dot mirror drops stack attributes", {
    p <- iNZightPlot(Sepal.Width, data = iris)
    s <- expect_mirror(p)
    panel <- first_panel(s)
    expect_equal(panel$class, "inzdot")
    expect_false("order" %in% names(panel))
    expect_null(attr(panel$toplot[[1L]], "order"))
})

test_that("histogram mirror uses inzhist", {
    p <- iNZightPlot(Sepal.Width, data = iris, plottype = "hist")
    s <- expect_mirror(p)
    expect_equal(first_panel(s)$class, "inzhist")
    expect_equal(s$meta$plottype, "hist")
})

test_that("hex mirror drops the S4 hexbin and keeps coordinates", {
    p <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris, plottype = "hex")
    s <- expect_mirror(p)
    panel <- first_panel(s)
    expect_equal(panel$class, "inzhex")
    expect_false("hex" %in% names(panel))
    expect_true("x" %in% names(panel))
    expect_true("y" %in% names(panel))
})

test_that("grid mirror drops the rectangle function and its inputs", {
    p <- suppressWarnings(
        iNZightPlot(Sepal.Width, Sepal.Length, data = iris, plottype = "grid")
    )
    s <- expect_mirror(p)
    panel <- first_panel(s)
    expect_equal(panel$class, "inzgrid")
    expect_false("makeRects" %in% names(panel))
    expect_false("args" %in% names(panel))
    expect_true("x" %in% names(panel))
})

test_that("survey scatter mirror drops the design", {
    data(api, package = "survey")
    dclus1 <- survey::svydesign(
        id = ~dnum, weights = ~pw, data = apiclus1, fpc = ~fpc
    )
    p <- iNZightPlot(api00, api99, design = dclus1)
    s <- expect_mirror(p)
    panel <- first_panel(s)
    expect_equal(panel$class, "inzscatter")
    expect_false("svy" %in% names(panel))
    expect_false(has_name(s, "design"))
    expect_true("x" %in% names(panel))
})

test_that("faceted scatter keeps g1 level names", {
    p <- iNZightPlot(Sepal.Width, Sepal.Length, g1 = Species, data = iris)
    s <- expect_mirror(p)
    expect_true(all(c("setosa", "versicolor", "virginica") %in% names(s$all)))
    expect_equal(s$all$setosa$class, "inzscatter")
    expect_false(is.null(s$meta$glevels))
})
