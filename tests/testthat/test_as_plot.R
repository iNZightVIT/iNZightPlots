context("plot drawing payload")

test_that("one-way bar matches species counts", {
    p <- iNZightPlot(Species, data = iris)
    pal <- p$gen$opts$col.default
    s <- as_plot(p)

    expect_true(is.function(pal$cat))
    expect_true(is.function(p$gen$opts$col.default$cat))
    expect_equal(s$schemaVersion, 1L)
    expect_equal(s$type, "bar")
    expect_equal(s$variables, list(v1 = "Species"))
    expect_null(s$layout)
    expect_length(s$panels, 1L)
    expect_null(s$panels[[1L]]$s1)
    expect_equal(s$panels[[1L]]$total, 150)
    expect_equal(
        s$panels[[1L]]$data,
        data.frame(
            label = c("setosa", "versicolor", "virginica"),
            count = c(50, 50, 50),
            proportion = c(50, 50, 50) / 150,
            stringsAsFactors = FALSE
        )
    )
    expect_type(jsonlite::toJSON(s, auto_unbox = TRUE, digits = NA), "character")
})

test_that("two-way bar keeps series proportions and pre-zoom totals", {
    d <- data.frame(
        Mode = factor(
            c(rep("Bus", 46), rep("Walk", 54)),
            levels = c("Bus", "Walk")
        ),
        Age = factor(
            c(rep("Child", 6), rep("Adult", 40), rep("Child", 14), rep("Adult", 40)),
            levels = c("Child", "Adult")
        )
    )
    s <- as_plot(iNZightPlot(Mode, Age, data = d))
    panel <- s$panels[[1L]]

    expect_equal(s$variables, list(v1 = "Mode", v2 = "Age"))
    expect_equal(panel$total, 100)
    expect_equal(
        panel$seriesTotals,
        data.frame(
            label = c("Child", "Adult"),
            total = c(20, 80),
            stringsAsFactors = FALSE
        )
    )
    expect_equal(
        panel$data,
        data.frame(
            label = c("Bus", "Bus", "Walk", "Walk"),
            series = c("Child", "Adult", "Child", "Adult"),
            count = c(6, 40, 14, 40),
            proportion = c(0.3, 0.5, 0.7, 0.5),
            stringsAsFactors = FALSE
        )
    )
})

test_that("segmented bar emits factor order and cell counts", {
    d <- data.frame(
        Day = factor(c(rep("Mon", 30), rep("Tue", 70)), levels = c("Mon", "Tue")),
        Weather = factor(
            c(rep("Rain", 12), rep("Dry", 18), rep("Rain", 14), rep("Dry", 56)),
            levels = c("Rain", "Dry")
        )
    )
    s <- as_plot(iNZightPlot(Day, colby = Weather, data = d))

    expect_equal(s$variables$colby, "Weather")
    expect_null(s$variables$v2)
    panel <- s$panels[[1L]]
    expect_equal(
        panel$data,
        data.frame(
            label = c("Mon", "Tue"),
            count = c(30, 70),
            proportion = c(0.3, 0.7),
            stringsAsFactors = FALSE
        )
    )
    expect_equal(
        panel$segments,
        data.frame(
            label = c("Mon", "Mon", "Tue", "Tue"),
            segment = c("Rain", "Dry", "Rain", "Dry"),
            count = c(12, 18, 14, 56),
            proportion = c(0.4, 0.6, 0.2, 0.8),
            stringsAsFactors = FALSE
        )
    )
})

test_that("segment counts stay below the bar when colby is missing", {
    d <- data.frame(
        Day = factor(rep("Mon", 5)),
        Weather = factor(c("Rain", "Rain", "Dry", "Dry", NA), levels = c("Rain", "Dry"))
    )
    panel <- as_plot(iNZightPlot(Day, colby = Weather, data = d))$panels[[1L]]
    expect_equal(panel$data$count, 5)
    expect_equal(panel$segments$segment, c("Rain", "Dry"))
    expect_equal(panel$segments$count, c(2, 2))
})

test_that("dot groups stack observed values and keep the box R computed", {
    d <- data.frame(
        sw = c(1, 1, 2, 5, 5, 5, 6),
        g = factor(c("a", "a", "a", "b", "b", "b", "b"))
    )
    s <- as_plot(iNZightPlot(sw, g, data = d))
    groups <- s$panels[[1L]]$groups

    expect_equal(s$type, "dot")
    expect_equal(s$variables, list(v1 = "sw", v2 = "g"))
    expect_equal(groups[[1L]]$label, "a")
    expect_equal(groups[[1L]]$points, data.frame(x = c(1, 1, 2)))
    expect_equal(
        groups[[1L]]$boxplot,
        list(min = 1, q1 = 1, median = 1, q3 = 1.5, max = 2)
    )
    expect_equal(
        groups[[2L]]$points,
        data.frame(x = c(5, 5, 5, 6))
    )
    expect_false(is.null(groups[[2L]]$boxplot))
})

test_that("one-way dot omits the group label", {
    s <- as_plot(iNZightPlot(Sepal.Width, data = iris))
    expect_equal(s$type, "dot")
    expect_null(s$panels[[1L]]$groups[[1L]]$label)
    expect_equal(nrow(s$panels[[1L]]$groups[[1L]]$points), 150L)
})

test_that("histogram uses the server edges and counts", {
    s <- as_plot(iNZightPlot(Sepal.Width, data = iris, plottype = "hist", hist.bins = 5))
    panel <- s$panels[[1L]]

    expect_equal(s$type, "hist")
    expect_equal(panel$groups[[1L]]$counts, c(11, 46, 68, 21, 4))
    expect_length(panel$edges, length(panel$groups[[1L]]$counts) + 1L)
    expect_equal(
        panel$groups[[1L]]$boxplot,
        list(min = 2, q1 = 2.8, median = 3, q3 = 3.3, max = 4.4)
    )
    expect_null(panel$groups[[1L]]$label)
})

test_that("scatter sends row ids and omits constant size and symbol", {
    p <- iNZightPlot(Sepal.Width, Sepal.Length, data = iris)
    s <- as_plot(p)
    pts <- s$panels[[1L]]$data

    expect_equal(s$type, "scatter")
    expect_equal(s$variables, list(v1 = "Sepal.Width", v2 = "Sepal.Length"))
    expect_s3_class(pts, "data.frame")
    expect_equal(nrow(pts), 150L)
    expect_false(any(c("size", "symbol", "colby") %in% names(pts)))
    expect_setequal(pts$id, as.integer(p$all$all$point.order))
    expect_equal(pts$x[[1L]], p$all$all$x[[1L]])
})

test_that("scatter sends colby, symbol, highlight, and varying size", {
    d <- data.frame(
        height = c(160, 171, 182),
        weight = c(54, 66, 79),
        grp = factor(c("B", "A", "A"), levels = c("A", "B")),
        sym = factor(c("p", "q", "p"))
    )
    s <- as_plot(iNZightPlot(
        height, weight,
        data = d, colby = grp, symbolby = sym, highlight = 2
    ))
    pts <- s$panels[[1L]]$data

    expect_equal(s$variables$colby, "grp")
    expect_equal(s$variables$symbolby, "sym")
    expect_s3_class(pts$colby, "factor")
    expect_equal(as.character(pts$colby[pts$id == 1L]), "B")
    expect_equal(pts$symbol[pts$id == 1L], 21L)
    expect_false(pts$highlight[pts$id == 1L])
    expect_true(pts$highlight[pts$id == 2L])
    expect_equal(as.character(pts$colby[pts$id == 3L]), "A")
    expect_false("size" %in% names(pts))

    sized <- data.frame(x = 1:3, y = 1:3, s = c(1, 4, 9))
    sp <- iNZightPlot(x, y, data = sized, sizeby = s)
    sized_plot <- as_plot(sp)
    expect_equal(sized_plot$variables$sizeby, "s")
    sizes <- sized_plot$panels[[1L]]$data$size
    expect_equal(sizes, as.numeric(sp$all$all$propsize / sp$gen$opts$cex.pt))
})

test_that("hex sends occupied cells and drops raw coordinates", {
    s <- as_plot(iNZightPlot(
        Sepal.Width, Sepal.Length,
        data = iris, plottype = "hex", hex.bins = 5
    ))
    panel <- s$panels[[1L]]

    expect_equal(s$type, "hex")
    expect_equal(panel$xBins, 5L)
    expect_equal(panel$shape, 1)
    expect_s3_class(panel$data, "data.frame")
    expect_equal(sum(panel$data$count), 150)
    expect_true(all(c("x", "y", "count", "meanX", "meanY") %in% names(panel$data)))
})

test_that("grid counts unweighted points on the shared window", {
    d <- data.frame(x = c(1, 2.4, 2.6), y = c(1, 2.8, 2.9))
    p <- suppressWarnings(
        iNZightPlot(x, y, data = d, plottype = "grid", scatter.grid.bins = 4)
    )
    panel <- as_plot(p)$panels[[1L]]

    expect_equal(panel$n, 4L)
    expect_equal(panel$xBounds, as.numeric(p$xlim))
    expect_equal(panel$yBounds, as.numeric(p$ylim))
    expect_s3_class(panel$data, "data.frame")
    expect_equal(sum(panel$data$count), 3)
})

test_that("an s1 facet is a flat list of panels", {
    s <- as_plot(iNZightPlot(Sepal.Width, Sepal.Length, g1 = Species, data = iris))

    expect_false(s$layout$matrix)
    expect_equal(s$variables$s1, "Species")
    expect_equal(
        vapply(s$panels, `[[`, character(1), "s1"),
        c("setosa", "versicolor", "virginica")
    )
    expect_null(s$panels[[1L]]$s2)
})

test_that("a named s1 level is not a facet", {
    s <- as_plot(iNZightPlot(
        Sepal.Width, Sepal.Length,
        g1 = Species, g1.level = "setosa", data = iris
    ))

    expect_equal(s$variables$s1, "Species")
    expect_null(s$layout)
    expect_length(s$panels, 1L)
    expect_equal(s$panels[[1L]]$s1, "setosa")
    expect_null(s$panels[[1L]]$s2)
    expect_equal(nrow(s$panels[[1L]]$data), sum(iris$Species == "setosa"))
})

test_that("a named s2 level is recorded and _ALL is not", {
    iris2 <- iris
    iris2$hand <- factor(rep(c("left", "right"), length.out = nrow(iris)))

    fixed <- as_plot(iNZightPlot(
        Sepal.Width, Sepal.Length,
        g1 = Species, g2 = hand, g2.level = "left", data = iris2
    ))
    expect_equal(fixed$variables$s2, "hand")
    expect_false(fixed$layout$matrix)
    expect_equal(unique(vapply(fixed$panels, `[[`, character(1), "s2")), "left")

    collapsed <- as_plot(iNZightPlot(
        Sepal.Width, Sepal.Length,
        g1 = Species, g2 = hand, g2.level = "_ALL", data = iris2
    ))
    expect_null(collapsed$panels[[1L]]$s2)
})

test_that("an s1 by s2 matrix keeps row then column order", {
    iris2 <- iris
    iris2$s2 <- iris2$Species
    s <- as_plot(iNZightPlot(
        Sepal.Length, Sepal.Width,
        g1 = Species, g2 = s2, g2.level = "_MULTI", data = iris2
    ))

    expect_true(s$layout$matrix)
    expect_length(s$panels, 9L)
    expect_equal(s$panels[[1L]]$s2, "setosa")
    expect_equal(s$panels[[1L]]$s1, "setosa")
    expect_equal(s$panels[[2L]]$s1, "versicolor")
    expect_equal(s$panels[[4L]]$s2, "versicolor")
    expect_equal(nrow(s$panels[[1L]]$data), 50L)
    expect_equal(nrow(s$panels[[2L]]$data), 0L)
    expect_equal(names(s$panels[[2L]]$data), c("id", "x", "y"))
})

test_that("as_plot rejects anything that is not a plot", {
    expect_error(as_plot(iris), "inzplotoutput")
})
