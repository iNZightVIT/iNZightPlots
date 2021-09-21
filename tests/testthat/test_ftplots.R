context("Financial Times plots")

gg_pkgs <- c(
    "ggplot2",
    "dplyr",
    "tidyr",
    "forcats",
    "ggmosaic",
    "waffle",
    "ggthemes",
    "ggbeeswarm",
    "ggridges"
)
gg_pkgs_check <- sapply(gg_pkgs, requireNamespace, quietly = TRUE)
skip_if(any(!gg_pkgs_check), "Unable to check FT plots as some packages are missing.")

iris$Test.Cat.Var <- sample(letters[1:3], size = nrow(iris), replace = TRUE)

test_that("Barcode plots work", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode")
    expect_match(attr(p1, "code"), 'ggplot2::geom_point\\(shape = "\\|"\\)')
    
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_barcode")
    expect_match(attr(p1, "code"), 'ggplot2::geom_point\\(shape = "\\|"\\)')
    
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode2")
    expect_equal(p1$labels$y, "")
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode3")
    expect_equal(p1$labels$y, "")
})

test_that("Boxplots work", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_boxplot")
    expect_match(attr(p1, "code"), 'ggplot2::geom_boxplot()')
    
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_boxplot")
    expect_match(attr(p1, "code"), 'ggplot2::geom_boxplot()')
    expect_match(attr(p1, "code"), 'fill = Species')
})

test_that("Violin plots work", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin")
    expect_match(attr(p1, "code"), 'ggplot2::geom_violin()')
    
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_violin")
    expect_match(attr(p1, "code"), 'ggplot2::geom_violin()')
    expect_match(attr(p1, "code"), 'fill = Species')
})

test_that("Dotstrip plots work", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_dotstrip")
    expect_match(attr(p1, "code"), 'ggplot2::geom_point()')
    
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_dotstrip")
    expect_match(attr(p1, "code"), 'ggplot2::geom_point()')
    expect_match(attr(p1, "code"), 'colour = Species')
})

test_that("Beeswarm plots work", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_beeswarm")
    expect_match(attr(p1, "code"), 'ggbeeswarm::geom_beeswarm()')
    
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_beeswarm")
    expect_match(attr(p1, "code"), 'ggbeeswarm::geom_beeswarm()')
    expect_match(attr(p1, "code"), 'colour = Species')
    
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_quasirandom")
    expect_match(attr(p1, "code"), 'ggbeeswarm::geom_quasirandom()')
    
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_quasirandom")
    expect_match(attr(p1, "code"), 'ggbeeswarm::geom_quasirandom()')
    expect_match(attr(p1, "code"), 'colour = Species')
})

test_that("Density plots work", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_density")
    expect_match(attr(p1, "code"), 'ggplot2::geom_density()')
    
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_density")
    expect_match(attr(p1, "code"), 'ggplot2::geom_density()')
    expect_match(attr(p1, "code"), 'fill = Species')
})

test_that("Ridgeline plots work", {
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_ridgeline")
    expect_match(attr(p1, "code"), 'ggridges::geom_density_ridges()')
    expect_match(attr(p1, "code"), 'fill = Species')
})

test_that("Density plots work", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_cumcurve")
    expect_match(attr(p1, "code")[2], 'ggplot2::geom_step()')
    
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_cumcurve")
    expect_match(attr(p1, "code")[2], 'ggplot2::geom_step()')
    expect_match(attr(p1, "code")[2], 'colour = Species')
})

test_that("Lollipop count plots work", {
    p1 <- iNZightPlot(Species, data = iris, plottype = "gg_lollipop2")
    expect_match(attr(p1, "code")[2], 'ggplot2::geom_point')
    expect_match(attr(p1, "code")[2], 'ggplot2::geom_segment')
    
    p1 <- iNZightPlot(Species, Test.Cat.Var, data = iris, plottype = "gg_lollipop2")
    expect_match(attr(p1, "code")[2], 'ggplot2::geom_point')
    expect_match(attr(p1, "code")[2], 'ggplot2::geom_linerange')
    expect_match(attr(p1, "code")[2], 'colour = Test.Cat.Var')
})

test_that("Pie plots work", {
    p1 <- iNZightPlot(Species, data = iris, plottype = "gg_pie")
    expect_match(attr(p1, "code"), 'ggplot2::geom_bar')
})

test_that("Donut plots work", {
    p1 <- iNZightPlot(Species, data = iris, plottype = "gg_donut")
    expect_match(attr(p1, "code")[2], 'ggplot2::geom_rect')
})

test_that("Grid plots work", {
    p1 <- iNZightPlot(Species, data = iris, plottype = "gg_gridplot")
    expect_match(attr(p1, "code")[2], 'waffle::waffle')
})


test_that("Inversing x/y doesn't affect graph", {
    p1 <- iNZightPlot(Species, Sepal.Length, data = iris, plottype = "gg_violin")
    p2 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_violin")
    expect_equal(attr(p1, "code"), attr(p2, "code"))
})

test_that("Y-axis label for one-way numeric is blank", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode")
    expect_equal(p1$labels$y, "")
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode2")
    expect_equal(p1$labels$y, "")
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_barcode3")
    expect_equal(p1$labels$y, "")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_boxplot")
    expect_equal(p1$labels$x, "")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin")
    expect_equal(p1$labels$x, "")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_dotstrip")
    expect_equal(p1$labels$y, "")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_beeswarm")
    expect_equal(p1$labels$x, "")
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_quasirandom")
    expect_equal(p1$labels$x, "")
})

test_that("Plots can be rotated", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin",
        rotation = FALSE)
    expect_match(attr(p1, "code"), "coord_flip")

    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin",
        rotation = TRUE)
    expect_false(grepl("coord_flip", attr(p1, "code")))
    
    p1 <- iNZightPlot(Species, data = iris, plottype = "gg_gridplot", 
        rotation = TRUE)
    expect_match(attr(p1, "code")[2], "flip = TRUE")

    # p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin",
    #     rotation = TRUE, rotate_labels = list(x = TRUE))
    # expect_match(attr(p1, "code"), "coord_flip")
})

test_that("Plots can have themes", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin", 
        gg_theme = "bw")
    expect_match(attr(p1, "code"), "theme_bw")
    
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin", 
                      gg_theme = "tufte")
    expect_match(attr(p1, "code"), "theme_tufte")
})

test_that("General plot formatting works", {
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin", 
                      rotate_labels = list(x = TRUE, y = TRUE))
    expect_equal(p1$theme$axis.text.x$angle, 45)
    expect_equal(p1$theme$axis.text.y$angle, 45)
    
    p1 <- iNZightPlot(Sepal.Length, data = iris, plottype = "gg_violin",
        bg = "black")
    expect_equal(p1$theme$panel.background$fill, "black")
    
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_violin",
        caption = "Test caption")
    expect_equal(p1$labels$caption, "Test caption")
    
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_violin",
        cex = 2)
    expect_equal(p1$theme$text$size, 22)
})

test_that("Each type of palette works", {
    ## General ggplot palettes 
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_violin",
        palette = "Reds")
    expect_match(attr(p1, "code"), 'scale_fill_brewer\\(palette = \\"Reds\\"\\)')
    
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_cumcurve",
                      palette = "Reds")
    expect_match(attr(p1, "code")[2], 'scale_colour_brewer\\(palette = \\"Reds\\"\\)')
    
    iris$Test.Cat.Var <- sample(letters[1:3], size = nrow(iris), replace = TRUE)
    
    p1 <- iNZightPlot(Species, Test.Cat.Var, data = iris, plottype = "gg_heatmap",
                      palette = "Reds")
    expect_match(attr(p1, "code")[2], 'scale_fill_distiller\\(palette = \\"Reds\\"\\)')
    
    ## Viridis palettes
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_violin",
                      palette = "inferno")
    expect_match(attr(p1, "code"), 'scale_fill_viridis_d\\(option = \\"inferno\\"\\)')
    
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_cumcurve",
                      palette = "inferno")
    expect_match(attr(p1, "code")[2], 'scale_colour_viridis_d\\(option = \\"inferno\\"\\)')
    
    p1 <- iNZightPlot(Species, Test.Cat.Var, data = iris, plottype = "gg_heatmap",
                      palette = "inferno")
    expect_match(attr(p1, "code")[2], 'scale_fill_viridis_c\\(option = \\"inferno\\"\\)')
    
    ## Greyscale palette
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_violin",
                      palette = "greyscale")
    expect_match(attr(p1, "code"), 'scale_fill_grey')
    
    p1 <- iNZightPlot(Sepal.Length, Species, data = iris, plottype = "gg_cumcurve",
                      palette = "greyscale")
    expect_match(attr(p1, "code")[2], 'scale_colour_grey')
    
    p1 <- iNZightPlot(Species, Test.Cat.Var, data = iris, plottype = "gg_heatmap",
                      palette = "greyscale")
    expect_match(attr(p1, "code")[2], 'scale_fill_gradient')
})
