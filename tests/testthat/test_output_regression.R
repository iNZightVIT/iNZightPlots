cas <- read.csv("cas.csv", stringsAsFactors = TRUE)

# --- Summary: bar charts ---

test_that("summary.inzbar one-way output unchanged", {
    skip_if(!file.exists("fixtures/summary_bar_oneway.rds"))
    expected <- readRDS("fixtures/summary_bar_oneway.rds")
    actual <- inzsummary(~travel, data = cas)
    expect_identical(as.character(actual), as.character(expected))
})

test_that("summary.inzbar one-way vertical output unchanged", {
    skip_if(!file.exists("fixtures/summary_bar_oneway_vertical.rds"))
    expected <- readRDS("fixtures/summary_bar_oneway_vertical.rds")
    actual <- inzsummary(~travel, data = cas, table.direction = "vertical")
    expect_identical(as.character(actual), as.character(expected))
})

test_that("summary.inzbar two-way output unchanged", {
    skip_if(!file.exists("fixtures/summary_bar_twoway.rds"))
    expected <- readRDS("fixtures/summary_bar_twoway.rds")
    actual <- inzsummary(travel ~ gender, data = cas)
    expect_identical(as.character(actual), as.character(expected))
})

test_that("summary.inzbar two-way vertical output unchanged", {
    skip_if(!file.exists("fixtures/summary_bar_twoway_vertical.rds"))
    expected <- readRDS("fixtures/summary_bar_twoway_vertical.rds")
    actual <- inzsummary(travel ~ gender, data = cas,
        table.direction = "vertical")
    expect_identical(as.character(actual), as.character(expected))
})

# --- Summary: dot plots ---

test_that("summary.inzdot output unchanged", {
    skip_if(!file.exists("fixtures/summary_dot.rds"))
    expected <- readRDS("fixtures/summary_dot.rds")
    actual <- inzsummary(~Sepal.Length, data = iris)
    expect_identical(as.character(actual), as.character(expected))
})

test_that("summary.inzdot by-group output unchanged", {
    skip_if(!file.exists("fixtures/summary_dot_by.rds"))
    expected <- readRDS("fixtures/summary_dot_by.rds")
    actual <- inzsummary(Sepal.Length ~ Species, data = iris)
    expect_identical(as.character(actual), as.character(expected))
})

# --- Summary: scatter plots ---

test_that("summary.inzscatter linear output unchanged", {
    skip_if(!file.exists("fixtures/summary_scatter_linear.rds"))
    expected <- readRDS("fixtures/summary_scatter_linear.rds")
    actual <- inzsummary(Sepal.Length ~ Sepal.Width, data = iris,
        trend = "linear")
    expect_identical(as.character(actual), as.character(expected))
})

test_that("summary.inzscatter quadratic output unchanged", {
    skip_if(!file.exists("fixtures/summary_scatter_quad.rds"))
    expected <- readRDS("fixtures/summary_scatter_quad.rds")
    actual <- inzsummary(Sepal.Length ~ Sepal.Width, data = iris,
        trend = c("linear", "quadratic"))
    expect_identical(as.character(actual), as.character(expected))
})

# --- Inference: bar charts ---

test_that("inference.inzbar one-way output unchanged", {
    skip_if(!file.exists("fixtures/inference_bar_oneway.rds"))
    expected <- readRDS("fixtures/inference_bar_oneway.rds")
    actual <- inzinference(~travel, data = cas)
    expect_identical(as.character(actual), as.character(expected))
})

test_that("inference.inzbar two-way output unchanged", {
    skip_if(!file.exists("fixtures/inference_bar_twoway.rds"))
    expected <- readRDS("fixtures/inference_bar_twoway.rds")
    actual <- inzinference(travel ~ gender, data = cas)
    expect_identical(as.character(actual), as.character(expected))
})

# --- Inference: dot plots ---

test_that("inference.inzdot one-sample output unchanged", {
    skip_if(!file.exists("fixtures/inference_dot_one.rds"))
    expected <- readRDS("fixtures/inference_dot_one.rds")
    actual <- inzinference(~Sepal.Length, data = iris)
    expect_identical(as.character(actual), as.character(expected))
})

test_that("inference.inzdot two-sample output unchanged", {
    skip_if(!file.exists("fixtures/inference_dot_two.rds"))
    expected <- readRDS("fixtures/inference_dot_two.rds")
    actual <- inzinference(Sepal.Length ~ Species, data = iris)
    expect_identical(as.character(actual), as.character(expected))
})

# --- Inference: scatter ---

test_that("inference.inzscatter output unchanged", {
    skip_if(!file.exists("fixtures/inference_scatter.rds"))
    expected <- readRDS("fixtures/inference_scatter.rds")
    actual <- inzinference(Sepal.Length ~ Sepal.Width, data = iris,
        trend = "linear")
    expect_identical(as.character(actual), as.character(expected))
})
