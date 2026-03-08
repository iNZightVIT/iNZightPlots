#!/usr/bin/env Rscript
# Generate snapshot fixtures for regression testing of the output refactoring.
# Run from the package root: Rscript tests/testthat/fixtures/generate_fixtures.R

library(iNZightPlots)

cas <- read.csv("tests/testthat/cas.csv", stringsAsFactors = TRUE)

fixtures <- list(
    # Summary - bar charts (cas has only categorical vars)
    summary_bar_oneway = inzsummary(~travel, data = cas),
    summary_bar_oneway_vertical = inzsummary(~travel, data = cas,
        table.direction = "vertical"),
    summary_bar_twoway = inzsummary(travel ~ gender, data = cas),
    summary_bar_twoway_vertical = inzsummary(travel ~ gender, data = cas,
        table.direction = "vertical"),

    # Summary - dot plots (numeric) using iris
    summary_dot = inzsummary(~Sepal.Length, data = iris),
    summary_dot_by = inzsummary(Sepal.Length ~ Species, data = iris),

    # Summary - scatter plots using iris
    summary_scatter_linear = inzsummary(Sepal.Length ~ Sepal.Width,
        data = iris, trend = "linear"),
    summary_scatter_quad = inzsummary(Sepal.Length ~ Sepal.Width,
        data = iris, trend = c("linear", "quadratic")),

    # Inference - bar charts
    inference_bar_oneway = inzinference(~travel, data = cas),
    inference_bar_twoway = inzinference(travel ~ gender, data = cas),

    # Inference - dot plots using iris
    inference_dot_one = inzinference(~Sepal.Length, data = iris),
    inference_dot_two = inzinference(Sepal.Length ~ Species, data = iris),

    # Inference - scatter using iris
    inference_scatter = inzinference(Sepal.Length ~ Sepal.Width,
        data = iris, trend = "linear"),

    # Additional iris fixtures
    summary_bar_iris = inzsummary(~Species, data = iris)
)

fixture_dir <- "tests/testthat/fixtures"
for (name in names(fixtures)) {
    saveRDS(fixtures[[name]],
        file.path(fixture_dir, paste0(name, ".rds")))
    cat("Saved:", name, "\n")
}

cat("\nAll", length(fixtures), "fixtures generated.\n")
