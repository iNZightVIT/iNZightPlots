context("Bootstrapping")

set.seed(100) # avoid a really bad seed ...

test_that("Numeric inference outputs correct values", {
    p <- inzplot(Sepal.Width ~ Species, data = iris,
        bs.inference = TRUE, inference.type = "conf")
    bs <- p$all$all$inference.info

    expect_true(
        all(abs(bs$mean$conf[, "mean"] -
            tapply(iris$Sepal.Width, iris$Species, mean)) < 0.01)
    )
    expect_true(
        all(abs(bs$median$conf[, "mean"] -
            tapply(iris$Sepal.Width, iris$Species, median)) < 0.01)
    )
    expect_true(
        all(abs(bs$iqr$conf[, "mean"] -
            tapply(iris$Sepal.Width, iris$Species, IQR, type = 4L)) < 0.5)
    )
})
