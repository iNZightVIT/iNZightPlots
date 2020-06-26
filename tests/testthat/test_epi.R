context("Epidemiological inference functions")

## All `test.mat` are based on examples from the following
## Boston University School of Public Health CI online course:
## https://sphweb.bumc.bu.edu/otlt/MPH-Modules/BS/BS704_Confidence_Intervals/index.html

test_that("odds ratios are calculated correctly", {
  test.mat <- matrix(c(57, 10, 6, 7), ncol = 2)
  test.or <- calculate_or(test.mat)
  
  expect_equivalent(round(test.or["estimate"], 2), 6.65)
  expect_equivalent(round(test.or["ci.lwr"], 2), 1.85)
  expect_equivalent(round(test.or["ci.upr"], 2), 23.94)
})

test_that("risk ratios are calculated correctly", {
  test.mat <- matrix(c(29, 41, 20, 9), ncol = 2)
  test.rr <- calculate_rr(test.mat)
  
  expect_equivalent(round(test.rr["estimate"], 2), 0.44)
  expect_equivalent(round(test.rr["ci.lwr"], 2), 0.22)
  expect_equivalent(round(test.rr["ci.upr"], 2), 0.87)
})

test_that("risk differences are calculated correctly", {
  test.mat <- matrix(c(2757, 663, 298, 81), ncol = 2)
  test.rd <- calculate_rd(test.mat)
  
  expect_equivalent(round(test.rd["estimate"], 4), 0.0113)
  expect_equivalent(round(test.rd["ci.lwr"], 4), -0.0134)
  expect_equivalent(round(test.rd["ci.upr"], 4), 0.0361)
})

test_that("all 2x2 table functions throw appropriate errors", {
  test.mat <- matrix(sample(1:100, size = 3 * 2), ncol = 2, nrow = 3)
  
  expect_error(calculate_or(test.mat), "2x2")
  expect_error(calculate_rr(test.mat), "2x2")
  expect_error(calculate_rd(test.mat), "2x2")
})
