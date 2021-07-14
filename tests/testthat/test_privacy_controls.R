test_that("Privacy control list created correctly", {
    pc <- make_privacy_controls(list(suppression = 10))
    expect_equal(names(pc), c("round", "suppression_matrix", "suppress"))
    expect_is(pc$round, "function")
    expect_is(pc$suppression_matrix, "function")
    expect_is(pc$suppress, "function")
})

test_that("Suppression matrix created correctly", {
    pc <- make_privacy_controls(list(suppression = 10))

    tab <- as.table(c(A = 20, B = 30, C = 5))
    st <- pc$suppression_matrix(tab)
    expect_equivalent(st, c(FALSE, FALSE, TRUE, TRUE))
    expect_equivalent(
        pc$suppress(c(tab, sum(tab)), st),
        c("20", "30", "S", "S")
    )

    tab <- as.table(cbind(c(20, 30, 5), c(30, 40, 8), c(5, 20, 40)))
    st <- pc$suppression_matrix(tab)
    expect_equivalent(rowSums(st), c(2, 0, 2))
    expect_equivalent(
        pc$suppress(cbind(tab, rowSums(tab)), st),
        cbind(c(20, 30, "S"), c(30, 40, "S"), c("S", 20, 40), c("S", 90, 53))
    )
})
