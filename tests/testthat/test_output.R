test_that("out_node creates object with correct class", {
    node <- out_node("test", value = 42)
    expect_s3_class(node, "out_test")
    expect_s3_class(node, "out_node")
    expect_equal(node$value, 42)
})
