test_that("out_node creates object with correct class", {
    node <- out_node("test", value = 42)
    expect_s3_class(node, "out_test")
    expect_s3_class(node, "out_node")
    expect_equal(node$value, 42)
})

# --- out_table ---------------------------------------------------------------

test_that("out_table stores raw numeric matrix", {
    m <- matrix(c(1, 22, 333, 4444), nrow = 2)
    node <- out_table(m, digits = 0L)
    expect_s3_class(node, "out_table")
    expect_s3_class(node, "out_node")
    expect_equal(node$mat, m)
    expect_equal(node$digits, 0L)
    lines <- format_plain(node)
    expect_match(lines[1], "^   ")
    expect_equal(length(lines), 2)
})

test_that("out_table adds col_headers and row_headers", {
    m <- matrix(c(1.23, 4.56), nrow = 1)
    node <- out_table(m,
        col_headers = c("A", "B"),
        row_headers = "Row1",
        digits = 2L
    )
    expect_equal(node$col_headers, c("A", "B"))
    expect_equal(node$row_headers, "Row1")
    lines <- format_plain(node)
    expect_equal(length(lines), 2) # header + 1 data row
    expect_match(lines[1], "A")
    expect_match(lines[2], "Row1")
})

test_that("out_table handles separator_after", {
    m <- matrix(1:6, nrow = 3)
    node <- out_table(m,
        col_headers = c("X", "Y"),
        separator_after = 2L,
        digits = 0L
    )
    lines <- format_plain(node)
    expect_match(lines[4], "^\\s+-+$") # separator after 2nd data row
})

test_that("out_table replaces NA and NaN", {
    m <- matrix(c(1, NA, NaN, 4), nrow = 2)
    node <- out_table(m, digits = 0L)
    lines <- format_plain(node)
    rendered <- paste(lines, collapse = " ")
    expect_false(grepl("NA", rendered))
    expect_false(grepl("NaN", rendered))
    expect_match(rendered, "\\|") # NaN -> "|"
})

test_that("out_table reproduces summary.inzbar one-way output pattern", {
    tab <- c(bus = 73, motor = 17, other = 7, train = 3)
    total <- sum(tab)
    perc <- tab / total * 100

    count_row <- c(tab, total)
    pct_row <- c(perc, 100)
    mat_vals <- rbind(count_row, pct_row)

    node <- out_table(
        mat_vals,
        col_headers = c(names(tab), "Total"),
        row_headers = c("Count", "Percent"),
        digits = 0L
    )
    expect_equal(node$mat[1, 1], 73)
    expect_equal(node$mat[2, 5], 100)
    lines <- format_plain(node)
    expect_equal(length(lines), 3) # header, count, percent
})

test_that("out_table accepts data.frame input", {
    df <- data.frame(a = 1:3, b = 4:6)
    node <- out_table(df, digits = 0L)
    expect_true(is.matrix(node$mat))
    expect_equal(nrow(node$mat), 3)
})

test_that("out_table per-column justify works", {
    m <- matrix(c(1, 2, 3, 4), nrow = 2)
    node <- out_table(m,
        col_headers = c("Left", "Right"),
        digits = 0L,
        justify = c("left", "right")
    )
    lines <- format_plain(node)
    expect_equal(length(lines), 3) # header + 2 rows
})

# --- text primitives ---------------------------------------------------------

test_that("out_text creates text node", {
    node <- out_text("hello ", "world")
    expect_s3_class(node, "out_text")
    expect_equal(node$text, "hello world")
    expect_equal(format_plain(node), "hello world")
})

test_that("out_text stores styling attributes", {
    node <- out_text("bold text", .bold = TRUE, .colour = "red")
    expect_true(node$.bold)
    expect_equal(node$.colour, "red")
    # plain text ignores styling
    expect_equal(format_plain(node), "bold text")
})

test_that("out_blank produces empty lines", {
    node <- out_blank(3L)
    expect_s3_class(node, "out_blank")
    lines <- format_plain(node)
    expect_equal(lines, c("", "", ""))
})

test_that("out_bullet formats bulleted list", {
    node <- out_bullet(c("item1", "item2", "item3"))
    lines <- format_plain(node)
    expect_equal(length(lines), 3)
    expect_equal(lines[1], "  * item1")
    expect_equal(lines[2], "  * item2")
})

test_that("out_bullet custom indent and bullet", {
    node <- out_bullet(c("a", "b"), indent = 4L, bullet = "- ")
    lines <- format_plain(node)
    expect_equal(lines[1], "    - a")
})

test_that("out_indent wraps another node", {
    inner <- out_text("indented")
    node <- out_indent(inner, n = 5L)
    lines <- format_plain(node)
    expect_equal(lines, "     indented")
})
