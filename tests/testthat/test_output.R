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

# --- structural primitives ---------------------------------------------------

test_that("out_h1 renders centered title with rules", {
    node <- out_h1("Test Title", width = 20L)
    lines <- format_plain(node)
    expect_equal(length(lines), 3)
    expect_equal(lines[1], strrep("=", 20))
    expect_match(lines[2], "Test Title")
    expect_match(lines[2], "^\\s+Test Title$") # centered with left padding
    expect_equal(lines[3], strrep("-", 20))
})

test_that("out_h2 renders text with underline", {
    node <- out_h2("Section")
    lines <- format_plain(node)
    expect_equal(lines[1], "Section")
    expect_equal(lines[2], strrep("-", nchar("Section")))
})

test_that("out_rule renders horizontal rule", {
    node <- out_rule("=", 50L)
    expect_equal(format_plain(node), strrep("=", 50))

    node2 <- out_rule("-", 30L)
    expect_equal(format_plain(node2), strrep("-", 30))
})

test_that("out_group composes multiple nodes", {
    node <- out_group(
        out_text("line1"),
        out_blank(),
        out_text("line2")
    )
    lines <- format_plain(node)
    expect_equal(lines, c("line1", "", "line2"))
})

test_that("out_group filters NULL entries", {
    node <- out_group(
        out_text("a"),
        NULL,
        out_text("b")
    )
    lines <- format_plain(node)
    expect_equal(lines, c("a", "b"))
})

test_that("out_doc composes nodes with width", {
    node <- out_doc(
        out_h1("Title", width = 40L),
        out_blank(),
        out_text("content"),
        width = 40L
    )
    lines <- format_plain(node)
    expect_equal(lines[1], strrep("=", 40))
    expect_match(lines[2], "Title")
    expect_equal(lines[4], "")
    expect_equal(lines[5], "content")
})

test_that("out_doc handles character nodes for backward compat", {
    node <- out_doc(
        out_text("structured"),
        c("legacy", "lines"),
        width = 80L
    )
    lines <- format_plain(node)
    expect_equal(lines, c("structured", "legacy", "lines"))
})

test_that("out_kv formats key-value pairs", {
    node <- out_kv("Name" = "Alice", "Age" = "30")
    lines <- format_plain(node)
    expect_equal(length(lines), 2)
    expect_match(lines[1], "Name:")
    expect_match(lines[1], "Alice")
    expect_match(lines[2], "Age:")
    expect_match(lines[2], "30")
})

test_that("out_kv right-justifies keys", {
    node <- out_kv("Short" = "a", "Much Longer Key" = "b")
    lines <- format_plain(node)
    # Both keys should have same padding (right-justified)
    key_width1 <- regexpr("[^ ]", lines[1]) - 1 # leading spaces
    key_width2 <- regexpr("[^ ]", lines[2]) - 1
    # The shorter key should have more leading whitespace
    expect_true(key_width1 > key_width2)
})

test_that("out_test stores raw values", {
    node <- out_test(
        name = "Welch Two Sample t-test",
        statistic = c(t = 2.345),
        parameter = c(df = 98.2),
        p_value = 0.0211,
        null_hyp = "true difference in means is equal to 0",
        alt_hyp = "true difference in means is not equal to 0"
    )
    expect_s3_class(node, "out_test")
    expect_equal(node$p_value, 0.0211)
    expect_equal(node$statistic, c(t = 2.345))
})

test_that("out_test renders hypothesis block layout", {
    node <- out_test(
        name = "Welch Two Sample t-test",
        statistic = c(t = 2.345),
        parameter = c(df = 98.2),
        p_value = 0.0211,
        null_hyp = "true difference in means is equal to 0",
        alt_hyp = "true difference in means is not equal to 0"
    )
    lines <- format_plain(node)
    expect_equal(lines[1], "Welch Two Sample t-test")
    expect_equal(lines[2], "")
    expect_match(lines[3], "t = ")
    expect_match(lines[3], "df = ")
    expect_match(lines[3], "p-value")
    expect_equal(lines[4], "")
    expect_match(lines[5], "Null Hypothesis:")
    expect_match(lines[6], "Alternative Hypothesis:")
})

test_that("out_table_tri renders lower triangular matrix", {
    mat <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), nrow = 3)
    node <- out_table_tri(mat, names = c("A", "B", "C"), digits = 2L)
    expect_s3_class(node, "out_table_tri")
    lines <- format_plain(node)
    # Header row + 2 data rows (B, C)
    expect_equal(length(lines), 3)
    # First line is header row
    expect_match(lines[1], "A")
    expect_match(lines[1], "B")
    # B row has only one value (B-A)
    expect_match(lines[2], "B")
    # C row has two values
    expect_match(lines[3], "C")
})

test_that("out_table_tri matches formatTriMat output", {
    mat <- matrix(c(0, 0.5, 0.3, 0.5, 0, 0.8, 0.3, 0.8, 0), nrow = 3)
    nms <- c("A", "B", "C")

    old_result <- formatTriMat(mat, nms, digits = 3)
    old_lines <- apply(old_result, 1, function(row) {
        paste0("   ", paste(row, collapse = "   "))
    })

    node <- out_table_tri(mat, names = nms, digits = 3L)
    new_lines <- format_plain(node)

    expect_equal(new_lines, old_lines)
})

test_that("out_table_pairwise renders with header separator", {
    mat <- matrix(
        c("A - B", "A - C", "0.5", "0.3", "-0.1", "-0.2", "1.1", "0.8"),
        nrow = 2
    )
    node <- out_table_pairwise(mat, digits = 3L)
    lines <- format_plain(node)
    # First line is header, second is separator
    expect_match(lines[2], "^\\s*-+$")
})

test_that("out_table_pairwise adds group breaks", {
    # 3 levels = 3 comparisons: A-B, A-C, B-C
    mat <- rbind(
        c("Comparison", "Diff", "Lower", "Upper"),
        c("A - B", "0.5", "-0.1", "1.1"),
        c("A - C", "0.3", "-0.2", "0.8"),
        c("B - C", "0.1", "-0.4", "0.6")
    )
    node <- out_table_pairwise(mat, levels = c("A", "B", "C"), digits = 3L)
    lines <- format_plain(node)
    # Check that some lines end with \n (group break)
    has_break <- any(grepl("\n$", lines))
    expect_true(has_break)
})
