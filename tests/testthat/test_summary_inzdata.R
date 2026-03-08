test_that("summary.inzdata handles single categorical column correctly", {
    s <- iNZightPlots:::summary.inzdata(iris, NULL, width = 80)
    out <- format(s, format = "plain")

    cat_header <- grep("Categorical variables:", out)
    expect_length(cat_header, 1L)

    # Find the table block after the header (header, underline, blank, then table)
    cat_lines <- out[(cat_header + 3):length(out)]
    end <- which(cat_lines == "" | grepl("^=+$", cat_lines))[1] - 1L
    table_lines <- cat_lines[seq_len(end)]
    table_lines <- table_lines[nzchar(trimws(table_lines))]

    # col_headers row + 1 data row = 2 lines
    expect_equal(length(table_lines), 2L,
        label = "categorical table should have header + 1 data row for iris (Species)")
    expect_match(table_lines[2], "Species", fixed = TRUE)
})

test_that("summary.inzdata handles single numeric column correctly", {
    df <- data.frame(
        x = 1:10,
        a = factor(sample(c("A", "B"), 10, replace = TRUE)),
        b = factor(sample(c("X", "Y", "Z"), 10, replace = TRUE))
    )
    s <- iNZightPlots:::summary.inzdata(df, NULL, width = 80)
    out <- format(s, format = "plain")

    num_header <- grep("Numeric variables:", out)
    expect_length(num_header, 1L)

    num_lines <- out[(num_header + 3):length(out)]
    end <- which(num_lines == "" | grepl("^=+$", num_lines))[1] - 1L
    table_lines <- num_lines[seq_len(end)]
    table_lines <- table_lines[nzchar(trimws(table_lines))]

    # col_headers row + 1 data row = 2 lines
    expect_equal(length(table_lines), 2L,
        label = "numeric table should have header + 1 data row for single numeric column")
    expect_match(table_lines[2], "x", fixed = TRUE)
})

test_that("summary.inzdata works with multiple columns of each type", {
    s <- iNZightPlots:::summary.inzdata(
        data.frame(
            x = 1:5, y = rnorm(5),
            a = factor(c("A", "B", "A", "B", "A")),
            b = factor(c("X", "Y", "X", "Y", "X"))
        ),
        NULL,
        width = 80
    )
    out <- format(s, format = "plain")

    num_header <- grep("Numeric variables:", out)
    num_lines <- out[(num_header + 3):length(out)]
    end <- which(num_lines == "" | grepl("^=+$", num_lines))[1] - 1L
    num_table <- num_lines[seq_len(end)]
    num_table <- num_table[nzchar(trimws(num_table))]
    # col_headers + 2 data rows = 3 lines
    expect_equal(length(num_table), 3L, label = "header + 2 numeric rows")

    cat_header <- grep("Categorical variables:", out)
    cat_lines <- out[(cat_header + 3):length(out)]
    end <- which(cat_lines == "" | grepl("^=+$", cat_lines))[1] - 1L
    cat_table <- cat_lines[seq_len(end)]
    cat_table <- cat_table[nzchar(trimws(cat_table))]
    # col_headers + 2 data rows = 3 lines
    expect_equal(length(cat_table), 3L, label = "header + 2 categorical rows")
})
