# Output System Refactoring Plan

> This document describes a comprehensive refactoring of the summary/inference output system in iNZightPlots, replacing hand-built character vectors with a composable `out_*()` API that supports multiple output formats (plain text, HTML).

## Table of Contents

1. [Motivation](#motivation)
2. [Current Architecture](#current-architecture)
3. [Target Architecture](#target-architecture)
4. [API Reference](#api-reference)
5. [Internal Data Structures](#internal-data-structures)
6. [Format Dispatching](#format-dispatching)
7. [Implementation Steps](#implementation-steps)
   - [Step 1: Core Node System](#step-1-core-node-system)
   - [Step 2: out_table](#step-2-out_table)
   - [Step 3: Text Primitives](#step-3-text-primitives)
   - [Step 4: Structural Primitives](#step-4-structural-primitives)
   - [Step 5: Domain Helpers](#step-5-domain-helpers)
   - [Step 6: Bridge Utilities](#step-6-bridge-utilities)
   - [Step 7: Snapshot Fixtures](#step-7-snapshot-fixtures)
   - [Step 8: Migrate summary.inzbar (one-way)](#step-8-migrate-summaryinzbar-one-way)
   - [Step 9: Migrate summary.inzscatter](#step-9-migrate-summaryinzscatter)
   - [Step 10: Migrate summary.inzdot](#step-10-migrate-summaryinzdot)
   - [Step 11: Migrate summary.inzbar (two-way)](#step-11-migrate-summaryinzbar-two-way)
   - [Step 12: Migrate inference.inzscatter](#step-12-migrate-inferenceinzscatter)
   - [Step 13: Migrate inference.inzdot](#step-13-migrate-inferenceinzdot)
   - [Step 14: Migrate inference.inzbar (one-way)](#step-14-migrate-inferenceinzbar-one-way)
   - [Step 15: Migrate inference.inzbar (two-way)](#step-15-migrate-inferenceinzbar-two-way)
   - [Step 16: Migrate epi.format](#step-16-migrate-epiformat)
   - [Step 17: Migrate summary.inzdata](#step-17-migrate-summaryinzdata)
   - [Step 18: Migrate the Orchestrator](#step-18-migrate-the-orchestrator)
   - [Step 19: Cleanup](#step-19-cleanup)
   - [Step 20: HTML Rendering](#step-20-html-rendering)
8. [Testing Strategy](#testing-strategy)
9. [Risks and Mitigations](#risks-and-mitigations)
10. [File Map](#file-map)

---

## Motivation

The current summary/inference output is built by concatenating strings into character vectors, line by line. The same matrix-formatting boilerplate appears 20+ times:

```r
mat <- format(mat, digits = opts$signif)
mat[grep("NA", mat)] <- ""
mat <- rbind(c("Header1", "Header2"), mat)
mat <- cbind(c("", rownames), mat)
mat <- matrix(apply(mat, 2, function(col) format(col, justify = "right")), nrow = nrow(mat))
mat <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
out <- c(out, "Section Title:", "", mat, "")
```

This duplication makes it:

- **Hard to maintain**: fixing a formatting bug means editing 20+ places
- **Hard to extend**: producing HTML instead of plain text requires rewriting everything
- **Inconsistent**: each copy has slight variations in spacing, headers, etc.

---

## Current Architecture

### Key Files

| File                | Lines | Purpose                                                                                                                |
| ------------------- | ----- | ---------------------------------------------------------------------------------------------------------------------- |
| `R/summary.R`       | 941   | S3 summary methods: `summary.inzdot`, `summary.inzbar`, `summary.inzscatter`                                           |
| `R/inference.R`     | 1707  | S3 inference methods: `inference.inzdot`, `inference.inzbar`, `inference.inzscatter` + `formatTriMat()`, `formatMat()` |
| `R/getSummary.R`    | 856   | Orchestrator `summary.inzplotoutput`, `summary.inzdata`, `print.inzight.plotsummary`, `centerText()`                   |
| `R/inference_epi.R` | 162   | `epi.format()` for OR/RR/RD tables                                                                                     |
| `R/general.R`       | 564   | `format_pval()`, `centerText()`                                                                                        |

### S3 Method Inventory

**Summary methods** (NAMESPACE exports):

- `summary.inzdot` - numeric variable summaries (min, Q1, median, Q3, max, mean, SD, n)
- `summary.inzhist` - delegates to `summary.inzdot`
- `summary.inzbar` - categorical contingency tables (counts + percentages)
- `summary.inzscatter` - trend equations and correlations
- `summary.inzgrid` - delegates to `summary.inzscatter`
- `summary.inzhex` - delegates to `summary.inzscatter`

**Inference methods** (NAMESPACE exports):

- `inference.inzdot` - CIs for means, t-tests, ANOVA, pairwise comparisons
- `inference.inzhist` - delegates to `inference.inzdot`
- `inference.inzbar` - CIs for proportions, chi-square, pairwise proportion diffs, epi calcs
- `inference.inzscatter` - trend coefficient CIs and p-values
- `inference.inzgrid` - delegates to `inference.inzscatter`
- `inference.inzhex` - delegates to `inference.inzscatter`

### How Output Is Built

All methods build a character vector `out`:

```r
# In the summary/inference methods:
out <- c(out, "Section Header:", "", formatted_lines, "")

# In the orchestrator (getSummary.R):
out <- character()
add <- function(..., underline = FALSE) {
    x <- paste0(..., collapse = "")
    out <<- c(out, x)
    if (underline) out <<- c(out, rule("-", width = nchar(x)))
}
# ... build everything via add() ...
class(out) <- "inzight.plotsummary"
```

The print method is trivial:

```r
print.inzight.plotsummary <- function(x, ...) cat(x, sep = "\n")
```

### Repeated Patterns

**Pattern A: Matrix table formatting** (~20 occurrences)

```r
mat <- format(mat, digits = opts$signif, scientific = FALSE)
mat[grep("NA", mat)] <- ""
mat <- gsub("NaN", "|", mat)
mat <- rbind(c("Col1", "Col2"), mat)
mat <- cbind(c("", rownames), mat)
mat <- matrix(apply(mat, 2, function(col) format(col, justify = "right")), nrow = nrow(mat))
mat <- apply(mat, 1, function(x) paste0("   ", paste(x, collapse = "   ")))
```

**Pattern B: Section header** (~15 occurrences)

```r
out <- c(out, "Section title:", "", content, "")
```

**Pattern C: Hypothesis test block** (~8 occurrences)

```r
out <- c(out, "",
    "Test Name", "",
    paste0("   t = ", stat, ", df = ", df, ", p-value ", pval), "",
    paste0("          Null Hypothesis: ..."),
    paste0("   Alternative Hypothesis: ..."))
```

**Pattern D: Pairwise comparison table** (~5 occurrences)

```r
mat <- c(mat[1], paste(rep("-", nchar(mat[1])), collapse = ""), mat[-1])
# with line breaks between groups:
rl <- cumsum((length(LEVELS) - 1L):1L) + 2L
mat[rl] <- paste0(mat[rl], "\n")
```

**Pattern E: Horizontal rules**

```r
Hrule <- paste0(rep("=", width), collapse = "")
hrule <- paste0(rep("-", width), collapse = "")
```

**Pattern F: Indentation**

```r
ind <- function(x, indent = 3) paste0(paste0(rep(" ", indent), collapse = ""), x)
```

### Existing Helpers

- `formatTriMat(mat, names, digits)` in `R/inference.R:599-636` - formats lower triangular matrix
- `formatMat(mat, digits)` in `R/inference.R:638-678` - formats labeled matrix
- `format_pval(p, opts, digits)` in `R/general.R:561-563` - formats p-values
- `centerText(x, width)` in `R/getSummary.R:846-850` - centers text within width
- `epi.format(mat, label, names, null_value)` in `R/inference_epi.R` - formats OR/RR/RD tables

---

## Target Architecture

### New Files

| File                                        | Purpose                                          | Dependencies                |
| ------------------------------------------- | ------------------------------------------------ | --------------------------- |
| `R/output.R` (~300 lines)                   | Output primitives + `format_plain.*` methods     | base R only                 |
| `R/output_helpers.R` (~150 lines)           | Domain-specific convenience wrappers             | `R/output.R`, `R/general.R` |
| `tests/testthat/test_output.R` (~200 lines) | Unit tests for primitives                        |                             |
| `tests/testthat/test_output_regression.R`   | Snapshot regression tests                        |                             |
| `tests/testthat/fixtures/`                  | Directory of saved reference output `.rds` files |                             |

### Design Principles

1. **Each `out_*()` returns a structured S3 object**, not a string
2. **Raw data in, formatting deferred**: nodes store raw R objects (numeric matrices, data frames, test results) and formatting parameters (digits, etc.). The `format_plain.*` methods handle number formatting, alignment, and layout. This means the same node can produce different representations (plain text, HTML via {gt}/{knitr}) without the caller worrying about formatting.
3. **Privacy suppression operates on raw data**: privacy controls modify the raw numeric matrix/values BEFORE they are passed to `out_table()`. The node stores the (possibly suppressed) raw values, not formatted strings.
4. **Backward compatible**: during migration, a `flatten_node()` bridge converts structured nodes back to character vectors so the orchestrator's `sapply(..., add)` pattern continues to work
5. **Incremental**: each method can be migrated independently; old and new coexist
6. **Testable at two levels**: (a) node construction can be tested by inspecting the structured data (e.g., checking `node$mat` values, `node$digits`), independent of rendering; (b) rendering can be tested by checking the character/HTML output of `format()`.
7. **Styling attributes on text nodes**: text nodes accept optional formatting hints (bold, italic, colour, CSS class) that are ignored in plain text but rendered in HTML. This enables richer output without breaking backward compatibility.
8. **HTML via established packages**: HTML rendering delegates to {gt} for tables and standard HTML tags + optional custom CSS for other elements, rather than hand-writing HTML.

---

## API Reference

### Core

```r
# Internal node constructor
out_node(type, ...)
# Returns: list with class c("out_<type>", "out_node")

# Document container
out_doc(..., width = 100L)
# Args: ... = out_node objects or character vectors (backward compat)
# Returns: list(nodes = list(...), width = width) with class c("out_doc", "out_node")

# Group nodes without chrome
out_group(...)
# Args: ... = out_node objects
# Returns: list(nodes = list(...)) with class c("out_group", "out_node")
```

### Headings and Rules

```r
# Top-level heading: double rule + centered title + single rule
out_h1(text, width = 100L)
# Plain: "====...====\n     text     \n----...----"

# Section heading with underline
out_h2(text)
# Plain: "text\n----" (dashes match text width)

# Horizontal rule
out_rule(char = "=", width = 100L)
# Plain: "====...====" (width characters)
```

### Text

```r
# One or more text lines
out_text(..., .bold = FALSE, .italic = FALSE, .colour = NULL, .css_class = NULL)
# Args: ... = character pieces, pasted with collapse = ""
# Styling args are ignored by format_plain (output is identical to current)
# but used by format_html to wrap in <strong>, <em>, <span style="...">, etc.
# Plain: single line (styling ignored)
# HTML: <p class="{.css_class}" style="color:{.colour}"><strong>text</strong></p>

# Blank line(s)
out_blank(n = 1L)
# Plain: n empty strings

# Bulleted list
out_bullet(items, indent = 2L, bullet = "* ")
# Plain: "  * item1\n  * item2\n..."

# Indentation wrapper
out_indent(node, n = 3L)
# Wraps another node, prepending n spaces to each rendered line (plain text only)
```

Note on styling: the `.bold`, `.italic`, `.colour`, and `.css_class` arguments use dot-prefixed names to avoid collisions with text content passed via `...`. These are stored on the node and used only by HTML (and potentially other rich-text) renderers. The plain text renderer ignores them entirely, ensuring backward compatibility.

### Tables

```r
# Formatted data table - THE main workhorse
# Stores the RAW data matrix (numeric/character) and formatting parameters.
# Number formatting is done by the format_plain/format_html methods, not the caller.
out_table(
    mat,                    # matrix or data.frame - raw values (numeric or character)
    col_headers = NULL,     # character vector of column headers
    row_headers = NULL,     # character vector of row headers
    digits = 4L,            # significant digits for numeric columns
    scientific = FALSE,     # use scientific notation?
    na_replace = "",        # replacement string for NA values
    nan_replace = "|",      # replacement string for NaN values (NULL to keep)
    indent = 3L,            # leading spaces (plain text only)
    col_sep = "   ",        # column separator string (plain text only)
    justify = "right",      # "right" or "left", or per-column vector (plain text only)
    separator_after = NULL, # integer vector: row indices after which to insert "---" line
    caption = NULL          # optional table caption (used in HTML rendering)
)
# The node stores the raw matrix + formatting params.
# format_plain: formats numbers, aligns, indents -> character vector
# format_html: delegates to gt::gt() or knitr::kable() -> HTML string

# Lower-triangular matrix (e.g., correlation/distance matrices)
out_table_tri(mat, names, digits = 3L)
# mat: square numeric matrix; only the lower triangle is rendered
# names: character vector of row/col names
# Plain: same as current formatTriMat() output

# Pairwise comparison table (with header separator and group spacing)
out_table_pairwise(
    comparisons,    # data.frame with columns: label, estimate, lower, upper, p_value (optional)
    levels = NULL,  # character vector of factor levels (for group break spacing)
    digits = 4L,    # significant digits for numeric columns
    indent = 1L
)
# Plain: header row, "---" separator, data rows with "\n" breaks between groups
```

Note: callers pass **raw numeric matrices** to `out_table()`. Privacy suppression (which replaces certain cells with suppression symbols) is applied to the raw matrix BEFORE passing it in - this is the one exception where cells may already be character. The node's `format_plain` method handles this gracefully by only formatting cells that are still numeric.

### Key-Value Pairs

```r
out_kv(..., indent = 3L)
# Args: named arguments or a named list
# Plain: right-justified keys followed by values, like the header metadata
```

### Hypothesis Tests

```r
out_test(
    name,           # character: test name (e.g., "Welch Two Sample t-test")
    statistic,      # named numeric: test statistic(s) (e.g., c(t = 2.5))
    parameter,      # named numeric: parameter(s) (e.g., c(df = 98.2))
    p_value,        # numeric: p-value
    null_hyp,       # character: null hypothesis text
    alt_hyp,        # character: alternative hypothesis text
    extras = NULL,  # character vector: additional lines (e.g., pooled variance)
    opts = NULL     # list: formatting options (min_pval, signif digits)
)
# Stores raw numeric values; format_plain renders the traditional layout:
# "Test Name"
# ""
# "   t = 2.5, df = 98, p-value = 0.014"
# ""
# "          Null Hypothesis: ..."
# "   Alternative Hypothesis: ..."
#
# format_html can render a structured card/panel with formatted numbers.
```

Note: storing raw statistic/p-value/df values means we can (a) test that the correct values were computed without parsing formatted strings, and (b) render them differently in HTML (e.g., coloured p-values, formatted with MathJax).

### Formatting/Rendering

```r
# Primary dispatch
format.out_node(x, format = c("plain", "html"), width = 100L, ...)

# Internal generics (not exported)
format_plain(x, ...)  # S3 generic
format_html(x, ...)   # S3 generic (stubs initially)

# Bridge utility: convert out_node to character vector
flatten_node(x, width = 100L)
# Equivalent to format_plain(x, width = width)
# Used during incremental migration so old code can consume new output
```

---

## Internal Data Structures

### Example: `out_table` node

```r
list(
    mat = matrix(c(1.23, 4.56, 78.90, 123.00), nrow = 2),  # RAW numeric
    col_headers = c("Mean", "SD"),
    row_headers = c("Group A", "Group B"),
    digits = 4L,
    scientific = FALSE,
    na_replace = "",
    nan_replace = "|",
    indent = 3L,
    col_sep = "   ",
    justify = "right",
    separator_after = integer(0),
    caption = NULL
)
# class: c("out_table", "out_node")
#
# Testing: can inspect node$mat directly to verify computed values,
# without needing to parse formatted output strings.
```

### Example: `out_test` node

```r
list(
    name = "Welch Two Sample t-test",
    statistic = c(t = 2.345),
    parameter = c(df = 98.2),
    p_value = 0.0211,
    null_hyp = "true difference in means is equal to 0",
    alt_hyp = "true difference in means is not equal to 0",
    extras = NULL,
    opts = list(min_pval = 2.2e-16, signif = 5L)
)
# class: c("out_test", "out_node")
#
# Testing: can check node$p_value == 0.0211 directly,
# rather than parsing "p-value = 0.0211" from a string.
```

### Example: `out_doc` node

```r
list(
    nodes = list(
        out_h1("iNZight Summary", width = 100L),
        out_kv("Primary variable" = "height (numeric)"),
        out_rule("=", 100L),
        out_blank(),
        out_h2("Summary of height:"),
        out_blank(),
        out_table(mat, col_headers = c("Min", "Q1", "Median", ...)),
        out_blank(),
        out_rule("=", 100L)
    ),
    width = 100L
)
# class: c("out_doc", "out_node")
```

---

## Format Dispatching

### Plain Text Rendering

Each node type gets a `format_plain.<type>` method. Key implementations:

#### `format_plain.out_table`

```r
format_plain.out_table <- function(x, ...) {
    mat <- x$mat

    # Format numeric values to character; leave character values as-is
    # (character cells arise from privacy suppression replacing values with symbols)
    mat <- matrix(
        apply(mat, 2L, function(col) {
            if (is.numeric(col)) {
                formatted <- format(col, digits = x$digits, scientific = x$scientific)
            } else {
                formatted <- as.character(col)
            }
            # Replace NA/NaN
            formatted[grep("NA", formatted)] <- x$na_replace
            if (!is.null(x$nan_replace)) {
                formatted <- gsub("NaN", x$nan_replace, formatted)
            }
            formatted
        }),
        nrow = nrow(mat)
    )

    # Prepend column headers as first row
    if (!is.null(x$col_headers)) {
        mat <- rbind(x$col_headers, mat)
    }

    # Prepend row headers as first column
    if (!is.null(x$row_headers)) {
        corner <- if (!is.null(x$col_headers)) "" else character(0)
        rh <- c(corner, x$row_headers)
        mat <- cbind(rh, mat)
    }

    # Apply column justification
    justify <- x$justify
    if (length(justify) == 1L) justify <- rep(justify, ncol(mat))

    mat <- matrix(
        mapply(
            function(col, j) format(col, justify = j),
            as.data.frame(mat, stringsAsFactors = FALSE),
            justify,
            SIMPLIFY = TRUE
        ),
        nrow = nrow(mat)
    )

    # Render rows with indent and separator
    lines <- apply(mat, 1L, function(row) {
        paste0(strrep(" ", x$indent), paste(row, collapse = x$col_sep))
    })

    # Insert separator lines after specified rows
    if (length(x$separator_after) > 0L) {
        total_width <- nchar(lines[1])
        sep_line <- paste0(
            strrep(" ", x$indent),
            strrep("-", total_width - x$indent)
        )
        offset <- 0L
        for (idx in sort(x$separator_after)) {
            pos <- idx + offset + if (!is.null(x$col_headers)) 1L else 0L
            lines <- append(lines, sep_line, after = pos)
            offset <- offset + 1L
        }
    }

    lines
}
```

#### `format_plain.out_test`

```r
format_plain.out_test <- function(x, ...) {
    # Format the statistic line from raw values
    stat_parts <- paste0(
        names(x$statistic), " = ",
        format(x$statistic, digits = 5)
    )
    param_parts <- paste0(
        names(x$parameter), " = ",
        format(x$parameter, digits = 5)
    )
    pval <- format_pval(x$p_value, x$opts)
    pval_prefix <- ifelse(substr(pval, 1, 1) == "<", "", "= ")

    stat_line <- paste0(
        "   ",
        paste(c(stat_parts, param_parts), collapse = ", "),
        ", p-value ", pval_prefix, pval
    )

    c(
        x$name,
        "",
        stat_line,
        "",
        paste0("          Null Hypothesis: ", x$null_hyp),
        paste0("   Alternative Hypothesis: ", x$alt_hyp),
        if (!is.null(x$extras)) c("", x$extras)
    )
}
```

#### `format_plain.out_h1`

```r
format_plain.out_h1 <- function(x, ...) {
    c(
        strrep("=", x$width),
        centerText(x$text, x$width),
        strrep("-", x$width)
    )
}
```

#### `format_plain.out_h2`

```r
format_plain.out_h2 <- function(x, ...) {
    c(x$text, strrep("-", nchar(x$text)))
}
```

#### `format_plain.out_doc` / `format_plain.out_group`

```r
format_plain.out_doc <- function(x, ...) {
    unlist(lapply(x$nodes, function(node) {
        if (is.character(node)) return(node)
        format_plain(node, width = x$width)
    }))
}

format_plain.out_group <- function(x, ...) {
    unlist(lapply(x$nodes, function(node) {
        if (is.character(node)) return(node)
        format_plain(node, ...)
    }))
}
```

#### `format_plain.out_blank`

```r
format_plain.out_blank <- function(x, ...) {
    rep("", x$n)
}
```

#### `format_plain.out_text`

```r
format_plain.out_text <- function(x, ...) {
    # Styling attributes (.bold, .italic, .colour, .css_class) are ignored
    # in plain text output - only used by format_html
    x$text
}
```

#### `format_plain.out_rule`

```r
format_plain.out_rule <- function(x, ...) {
    strrep(x$char, x$width)
}
```

#### `format_plain.out_bullet`

```r
format_plain.out_bullet <- function(x, ...) {
    paste0(strrep(" ", x$indent), x$bullet, x$items)
}
```

#### `format_plain.out_kv`

```r
format_plain.out_kv <- function(x, ...) {
    keys <- format(paste0(names(x$pairs), ": "), justify = "right")
    paste0(strrep(" ", x$indent), keys, x$pairs)
}
```

#### `format_plain.out_indent`

```r
format_plain.out_indent <- function(x, ...) {
    lines <- format_plain(x$node, ...)
    paste0(strrep(" ", x$n), lines)
}
```

#### `format_plain.out_table_pairwise`

```r
format_plain.out_table_pairwise <- function(x, ...) {
    mat <- x$mat  # already a character matrix with label, estimate, lower, upper, [p_value]
    header <- mat[1, ]
    body <- mat[-1, , drop = FALSE]

    mat_fmt <- matrix(
        apply(mat, 2, function(col) format(col, justify = "right")),
        nrow = nrow(mat)
    )

    lines <- apply(mat_fmt, 1, function(row) paste(row, collapse = "   "))
    sep_line <- strrep("-", nchar(lines[1]))

    # Insert separator after header
    lines <- c(lines[1], sep_line, lines[-1])

    # Insert group breaks
    if (!is.null(x$levels) && length(x$levels) > 2L) {
        rl <- (length(x$levels) - 1L):1L
        rl <- cumsum(rl) + 2L  # +2 for header + separator
        for (i in rev(rl)) {
            if (i <= length(lines)) {
                lines[i] <- paste0(lines[i], "\n")
            }
        }
    }

    paste0(strrep(" ", x$indent), lines)
}
```

### HTML Rendering (future - Step 20)

Each type gets a `format_html.<type>` method. Because nodes store **raw data**, HTML rendering can delegate to established packages rather than hand-writing HTML:

```r
format_html.out_table <- function(x, ...) {
    # Because we have the raw numeric matrix, we can use {gt} directly:
    df <- as.data.frame(x$mat)
    if (!is.null(x$col_headers)) colnames(df) <- x$col_headers
    if (!is.null(x$row_headers)) df <- cbind(` ` = x$row_headers, df)

    tbl <- gt::gt(df)
    if (!is.null(x$caption)) tbl <- gt::tab_header(tbl, title = x$caption)
    # gt handles formatting, alignment, NA display, etc.
    gt::as_raw_html(tbl)
}

format_html.out_h1 <- function(x, ...) {
    sprintf('<h1 class="inzight-title">%s</h1>', htmlEscape(x$text))
}

format_html.out_text <- function(x, ...) {
    text <- htmlEscape(x$text)
    if (isTRUE(x$.bold)) text <- paste0("<strong>", text, "</strong>")
    if (isTRUE(x$.italic)) text <- paste0("<em>", text, "</em>")

    style <- if (!is.null(x$.colour)) sprintf(' style="color:%s"', x$.colour) else ""
    class <- if (!is.null(x$.css_class)) sprintf(' class="%s"', x$.css_class) else ""
    sprintf("<p%s%s>%s</p>", class, style, text)
}

format_html.out_test <- function(x, ...) {
    # Structured card with raw values - can add colour to p-values, etc.
    pval_colour <- if (x$p_value < 0.05) "red" else "black"
    sprintf(
        '<div class="hypothesis-test">
         <h3>%s</h3>
         <p>%s = %s, %s = %s, p-value = <span style="color:%s">%s</span></p>
         <dl>
           <dt>Null Hypothesis</dt><dd>%s</dd>
           <dt>Alternative Hypothesis</dt><dd>%s</dd>
         </dl>
         </div>',
        htmlEscape(x$name),
        names(x$statistic), format(x$statistic, digits = 5),
        names(x$parameter), format(x$parameter, digits = 5),
        pval_colour, format_pval(x$p_value, x$opts),
        htmlEscape(x$null_hyp), htmlEscape(x$alt_hyp)
    )
}
```

**Custom CSS**: the `out_doc` container can accept an optional `css` argument (character string of CSS rules) which gets embedded in a `<style>` tag when rendering to HTML. This allows per-document styling without modifying the rendering code.

```r
out_doc(..., width = 100L, css = NULL)
# css: optional character string of CSS, e.g., ".inzight-title { color: navy; }"
```

---

## Implementation Steps

Each step is self-contained: implement, test, verify `make test` passes, then proceed.

---

### Step 1: Core Node System

**File**: `R/output.R`
**What**: Create the foundational `out_node()` constructor, `format.out_node()` dispatcher, and `format_plain()` generic.

```r
# Internal constructor
out_node <- function(type, ...) {
    node <- list(...)
    class(node) <- c(paste0("out_", type), "out_node")
    node
}

# Public format method
#' @export
format.out_node <- function(x, format = c("plain", "html"), width = 100L, ...) {
    format <- match.arg(format)
    switch(format,
        plain = format_plain(x, width = width),
        html = format_html(x, width = width)
    )
}

# Internal S3 generics
format_plain <- function(x, ...) UseMethod("format_plain")
format_html <- function(x, ...) {
    # Stub: fall back to plain text wrapped in <pre>
    paste0("<pre>", paste(format_plain(x, ...), collapse = "\n"), "</pre>")
}
```

**Test** (`tests/testthat/test_output.R`):

```r
test_that("out_node creates object with correct class", {
    node <- out_node("test", value = 42)
    expect_s3_class(node, "out_test")
    expect_s3_class(node, "out_node")
    expect_equal(node$value, 42)
})
```

**Verify**: `make test` passes (new code only, no changes to existing).

---

### Step 2: `out_table`

**File**: `R/output.R`
**What**: The most important primitive. Implement `out_table()` and `format_plain.out_table()`.

This replaces the ~20 occurrences of the matrix formatting boilerplate (Pattern A from Current Architecture section).

```r
out_table <- function(mat,
                      col_headers = NULL,
                      row_headers = NULL,
                      digits = 4L,
                      scientific = FALSE,
                      na_replace = "",
                      nan_replace = "|",
                      indent = 3L,
                      col_sep = "   ",
                      justify = "right",
                      separator_after = NULL,
                      caption = NULL) {
    stopifnot(is.matrix(mat) || is.data.frame(mat))
    mat <- as.matrix(mat)
    out_node("table",
        mat = mat,
        col_headers = col_headers,
        row_headers = row_headers,
        digits = as.integer(digits),
        scientific = scientific,
        na_replace = na_replace,
        nan_replace = nan_replace,
        indent = as.integer(indent),
        col_sep = col_sep,
        justify = justify,
        separator_after = as.integer(separator_after %||% integer(0)),
        caption = caption
    )
}
```

Implement `format_plain.out_table` as described in [Format Dispatching](#format-plain-out_table).

**Tests**:

```r
test_that("out_table stores raw numeric matrix", {
    m <- matrix(c(1, 22, 333, 4444), nrow = 2)
    node <- out_table(m, digits = 0L)
    expect_equal(node$mat, m)
    expect_equal(node$digits, 0L)
    lines <- format_plain(node)
    # Should be right-justified with 3-space indent
    expect_match(lines[1], "^   ")
    expect_equal(length(lines), 2)
})

test_that("out_table adds col_headers and row_headers", {
    m <- matrix(c(1.23, 4.56), nrow = 1)
    node <- out_table(m, col_headers = c("A", "B"), row_headers = c("Row1"),
                      digits = 2L)
    expect_equal(node$col_headers, c("A", "B"))
    expect_equal(node$row_headers, "Row1")
    lines <- format_plain(node)
    expect_equal(length(lines), 2)  # header + 1 data row
    expect_match(lines[1], "A")
    expect_match(lines[2], "Row1")
})

test_that("out_table handles separator_after", {
    m <- matrix(1:6, nrow = 3)
    node <- out_table(m, col_headers = c("X", "Y"), separator_after = 2L,
                      digits = 0L)
    lines <- format_plain(node)
    expect_match(lines[4], "^\\s+-+$")  # separator after 2nd data row (3rd line)
})

test_that("out_table reproduces summary.inzbar one-way output pattern", {
    # Exact reproduction test using known values
    tab <- c(bus = 73, motor = 17, other = 7, train = 3)
    total <- sum(tab)
    perc <- tab / total * 100

    # Build a mixed matrix: counts in row 1, percentages in row 2
    count_row <- c(tab, total)
    pct_row <- c(perc, 100)
    mat_vals <- rbind(count_row, pct_row)

    node <- out_table(
        mat_vals,
        col_headers = c(names(tab), "Total"),
        row_headers = c("Count", "Percent"),
        digits = 0L
    )
    # Verify raw data is stored
    expect_equal(node$mat[1, 1], 73)
    expect_equal(node$mat[2, 5], 100)
    lines <- format_plain(node)
    # Verify structure: 3 lines (header, count, percent)
    expect_equal(length(lines), 3)
})
```

**Verify**: `make test` passes.

---

### Step 3: Text Primitives

**File**: `R/output.R`
**What**: Implement `out_text()`, `out_blank()`, `out_bullet()`, `out_indent()`.

```r
out_text <- function(..., .bold = FALSE, .italic = FALSE,
                     .colour = NULL, .css_class = NULL) {
    text <- paste0(..., collapse = "")
    out_node("text", text = text,
        .bold = .bold, .italic = .italic,
        .colour = .colour, .css_class = .css_class)
}

out_blank <- function(n = 1L) {
    out_node("blank", n = as.integer(n))
}

out_bullet <- function(items, indent = 2L, bullet = "* ") {
    out_node("bullet", items = items, indent = as.integer(indent), bullet = bullet)
}

out_indent <- function(node, n = 3L) {
    out_node("indent", node = node, n = as.integer(n))
}
```

Plus their `format_plain.*` methods (see [Format Dispatching](#format-dispatching)).

**Tests**: Test each returns correct character output.

**Verify**: `make test` passes.

---

### Step 4: Structural Primitives

**File**: `R/output.R`
**What**: Implement `out_h1()`, `out_h2()`, `out_rule()`, `out_group()`, `out_doc()`, `out_kv()`, `out_test()`, `out_table_tri()`, `out_table_pairwise()`.

#### `out_h1` and `out_h2`

```r
out_h1 <- function(text, width = 100L) {
    out_node("h1", text = text, width = as.integer(width))
}

out_h2 <- function(text) {
    out_node("h2", text = text)
}
```

#### `out_rule`

```r
out_rule <- function(char = "=", width = 100L) {
    stopifnot(nchar(char) == 1L)
    out_node("rule", char = char, width = as.integer(width))
}
```

#### `out_group` and `out_doc`

```r
out_group <- function(...) {
    nodes <- list(...)
    # Flatten any NULL entries
    nodes <- nodes[!vapply(nodes, is.null, logical(1))]
    out_node("group", nodes = nodes)
}

out_doc <- function(..., width = 100L) {
    nodes <- list(...)
    nodes <- nodes[!vapply(nodes, is.null, logical(1))]
    doc <- out_node("doc", nodes = nodes, width = as.integer(width))
    doc
}
```

#### `out_kv`

```r
out_kv <- function(..., indent = 3L) {
    args <- list(...)
    if (length(args) == 1L && is.list(args[[1]])) {
        pairs <- args[[1]]
    } else {
        pairs <- args
    }
    out_node("kv", pairs = pairs, indent = as.integer(indent))
}
```

#### `out_test`

```r
out_test <- function(name, statistic, parameter, p_value,
                     null_hyp, alt_hyp, extras = NULL, opts = NULL) {
    out_node("test",
        name = name,
        statistic = statistic,    # named numeric, e.g. c(t = 2.5)
        parameter = parameter,    # named numeric, e.g. c(df = 98.2)
        p_value = p_value,        # numeric scalar
        null_hyp = null_hyp,
        alt_hyp = alt_hyp,
        extras = extras,
        opts = opts               # list with min_pval, signif digits
    )
}
```

#### `out_table_tri`

Stores the raw symmetric matrix and names; formatting (extracting lower triangle, aligning) is deferred to `format_plain`/`format_html`:

```r
out_table_tri <- function(mat, names, digits = 3L) {
    stopifnot(is.matrix(mat), nrow(mat) == ncol(mat))
    out_node("table_tri",
        mat = mat,
        names = names,
        digits = as.integer(digits)
    )
}
```

The `format_plain.out_table_tri` method handles the lower-triangle extraction and alignment:

```r
format_plain.out_table_tri <- function(x, ...) {
    mat <- x$mat
    mat[!lower.tri(mat)] <- NA
    mat <- mat[-1, , drop = FALSE]
    mat <- format(mat, digits = x$digits)
    mat[grep("NA", mat)] <- ""
    mat[grep("NaN", mat)] <- ""

    full <- cbind(c("", x$names[-1]), rbind(x$names, mat))
    full <- full[, -ncol(full)]
    full <- matrix(
        apply(full, 2, function(col) format(col, justify = "right")),
        nrow = nrow(full)
    )

    apply(full, 1, function(row) paste0("   ", paste(row, collapse = "   ")))
}
```

**Tests**: Comprehensive tests for each, especially:

- `out_h1` matches the `"====\n  title  \n----"` pattern
- `out_test` matches the exact hypothesis block layout
- `out_table_tri` produces identical output to `formatTriMat()`
- `out_doc` composes multiple nodes correctly

**Verify**: `make test` passes.

---

### Step 5: Domain Helpers

**File**: `R/output_helpers.R` (NEW)
**What**: Convenience functions that encode iNZightPlots-specific formatting conventions.

#### `out_format_mat` (simplified role)

Since `out_table()` now accepts raw numeric matrices and handles formatting internally, this helper's role is reduced to applying privacy suppression and other pre-processing that must happen before the data reaches the node. It remains useful for cases where specific columns need different treatment (e.g., suppressing specific columns based on privacy rules):

```r
out_format_mat <- function(mat, privacy_controls = NULL, suppress_cols = NULL,
                           suppress_mat = NULL) {
    # Apply privacy suppression to specific columns if needed
    # This converts suppressed cells from numeric to character (suppression symbol)
    # which out_table's format_plain handles gracefully
    if (!is.null(suppress_mat) && !is.null(privacy_controls)) {
        for (col in suppress_cols) {
            mat[, col] <- privacy_controls$suppress(mat[, col], suppress_mat)
        }
    }
    mat
}
```

Note: most simple cases won't need this helper at all - they can pass the raw matrix directly to `out_table()`. This helper is only needed when privacy suppression must target specific columns.

#### `out_ci_section`

Builds the recurring "Statistic with XX% CI" pattern:

```r
out_ci_section <- function(estimates, lower, upper,
                           label = "Mean",
                           ci_width = 0.95,
                           bootstrap = FALSE,
                           by_factor = FALSE,
                           factor_names = NULL,
                           is_survey = FALSE,
                           plural = NULL,
                           digits = 4L) {
    if (is.null(plural)) plural <- by_factor

    # Store raw numeric values - formatting deferred to format_plain/format_html
    mat <- cbind(estimates, lower, upper)

    bsCI <- if (bootstrap) " Percentile Bootstrap" else ""
    header_text <- paste0(
        label,
        " with ", ci_width * 100, "%", bsCI,
        " Confidence Interval", if (plural) "s" else ""
    )

    out_group(
        out_text(header_text),
        out_blank(),
        out_table(mat,
            col_headers = c("Estimate", "Lower", "Upper"),
            row_headers = factor_names,
            digits = digits
        )
    )
}
```

#### `out_privacy_section`

Replaces the ~80-line privacy controls block in `getSummary.R:424-502`:

```r
out_privacy_section <- function(privacy_controls, width = 100L) {
    if (is.null(privacy_controls)) return(NULL)

    items <- character()

    if (privacy_controls$has("rounding")) {
        items <- c(items, sprintf("counts are rounded using %s",
            switch(privacy_controls$get("rounding"),
                "RR3" = "RR3 (random rounding to base 3)",
                paste0("other (", privacy_controls$get("rounding"), ")")
            )
        ))
    }

    if (privacy_controls$has("suppression")) {
        items <- c(items, sprintf(
            "suppression of counts smaller than %d, indicated by %s%s",
            privacy_controls$get("suppression"),
            privacy_controls$get("symbol"),
            if (privacy_controls$get("secondary_suppression"))
                ", with secondary suppression where necessary" else ""
        ))
    }

    if (privacy_controls$has("suppression_raw_counts")) {
        items <- c(items, sprintf(
            "suppression of weighted counts with corresponding unweighted counts < %s",
            privacy_controls$get("suppression_raw_counts")
        ))
    }

    if (privacy_controls$has("suppression_magnitude")) {
        items <- c(items, sprintf(
            "suppression of totals and means where underlying unrounded count < %s",
            privacy_controls$get("suppression_magnitude")
        ))
    }

    # ... handle suppression_quantiles, check_rse, seed similarly

    out_group(
        out_h2("Privacy and confidentialisation information"),
        out_blank(),
        out_bullet(items),
        out_blank(),
        out_text(
            "NOTE: this feature is still experimental, and all output should be manually\n",
            "checked before being made public. This is simply to aid that process.\n"
        ),
        out_rule("=", width),
        out_blank()
    )
}
```

**Tests**:

- `out_format_mat` with known numeric matrix + NAs
- `out_ci_section` structure test
- `out_privacy_section` with mock privacy controls object

**Verify**: `make test` passes.

---

### Step 6: Bridge Utilities

**File**: `R/output.R` (add to existing)
**What**: Utilities for incremental migration.

```r
# Convert any out_node to a character vector (bridge for migration)
flatten_node <- function(x, width = 100L) {
    if (is.character(x)) return(x)
    format_plain(x, width = width)
}
```

Also update `print.inzight.plotsummary` in `R/getSummary.R`:

```r
#' @export
print.inzight.plotsummary <- function(x, ...) {
    if (inherits(x, "out_doc")) {
        cat(format(x, format = "plain"), sep = "\n")
    } else {
        cat(x, sep = "\n")
    }
}
```

**Test**: existing print tests still pass, new doc object prints correctly.

**Verify**: `make test` passes.

---

### Step 7: Snapshot Fixtures

**What**: Before migrating any method, capture current output as reference fixtures.

Create a script `tests/testthat/fixtures/generate_fixtures.R`:

```r
library(iNZightPlots)
cas <- read.csv("tests/testthat/cas.csv", stringsAsFactors = TRUE)

fixtures <- list(
    # Summary
    summary_bar_oneway = inzsummary(~travel, data = cas),
    summary_bar_oneway_vertical = inzsummary(~travel, data = cas,
        table.direction = "vertical"),
    summary_bar_twoway = inzsummary(travel ~ gender, data = cas),
    summary_bar_twoway_vertical = inzsummary(travel ~ gender, data = cas,
        table.direction = "vertical"),
    summary_dot = inzsummary(~height, data = cas),
    summary_dot_by = inzsummary(height ~ gender, data = cas),
    summary_scatter_linear = getPlotSummary(height, armspan, data = cas,
        trend = "linear"),
    summary_scatter_quad = getPlotSummary(height, armspan, data = cas,
        trend = c("linear", "quadratic")),

    # Inference
    inference_bar_oneway = inzinference(~travel, data = cas),
    inference_bar_twoway = inzinference(travel ~ gender, data = cas),
    inference_dot_one = inzinference(~height, data = cas),
    inference_dot_two = inzinference(height ~ gender, data = cas),
    inference_scatter = getPlotSummary(height, armspan, data = cas,
        summary.type = "inference", trend = "linear", inference.type = "conf")
)

# Also with iris for reproducible examples
fixtures$summary_bar_iris = inzsummary(~Species, data = iris)
fixtures$inference_dot_iris = inzinference(~Sepal.Length, data = iris)
fixtures$inference_dot_iris_by = inzinference(Sepal.Length ~ Species, data = iris)

for (name in names(fixtures)) {
    saveRDS(fixtures[[name]],
        file.path("tests/testthat/fixtures", paste0(name, ".rds")))
}
```

Create `tests/testthat/test_output_regression.R`:

```r
# Template for regression tests (add per-method as migration proceeds)
cas <- read.csv("cas.csv", stringsAsFactors = TRUE)

test_that("summary.inzbar one-way output unchanged", {
    skip_if(!file.exists("fixtures/summary_bar_oneway.rds"))
    expected <- readRDS("fixtures/summary_bar_oneway.rds")
    actual <- inzsummary(~travel, data = cas)
    expect_identical(as.character(actual), as.character(expected))
})
```

**Verify**: Run fixture generation, then `make test` passes with regression tests.

---

### Step 8: Migrate `summary.inzbar` (one-way)

**File**: `R/summary.R` lines 678-765
**Complexity**: Low - simplest complete method
**What**: Replace the one-way branch of `summary.inzbar` with `out_*()` calls.

#### Current code (simplified):

```r
# Lines 678-765 of summary.R
cm <- c(tab, sum(tab))
perc <- ...
mat <- rbind(c(colnames(tab), "Total"), cm, pm)
mat <- cbind(c("", "Count", "Percent"), mat)
# ... apply formatting, right-justify, join ...
```

#### New code:

```r
# One-way branch of summary.inzbar
counts <- c(tab, Total = sum(tab))
perc <- c(tab / sum(tab) * 100, 100)  # raw percentages, NOT pre-formatted

# Apply privacy suppression to raw data (BEFORE creating table node)
if (!is.null(s_mat)) {
    counts <- privacy_controls$suppress(counts, s_mat)
    perc <- privacy_controls$suppress(perc, s_mat)
}

# Build raw numeric matrix: counts in row 1, percentages in row 2
mat_vals <- rbind(counts, perc)

node <- out_table(
    mat_vals,
    col_headers = c(colnames(tab), "Total"),
    row_headers = c("Count", "Percent"),
    digits = 0L  # counts and rounded percentages
)

# Handle survey: additional SE and deff rows as separate tables
if (is.survey) {
    se_vals <- ...   # raw numeric SE values
    deff_vals <- ... # raw numeric deff values
    node <- out_group(
        out_text("Population Estimates:"), out_blank(),
        node, out_blank(),
        out_text("Standard errors:"), out_blank(),
        out_table(se_vals, col_headers = ..., digits = 4L),
        out_blank(),
        out_text("Design effects:"), out_blank(),
        out_table(deff_vals, col_headers = ..., digits = 4L)
    )
}

# Handle vertical table direction
if (table.direction == "vertical") {
    # Transpose the raw matrix BEFORE creating the node
    # swap row_headers and col_headers, add separator_after for the total row
}

# Return character vector via bridge
out <- flatten_node(node)
```

**Key considerations**:

- The `table.direction == "vertical"` case transposes the matrix before creating the node. The current code does `mat <- t(mat)` and adds separator lines. In the new code, transpose the raw values matrix, then use `separator_after` parameter.
- Privacy suppression modifies `cm` and `pm` BEFORE they go into `out_table()`.
- Survey SE/deff sections become additional `out_table` nodes in an `out_group`.

**Test**: Run regression test from Step 7. Output must be character-identical.

**Verify**: `make test` passes.

---

### Step 9: Migrate `summary.inzscatter`

**File**: `R/summary.R` lines 770-933
**Complexity**: Low - primarily text and equations

#### Current pattern:

```r
out <- character()
if ("linear" %in% trend) {
    beta <- coef(lm(y ~ x))
    out <- c(out, "Linear trend:", "",
        sprintf("    %s = %s + %s * %s", vn$y, beta[1], beta[2], vn$x),
        paste0("    Linear correlation: ", cor(x, y)), "")
}
# ... quadratic, cubic similarly ...
out <- c(out, paste0("Rank correlation: ", rank.cor))
```

#### New code:

```r
out_parts <- list()

if ("linear" %in% trend) {
    beta <- try({ ... }, silent = TRUE)
    if (inherits(beta, "try-error")) {
        out_parts <- c(out_parts, list(out_text("Unable to fit linear trend.")))
    } else {
        out_parts <- c(out_parts, list(
            out_text("Linear trend:"),
            out_blank(),
            out_text(sprintf("    %s = %s %s %s * %s", ...)),
            out_text(paste0("    Linear correlation: ", ...)),
            out_blank()
        ))
    }
}
# ... quadratic, cubic similarly ...

out_parts <- c(out_parts, list(
    out_text(paste0("Rank correlation: ", rank.cor, ...))
))

flatten_node(do.call(out_group, out_parts))
```

**Test**: Regression test. **Verify**: `make test`.

---

### Step 10: Migrate `summary.inzdot`

**File**: `R/summary.R` lines 1-388
**Complexity**: Medium - survey/non-survey branching, SE/deff sections, privacy suppression

#### Structure overview:

1. Lines 1-76: Compute data matrix (non-survey: basic stats; survey: svyby calls)
2. Lines 77-275: Build the numeric matrix with estimates, SE, deff rows
3. Lines 277-308: Format numbers, apply privacy suppression
4. Lines 310-348: Add row/column labels, right-justify, join
5. Lines 350-385: Split into sections (estimates, SE, deff)

#### New code approach:

The computation (steps 1-2) stays the same - these produce raw numeric matrices. Step 3 applies privacy suppression on the raw data using `out_format_mat()` if needed, but most columns pass through unchanged. Steps 4-5 become separate `out_table()` nodes with raw numeric data:

```r
# Keep the raw numeric matrices from steps 1-2
# Apply privacy suppression to specific columns on the RAW data
if (!is.null(privacy_controls)) {
    est_mat <- out_format_mat(est_mat,
        privacy_controls = privacy_controls,
        suppress_cols = c(4L, 7L),  # e.g., IQR, n columns
        suppress_mat = s_mat)
}

# Build row/column structure
col_names <- rns  # "Min", "25%", "Median", etc.
row_names <- if (length(toplot) > 1) names(toplot) else NULL
sig <- opts$signif  # digits for formatting

if (exists("semat") && exists("deffmat")) {
    # Three sections as separate raw-data tables
    out_group(
        out_text(ifelse(is.null(des), "Estimates", "Population estimates:")),
        out_blank(),
        out_table(est_mat, col_headers = col_names, row_headers = row_names,
                  digits = sig),
        out_blank(),
        out_text("Standard error of estimates:"),
        out_blank(),
        out_table(semat, col_headers = col_names, row_headers = row_names,
                  digits = sig),
        out_blank(),
        out_text("Design effects:"),
        out_blank(),
        out_table(deffmat, col_headers = col_names, row_headers = row_names,
                  digits = sig)
    ) |> flatten_node()
} else if (exists("semat")) {
    # Two sections: estimates, SE
    # ... similar pattern with raw matrices ...
} else {
    # One section: estimates only
    out_table(est_mat, col_headers = col_names, row_headers = row_names,
              digits = sig) |> flatten_node()
}
```

**Key considerations**:

- The current code joins ALL sections into one big matrix then splits by row index. The new code keeps them as separate matrices from the start and creates separate `out_table` nodes. This is cleaner but requires careful index matching.
- Privacy suppression modifies specific columns (`mat[, 4L]`, `mat[, 7L]`, etc.) - this still operates on the numeric matrix before `out_format_mat()`.

**Test**: Regression tests for both survey and non-survey cases.

**Verify**: `make test`.

---

### Step 11: Migrate `summary.inzbar` (two-way)

**File**: `R/summary.R` lines 396-677
**Complexity**: High - the most complex summary method

#### Structure overview:

1. Lines 396-447: Setup, privacy controls, survey calculations
2. Lines 449-518: Build count table (mat1), format, handle vertical direction
3. Lines 528-581: Build percentage table (mat2), format, handle vertical direction
4. Lines 583-598: Combine into output sections
5. Lines 600-676: Optional SE and deff tables (survey only)

#### New code approach:

Each table (counts, percentages, SE, deff) becomes a separate `out_table()` node with raw numeric data:

```r
# 1. Build raw count matrix (numeric)
count_mat <- cbind(tab, Total = rowSums(tab))
# Apply privacy suppression on raw data...
if (!is.null(s_mat)) {
    count_mat <- privacy_controls$suppress(count_mat, s_mat)
}

count_table <- out_table(
    count_mat,
    col_headers = c(colnames(tab), paste(dir_label, "Total")),
    row_headers = rownames(tab),
    separator_after = if (table.direction == "vertical") nrow(tab) else NULL,
    digits = 0L
)

# 2. Build raw percentage matrix (numeric)
pct_mat <- prop.table(tab, margin = ...) * 100  # raw percentages

pct_table <- out_table(
    pct_mat,
    col_headers = colnames(tab),
    row_headers = rownames(tab),
    digits = as.integer(opts$round_percent)
)

# 3. Combine
sections <- list(
    out_text(sprintf("Table of %sCounts:", survey_prefix)),
    out_blank(),
    count_table,
    out_blank(),
    out_text(sprintf("Table of %sPercentages (within categories of %s):",
        survey_prefix, vn$y)),
    out_blank(),
    pct_table
)

# 4. Survey-only: SE and deff tables (raw numeric)
if (is.survey) {
    sections <- c(sections, list(
        out_blank(),
        out_text("Standard errors of estimated percentages:"),
        out_blank(),
        out_table(se_mat, col_headers = ..., row_headers = ..., digits = 4L),
        if (!isFALSE(survey.options$deff)) out_group(
            out_blank(),
            out_text("Design effects:"),
            out_blank(),
            out_table(deff_mat, col_headers = ..., row_headers = ..., digits = 4L)
        )
    ))
}

flatten_node(do.call(out_group, sections))
```

**Key considerations**:

- The `table.direction == "vertical"` case transposes the matrix. Do this BEFORE creating the `out_table` node. The `separator_after` parameter handles the total line separator.
- RSE markup (`privacy_controls$markup()`) modifies cell values in-place, then switches to left-justify. Handle this by passing `justify = "left"` when markup is applied.

**Test**: Regression tests for horizontal and vertical directions, with and without survey.

**Verify**: `make test`.

---

### Step 12: Migrate `inference.inzscatter`

**File**: `R/inference.R` lines 1546-1696
**Complexity**: Medium - trend coefficient tables

#### Structure:

1. Loop over trend types (linear, quadratic, cubic)
2. For each: fit model, format coefficients + CI + p-values
3. Build table with row names (Intercept, x, x^2, x^3)
4. Optional bootstrap: different columns, plus correlation row

#### New code:

```r
for (t in tr) {
    # ... fit model, get cc (coefficients) and ci (confidence intervals) ...

    # Store raw numeric data; formatting (significant digits, p-value display)
    # is handled by format_plain.out_table
    if (bs) {
        mat_vals <- cbind(cc[, 1], ci[, 1], ci[, 2])
    } else {
        mat_vals <- cbind(cc[, 1], ci[, 1], ci[, 2], cc[, 4])
    }

    rn <- paste0(vn$x, c("", "^2", "^3"))
    row_names <- c("Intercept", rn[1:t])

    trend_table <- out_table(
        mat_vals,
        col_headers = c("Estimate", "Lower", "Upper", if (!bs) "p-value"),
        row_headers = row_names,
        digits = as.integer(opts$signif)
    )

    header <- paste0(
        switch(t, "Linear", "Quadratic", "Cubic"),
        " Trend Coefficients with ", ci.width * 100, "% ",
        ifelse(bs, "Percentile Bootstrap ", ""),
        "Confidence Intervals"
    )

    parts <- c(parts, list(
        out_blank(),
        out_text(header),
        out_blank(),
        trend_table
    ))
}

# Add p-value key at end
if (!bs) {
    parts <- c(parts, list(
        out_blank(), out_blank(),
        out_text("   p-values for the null hypothesis of no association, H0: beta = 0")
    ))
}

flatten_node(do.call(out_group, parts))
```

**Test**: Regression tests. **Verify**: `make test`.

---

### Step 13: Migrate `inference.inzdot`

**File**: `R/inference.R` lines 7-597
**Complexity**: High - many branches

#### Sub-sections to migrate:

**A. CI tables for mean** (lines 24-72):

```r
mat <- inf$mean$conf[, c("mean", "lower", "upper"), drop = FALSE]
```

Use `out_ci_section()` helper from Step 5.

**B. Bootstrap median and IQR** (lines 74-196):
Same pattern repeated 3 times. Each becomes an `out_ci_section()`.

**C. Two-sample t-test** (lines 198-291):

```r
out_test(
    name = ifelse(is.survey, "Design-based Two Sample T-test", "Welch Two Sample t-test"),
    statistic = c(t = ttest$statistic),    # raw numeric
    parameter = c(df = ttest$parameter),   # raw numeric
    p_value = ttest$p.value,               # raw numeric
    null_hyp = paste0("true difference in means is equal to ", null_value),
    alt_hyp = paste0("true difference in means is ", alt_text),
    opts = list(signif = opts$signif)
)
```

**D. ANOVA F-test** (lines 324-372):

```r
out_test(
    name = "One-way Analysis of Variance (ANOVA F-test)",
    statistic = c(F = aov_result$statistic),   # raw numeric
    parameter = c(df = aov_result$parameter),  # raw numeric (or named vector for df1, df2)
    p_value = aov_result$p.value,              # raw numeric
    null_hyp = "true group means are all equal",
    alt_hyp = "true group means are not all equal",
    opts = list(signif = opts$signif)
)
```

**E. Difference in means CI** (lines 374-452):
Use `out_ci_section()` for two-sample case, `out_table_pairwise()` for multi-group.

**F. One-sample t-test** (lines 550-594):
Use `out_test()`.

Each sub-section can be migrated independently within the method. Build the result as a list of nodes, then `flatten_node(do.call(out_group, parts))`.

**Test**: Regression tests for one-sample, two-sample, and multi-group cases, with and without hypothesis test, with and without bootstrap.

**Verify**: `make test`.

---

### Step 14: Migrate `inference.inzbar` (one-way)

**File**: `R/inference.R` lines 1243-1409
**Complexity**: Medium

#### Sub-sections:

**A. Proportion CI table** (lines 1243-1296):

```r
out_ci_section(estimates, lower, upper,
    label = "Estimated Proportions",
    ci_width = ci_width, bootstrap = bs,
    factor_names = LEVELS)
```

**B. Hypothesis test** (various proportion/chi2 tests):
Already computed as `HypOut`. Convert to `out_test()` where possible, or keep as text for complex cases.

**C. Pairwise proportion differences** (lines 1344-1408):
Use `out_table_pairwise()`.

**Test**: Regression tests. **Verify**: `make test`.

---

### Step 15: Migrate `inference.inzbar` (two-way)

**File**: `R/inference.R` lines 693-1242
**Complexity**: Very High - most complex method in the package

#### Sub-sections:

**A. Proportion estimate table** (lines 933-977):

- Use `out_table()` for the estimate matrix

**B. CI matrix** (lines 979-1025):

- Two rows per group (lower, upper). Use `out_table()`.

**C. Hypothesis test** (lines 1027-1031):

- Inline the `HypOut` as `out_test()` or text

**D. Per-column pairwise differences** (lines 1084-1157):

- Loop over columns, each producing an `out_table_pairwise()` with heading

**E. Epi calculations** (lines 1159-1241):

- OR, RR, RD sections. Migrate after Step 16.

**Key complexity**: The nested loop structure (for each column of the two-way table, compute pairwise differences) produces multiple subsections. Build as a list of nodes.

**Test**: Regression tests for various two-way cases. **Verify**: `make test`.

---

### Step 16: Migrate `epi.format`

**File**: `R/inference_epi.R`
**Complexity**: Low

The `epi.format()` function already formats a matrix. Wrap it with `out_table()`:

```r
epi.format <- function(mat, label, names, null_value = 1) {
    # ... existing computation ...
    out_table(formatted_mat,
        col_headers = c(label, "Lower", "Upper", "P-value"),
        row_headers = comparison_labels,
        indent = 3L
    ) |> flatten_node()
}
```

**Test**: Regression test. **Verify**: `make test`.

---

### Step 17: Migrate `summary.inzdata`

**File**: `R/getSummary.R` lines 691-835
**Complexity**: Low

This standalone method has its own `add()` closure. Convert to:

```r
summary.inzdata <- function(object, des, width = 100, ...) {
    doc <- out_doc(
        out_h1(sprintf("iNZight summary of %s", dataset_name), width),
        out_kv(
            "Number of observations (rows)" = nrow(object),
            "Number of variables (columns)" = sprintf("%s (%s numeric and %s categorical)", ...)
        ),
        out_blank(),
        out_rule("=", width),
        # Numeric variables section
        if (n.numeric > 0) out_group(
            out_h2("Numeric variables:"),
            out_blank(),
            out_table(num_mat, col_headers = c("min", "max", "n. missing"), row_headers = names(numvars)),
            out_blank()
        ),
        # Categorical variables section
        if (n.factor > 0) out_group(
            out_blank(),
            out_h2("Categorical variables:"),
            out_blank(),
            out_table(cat_mat, col_headers = c("n. categories", "n. missing"), row_headers = names(catvars)),
            out_blank()
        ),
        out_rule("=", width),
        width = width
    )
    class(doc) <- c("inzight.plotsummary", class(doc))
    doc
}
```

**Test**: Regression test. **Verify**: `make test`.

---

### Step 18: Migrate the Orchestrator

**File**: `R/getSummary.R` lines 218-689
**Complexity**: Medium-High

This is the central `summary.inzplotoutput` method. It:

1. Sets up variables and helpers (lines 218-266)
2. Builds document header (lines 274-421)
3. Builds privacy section (lines 424-502)
4. Loops over g2 levels > g1 levels, calling summary/inference for each (lines 522-677)
5. Adds footer (lines 679-689)

#### New approach:

```r
summary.inzplotoutput <- function(object, summary.type = "summary", ...) {
    # ... same setup code (lines 218-270) ...

    parts <- list()

    # Header
    parts <- c(parts, list(
        out_h1(header_text, width),
        out_kv(header_metadata),
        # ... survey info if needed ...
        out_rule("=", width),
        out_blank()
    ))

    # Privacy section
    if (!is.null(privacy_controls)) {
        parts <- c(parts, list(out_privacy_section(privacy_controls, width)))
    }

    # Main content loop
    for (this in names(obj)) {
        if (this != "all") {
            parts <- c(parts, list(
                out_rule("=", width),
                out_indent(out_text(paste0("For the subset where ", vnames$g2, " = ", this)), 5)
            ))
        }

        for (o in names(obj[[this]])) {
            pl <- obj[[this]][[o]]
            header <- build_section_header(...)  # extract header logic

            if (o != "all") {
                parts <- c(parts, list(out_rule("-", width)))
                header <- paste0(header, ", for ", vnames$g1, " = ", o)
            }

            parts <- c(parts, list(
                out_h2(paste0(header, ":")),
                out_blank()
            ))

            # Call the summary/inference method
            result <- switch(summary.type,
                "summary" = summary(pl, opts = inzpars, ...),
                "inference" = inference(pl, ...)
            )

            # If result is an out_node, add directly; if character, wrap
            if (inherits(result, "out_node")) {
                parts <- c(parts, list(result))
            } else {
                parts <- c(parts, list(out_text(result)))
            }

            parts <- c(parts, list(out_blank()))
        }
        parts <- c(parts, list(out_blank()))
    }

    parts <- c(parts, list(out_rule("=", width), out_blank(), out_blank()))

    doc <- do.call(out_doc, c(parts, list(width = width)))
    class(doc) <- c("inzight.plotsummary", class(doc))
    doc
}
```

**Key**: During migration, the method detects whether child results are `out_node` (new) or character (old) and handles both. This allows the orchestrator to be migrated before all children are converted (or vice versa).

**Test**: Full regression tests for all combinations. **Verify**: `make test`.

---

### Step 19: Cleanup

After all methods are migrated:

1. **Remove `formatTriMat()`** from `R/inference.R` (replaced by `out_table_tri()`)
2. **Remove `formatMat()`** from `R/inference.R` (replaced by `out_table()`)
3. **Remove the `add()` closure pattern** from `summary.inzplotoutput` and `summary.inzdata`
4. **Remove `centerText()`** from `R/getSummary.R` if only used by `out_h1` (keep if used elsewhere)
5. **Update NAMESPACE** if any `out_*` functions should be exported
6. **Run full test suite**: `make test` and `make check`
7. **Verify line count reduction**: summary.R ~941 -> ~500, inference.R ~1707 -> ~900

---

### Step 20: HTML Rendering

**File**: `R/output.R` (add `format_html.*` methods)
**Complexity**: Medium

Because nodes store raw data, HTML rendering can leverage rich R packages rather than hand-writing HTML. Use `{gt}` for tables and `{htmltools}` for structural elements.

**Dependencies**: Add `gt` and `htmltools` to `Suggests` in DESCRIPTION (they are only needed for HTML output, not plain text).

#### Tables via `{gt}`

```r
format_html.out_table <- function(x, ...) {
    # Build a data.frame from the raw matrix
    df <- as.data.frame(x$mat)
    if (!is.null(x$col_headers)) {
        colnames(df) <- x$col_headers
    }
    if (!is.null(x$row_headers)) {
        df <- cbind(` ` = x$row_headers, df)
    }

    tbl <- gt::gt(df) |>
        gt::fmt_number(decimals = x$digits) |>
        gt::tab_options(table.css.id = "inzight-table")

    if (!is.null(x$caption)) {
        tbl <- tbl |> gt::tab_header(title = x$caption)
    }

    as.character(gt::as_raw_html(tbl))
}
```

#### Text with styling

```r
format_html.out_text <- function(x, ...) {
    text <- htmltools::htmlEscape(x$text)

    # Apply inline styling
    if (x$.bold) text <- paste0("<strong>", text, "</strong>")
    if (x$.italic) text <- paste0("<em>", text, "</em>")

    style <- if (!is.null(x$.colour)) paste0("color:", x$.colour, ";") else ""
    cls <- if (!is.null(x$.css_class)) paste0(' class="', x$.css_class, '"') else ""

    if (nzchar(style) || nzchar(cls)) {
        style_attr <- if (nzchar(style)) paste0(' style="', style, '"') else ""
        paste0("<span", cls, style_attr, ">", text, "</span>")
    } else {
        paste0("<p>", text, "</p>")
    }
}
```

#### Headings and structural elements

```r
format_html.out_h1 <- function(x, ...) {
    paste0('<h1 class="inzight-title">', htmltools::htmlEscape(x$text), '</h1>')
}

format_html.out_h2 <- function(x, ...) {
    paste0('<h2>', htmltools::htmlEscape(x$text), '</h2>')
}

format_html.out_rule <- function(x, ...) {
    '<hr class="inzight-rule">'
}

format_html.out_blank <- function(x, ...) {
    '<br>'
}
```

#### Hypothesis tests

```r
format_html.out_test <- function(x, ...) {
    # Format from raw values
    opts <- x$opts %||% list(signif = 4L)
    stat_name <- names(x$statistic)
    stat_val <- format(x$statistic, digits = opts$signif)
    param_name <- names(x$parameter)
    param_val <- format(x$parameter, digits = opts$signif)
    p_display <- format_pval(x$p_value, opts)

    paste0(
        '<div class="hypothesis-test">',
        '<h3>', htmltools::htmlEscape(x$name), '</h3>',
        '<p class="test-stat">',
        stat_name, ' = ', stat_val, ', ',
        param_name, ' = ', param_val, ', ',
        'p-value ', p_display,
        '</p>',
        '<dl>',
        '<dt>Null Hypothesis</dt><dd>', htmltools::htmlEscape(x$null_hyp), '</dd>',
        '<dt>Alternative Hypothesis</dt><dd>', htmltools::htmlEscape(x$alt_hyp), '</dd>',
        '</dl>',
        '</div>'
    )
}
```

#### Document wrapper with custom CSS

```r
format_html.out_doc <- function(x, ...) {
    body <- vapply(x$nodes, format_html, character(1))

    css <- x$css %||% default_inzight_css()
    paste0(
        '<div class="inzight-output">',
        '<style>', css, '</style>',
        paste(body, collapse = "\n"),
        '</div>'
    )
}

default_inzight_css <- function() {
    "
    .inzight-output { font-family: sans-serif; max-width: 800px; }
    .inzight-title { text-align: center; border-top: 2px solid #333; border-bottom: 1px solid #333; }
    .inzight-rule { border: 0; border-top: 1px solid #999; }
    .hypothesis-test { margin: 1em 0; padding: 0.5em; background: #f8f8f8; }
    .hypothesis-test dt { font-weight: bold; }
    "
}
```

#### Wiring up

Wire up the `html = TRUE` parameter in `getPlotSummary` (already exists but unused):

```r
# In getPlotSummary, after getting the summary object:
if (html) {
    return(format(result, format = "html"))
}
```

---

## Testing Strategy

### Layer 1: Node Data Tests (`test_output.R`)

Test that nodes store the correct raw data. Because nodes hold structured R objects (numeric matrices, test statistics, p-values), these tests are straightforward assertions on data, not string parsing:

```r
test_that("out_table stores raw numeric matrix", {
    m <- matrix(c(1.23, 4.56, 78.9, 123.0), nrow = 2)
    node <- out_table(m, col_headers = c("A", "B"), digits = 3L)
    expect_equal(node$mat, m)
    expect_equal(node$col_headers, c("A", "B"))
    expect_equal(node$digits, 3L)
})

test_that("out_test stores raw statistic values", {
    node <- out_test("t-test", c(t = 2.5), c(df = 98), 0.014,
        null_hyp = "mu = 0", alt_hyp = "mu != 0")
    expect_equal(node$p_value, 0.014)
    expect_equal(node$statistic, c(t = 2.5))
})
```

**Coverage target**: 95% of `R/output.R` and `R/output_helpers.R`.

### Layer 2: Rendering Tests (`test_output.R`)

Test that `format_plain()` produces the correct character output for each node type. These verify backward compatibility of the plain text output:

```r
test_that("format_plain.out_table reproduces expected layout", {
    m <- matrix(c(1, 22, 333), nrow = 1)
    node <- out_table(m, col_headers = c("A", "B", "C"), digits = 0L)
    lines <- format_plain(node)
    expect_match(lines[1], "^\\s+A\\s+B\\s+C$")
})
```

### Layer 3: Regression Tests (`test_output_regression.R`)

Before-and-after comparison using saved fixtures. One test per migrated method per major case (survey/non-survey, one-way/two-way, with/without hypothesis, etc.).

**Created in Step 7, expanded at each migration step.**

### Layer 4: Existing Tests

The existing `test_summary.R`, `test_inference.R`, `test_survey_inference.R`, `test_privacy_controls.R` must continue to pass throughout. These parse the character output to check content, so they verify backward compatibility.

### Running Tests

```bash
# In package directory:
cd pkgs/iNZightPlots

# Run all tests
make test
# or
Rscript -e "devtools::test()"

# Run specific test file
Rscript -e "devtools::test_file('tests/testthat/test_output.R')"

# Full check
make check
```

---

## Risks and Mitigations

| Risk                                   | Severity | Mitigation                                                                             |
| -------------------------------------- | -------- | -------------------------------------------------------------------------------------- |
| Plain text output differs from current | High     | Snapshot regression tests at every step                                                |
| Privacy controls interact badly        | Medium   | Suppression modifies data BEFORE `out_table()`; this separation is preserved by design |
| Scope creep                            | Medium   | Each step is independently mergeable                                                   |
| `sapply(..., add)` pattern breaks      | Medium   | `flatten_node()` bridge converts nodes to character during migration                   |
| Performance regression                 | Low      | Output generation is fast; benchmark after Step 8                                      |
| `table.direction = "vertical"`         | Medium   | Transpose raw matrix BEFORE node creation; test both directions                        |

---

## File Map

| File                                      | Action                                                      | Steps             |
| ----------------------------------------- | ----------------------------------------------------------- | ----------------- |
| `R/output.R`                              | **NEW**                                                     | 1, 2, 3, 4, 6, 20 |
| `R/output_helpers.R`                      | **NEW**                                                     | 5                 |
| `tests/testthat/test_output.R`            | **NEW**                                                     | 1-5               |
| `tests/testthat/test_output_regression.R` | **NEW**                                                     | 7-18              |
| `tests/testthat/fixtures/`                | **NEW** directory                                           | 7                 |
| `R/getSummary.R`                          | MODIFY print method (Step 6), orchestrator (Step 18)        | 6, 17, 18         |
| `R/summary.R`                             | MODIFY all summary methods                                  | 8-11              |
| `R/inference.R`                           | MODIFY all inference methods, remove formatTriMat/formatMat | 12-15, 19         |
| `R/inference_epi.R`                       | MODIFY epi.format                                           | 16                |
| `R/general.R`                             | NO CHANGE                                                   | -                 |
| `NAMESPACE`                               | MODIFY if exporting out\_\*                                 | 1                 |
| `DESCRIPTION`                             | MODIFY: add `gt`, `htmltools` to Suggests (Step 20)         | 20                |

---

## Dependency Order

```
Step 1 (core) ─→ Step 2 (out_table) ─→ Step 3 (text) ─→ Step 4 (structural)
                                                              │
                                                              ▼
                                                         Step 5 (helpers)
                                                              │
                                                              ▼
                                                         Step 6 (bridge)
                                                              │
                                                              ▼
                                                         Step 7 (fixtures)
                                                              │
              ┌───────────────────────────────────────────────┤
              ▼                                               ▼
         Step 8 (bar 1-way)                             Step 9 (scatter)
              │                                               │
              ▼                                               ▼
         Step 11 (bar 2-way)                            Step 10 (dot)
              │                                               │
              └───────────────┬───────────────────────────────┘
                              ▼
                    Step 12-16 (inference methods)
                              │
                              ▼
                    Step 17 (inzdata)
                              │
                              ▼
                    Step 18 (orchestrator)
                              │
                              ▼
                    Step 19 (cleanup)
                              │
                              ▼
                    Step 20 (HTML)
```

Steps 8-11 and 12-16 can be done in any order within their group. Steps 8-16 can be interleaved. Step 18 can be done at any point after Step 6, but is cleanest after all methods are migrated.
