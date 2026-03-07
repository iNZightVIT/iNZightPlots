out_node <- function(type, ...) {
    node <- list(...)
    class(node) <- c(paste0("out_", type), "out_node")
    node
}

#' format method for nodes
#' @param format the desired output format ("plain" or "html" at present)
#' @param width the width of the output
#'
#' @export
format.out_node <- function(x, format = c("plain", "html"), width = 100L, ...) {
    format <- match.arg(format)
    switch(format,
        plain = format_plain(x, width = width),
        html = format_html(x, width = width)
    )
}

format_plain <- function(x, ...) {
    UseMethod("format_plain")
}

format_html <- function(x, ...) {
    paste0("<pre>", paste(format_plain(x, ...), collapse = "\n"), "</pre>")
}

# --- out_table ---------------------------------------------------------------

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

format_plain.out_table <- function(x, ...) {
    mat <- x$mat

    mat <- matrix(
        vapply(seq_len(ncol(mat)), function(j) {
            col <- mat[, j]
            if (is.numeric(col)) {
                fmt_args <- list(col, scientific = x$scientific)
                if (x$digits > 0L) {
                    fmt_args$digits <- x$digits
                } else {
                    fmt_args$nsmall <- 0L
                    fmt_args$big.mark <- ""
                }
                formatted <- do.call(format, fmt_args)
            } else {
                formatted <- as.character(col)
            }
            formatted[grep("NA", formatted)] <- x$na_replace
            if (!is.null(x$nan_replace)) {
                formatted <- gsub("NaN", x$nan_replace, formatted)
            }
            formatted
        }, character(nrow(mat))),
        nrow = nrow(mat)
    )

    if (!is.null(x$col_headers)) {
        mat <- rbind(x$col_headers, mat)
    }

    if (!is.null(x$row_headers)) {
        corner <- if (!is.null(x$col_headers)) "" else character(0)
        rh <- c(corner, x$row_headers)
        mat <- cbind(rh, mat)
    }

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

    lines <- apply(mat, 1L, function(row) {
        paste0(strrep(" ", x$indent), paste(row, collapse = x$col_sep))
    })

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

# --- text primitives ---------------------------------------------------------

out_text <- function(..., .bold = FALSE, .italic = FALSE,
                     .colour = NULL, .css_class = NULL) {
    text <- paste0(..., collapse = "")
    out_node("text",
        text = text,
        .bold = .bold,
        .italic = .italic,
        .colour = .colour,
        .css_class = .css_class
    )
}

format_plain.out_text <- function(x, ...) {
    x$text
}

out_blank <- function(n = 1L) {
    out_node("blank", n = as.integer(n))
}

format_plain.out_blank <- function(x, ...) {
    rep("", x$n)
}

out_bullet <- function(items, indent = 2L, bullet = "* ") {
    out_node("bullet",
        items = items,
        indent = as.integer(indent),
        bullet = bullet
    )
}

format_plain.out_bullet <- function(x, ...) {
    paste0(strrep(" ", x$indent), x$bullet, x$items)
}

out_indent <- function(node, n = 3L) {
    out_node("indent", node = node, n = as.integer(n))
}

format_plain.out_indent <- function(x, ...) {
    lines <- format_plain(x$node, ...)
    paste0(strrep(" ", x$n), lines)
}

# --- structural primitives ---------------------------------------------------

out_h1 <- function(text, width = 100L) {
    out_node("h1", text = text, width = as.integer(width))
}

format_plain.out_h1 <- function(x, ...) {
    pad <- floor((x$width - nchar(x$text)) / 2)
    c(
        strrep("=", x$width),
        paste0(strrep(" ", pad), x$text),
        strrep("-", x$width)
    )
}

out_h2 <- function(text) {
    out_node("h2", text = text)
}

format_plain.out_h2 <- function(x, ...) {
    c(x$text, strrep("-", nchar(x$text)))
}

out_rule <- function(char = "=", width = 100L) {
    stopifnot(nchar(char) == 1L)
    out_node("rule", char = char, width = as.integer(width))
}

format_plain.out_rule <- function(x, ...) {
    strrep(x$char, x$width)
}

out_group <- function(...) {
    nodes <- list(...)
    nodes <- nodes[!vapply(nodes, is.null, logical(1))]
    out_node("group", nodes = nodes)
}

format_plain.out_group <- function(x, ...) {
    unlist(lapply(x$nodes, function(node) {
        if (is.character(node)) return(node)
        format_plain(node, ...)
    }))
}

out_doc <- function(..., width = 100L) {
    nodes <- list(...)
    nodes <- nodes[!vapply(nodes, is.null, logical(1))]
    out_node("doc", nodes = nodes, width = as.integer(width))
}

format_plain.out_doc <- function(x, ...) {
    unlist(lapply(x$nodes, function(node) {
        if (is.character(node)) return(node)
        format_plain(node, width = x$width)
    }))
}

out_kv <- function(..., indent = 3L) {
    args <- list(...)
    if (length(args) == 1L && is.list(args[[1]])) {
        pairs <- args[[1]]
    } else {
        pairs <- args
    }
    out_node("kv", pairs = pairs, indent = as.integer(indent))
}

format_plain.out_kv <- function(x, ...) {
    keys <- format(paste0(names(x$pairs), ": "), justify = "right")
    paste0(strrep(" ", x$indent), keys, x$pairs)
}

out_test <- function(name, statistic, parameter, p_value,
                     null_hyp, alt_hyp, extras = NULL, opts = NULL) {
    out_node("test",
        name = name,
        statistic = statistic,
        parameter = parameter,
        p_value = p_value,
        null_hyp = null_hyp,
        alt_hyp = alt_hyp,
        extras = extras,
        opts = opts
    )
}

format_plain.out_test <- function(x, ...) {
    stat_parts <- paste0(
        names(x$statistic), " = ",
        format(x$statistic, digits = 5)
    )
    param_parts <- paste0(
        names(x$parameter), " = ",
        format(x$parameter, digits = 5)
    )
    pval <- format_pval(x$p_value, x$opts %||% list(min_pval = 2.2e-16))
    pval_prefix <- if (startsWith(pval, "<")) "" else "= "

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

out_table_tri <- function(mat, names, digits = 3L) {
    stopifnot(is.matrix(mat), nrow(mat) == ncol(mat))
    out_node("table_tri",
        mat = mat,
        names = names,
        digits = as.integer(digits)
    )
}

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

out_table_pairwise <- function(mat, levels = NULL, digits = 4L, indent = 1L) {
    stopifnot(is.matrix(mat) || is.data.frame(mat))
    mat <- as.matrix(mat)
    out_node("table_pairwise",
        mat = mat,
        levels = levels,
        digits = as.integer(digits),
        indent = as.integer(indent)
    )
}

format_plain.out_table_pairwise <- function(x, ...) {
    mat <- x$mat

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
        rl <- cumsum(rl) + 2L # +2 for header + separator
        for (i in rev(rl)) {
            if (i <= length(lines)) {
                lines[i] <- paste0(lines[i], "\n")
            }
        }
    }

    paste0(strrep(" ", x$indent), lines)
}
