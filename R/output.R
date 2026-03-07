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
