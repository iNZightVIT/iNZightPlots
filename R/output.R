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
