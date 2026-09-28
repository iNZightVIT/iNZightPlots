#' JSON-safe mirror of an iNZight plot
#'
#' Copies an \code{inzplotoutput} into a list with the same nesting and field
#' names. Palette functions and other values that cannot be serialized are
#' omitted. The plot object itself is left unchanged.
#'
#' @param obj an \code{inzplotoutput} object from \code{\link{iNZightPlot}}.
#' @return A list. Group levels, \code{$gen}, \code{$xlim}, and \code{$ylim}
#'   stay where they are. \code{$meta} holds a fixed set of attributes, and
#'   each panel has a \code{class} field.
#' @export
as_plot_structure <- function(obj) {
    if (!inherits(obj, "inzplotoutput")) {
        stop("'obj' must be an inzplotoutput object")
    }

    plottype <- attr(obj, "plottype")
    pt <- if (is.null(plottype) || !length(plottype)) "" else plottype[[1L]]

    out <- list()
    for (nm in names(obj)) {
        if (nm == "gen") {
            out$gen <- sanitize_gen(obj[[nm]], pt)
        } else if (nm %in% c("xlim", "ylim")) {
            kept <- sanitize_value(obj[[nm]])
            if (!is_drop(kept)) out[[nm]] <- kept
        } else {
            out[[nm]] <- sanitize_g2(obj[[nm]])
        }
    }

    out$meta <- list(
        varnames = keep_or_null(sanitize_value(attr(obj, "varnames"))),
        vartypes = keep_or_null(sanitize_value(attr(obj, "vartypes"))),
        plottype = keep_or_null(sanitize_value(attr(obj, "plottype"))),
        glevels = keep_or_null(sanitize_value(attr(obj, "glevels"))),
        missing = keep_or_null(sanitize_value(attr(obj, "missing"))),
        total.missing = keep_or_null(sanitize_value(attr(obj, "total.missing"))),
        total.obs = keep_or_null(sanitize_value(attr(obj, "total.obs")))
    )
    out
}

sanitize_gen <- function(gen, plottype) {
    if (!is.list(gen)) {
        return(list())
    }
    list(
        opts = filter_opts(gen$opts, plottype),
        mcex = keep_or_null(sanitize_value(gen$mcex)),
        col.args = keep_or_null(sanitize_value(gen$col.args)),
        maxcount = keep_or_null(sanitize_value(gen$maxcount))
    )
}

filter_opts <- function(opts, plottype) {
    if (!is.list(opts) || !length(opts)) {
        return(list())
    }
    nms <- names(opts)
    if (is.null(nms)) {
        return(list())
    }
    keep <- valid_par(nms, plottype, "plot")
    kept <- unclass(opts)[keep]
    filtered <- sanitize_list(kept)
    if (is_drop(filtered)) list() else filtered
}

sanitize_g2 <- function(g2) {
    if (!is.list(g2)) {
        kept <- sanitize_value(g2)
        return(if (is_drop(kept)) list() else kept)
    }
    nms <- names(g2)
    out <- list()
    for (i in seq_along(g2)) {
        panel <- sanitize_panel(g2[[i]])
        nm <- if (is.null(nms) || !nzchar(nms[[i]])) as.character(i) else nms[[i]]
        out[[nm]] <- panel
    }
    out
}

sanitize_panel <- function(panel) {
    if (!is.list(panel)) {
        kept <- sanitize_value(panel)
        return(if (is_drop(kept)) list() else kept)
    }
    cls <- class(panel)[[1L]]
    nms <- names(panel)
    nms <- nms[!nms %in% c("svy", "args")]
    out <- list()
    for (nm in nms) {
        child <- sanitize_value(panel[[nm]])
        if (is_drop(child)) next
        out[nm] <- list(child)
    }
    out["class"] <- list(cls)
    out
}

sanitize_list <- function(x) {
    if (!length(x)) {
        return(list())
    }
    nms <- names(x)
    out <- list()
    for (i in seq_along(x)) {
        child <- sanitize_value(x[[i]])
        if (is_drop(child)) next
        nm <- if (is.null(nms)) "" else nms[[i]]
        if (is.null(nm) || !nzchar(nm)) {
            out[length(out) + 1L] <- list(child)
        } else {
            out[nm] <- list(child)
        }
    }
    if (!length(out)) drop_marker() else out
}

sanitize_value <- function(x) {
    if (is.null(x)) {
        return(NULL)
    }
    if (is.function(x) || is.environment(x) || isS4(x) ||
        inherits(x, "survey.design")) {
        return(drop_marker())
    }
    if (is.atomic(x)) {
        ## `table` keeps its dim, and jsonlite has no asJSON method for that class.
        if (inherits(x, "table")) {
            x <- unclass(x)
        }
        return(x)
    }
    if (is.list(x)) {
        return(sanitize_list(x))
    }
    drop_marker()
}

keep_or_null <- function(x) {
    if (is_drop(x)) NULL else x
}

drop_marker <- function() {
    structure(list(), class = "inz_plot_structure_drop")
}

is_drop <- function(x) {
    inherits(x, "inz_plot_structure_drop")
}
