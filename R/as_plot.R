#' Drawing payload for an iNZight plot
#'
#' Turns an \code{inzplotoutput} into the list a client draws, and that
#' rserve-ts can send. Absent optional fields are left out. The plot object
#' is not changed.
#'
#' @param obj an \code{inzplotoutput} object from \code{\link{iNZightPlot}}.
#' @return A list with \code{schemaVersion}, \code{type}, \code{variables},
#'   \code{panels}, and \code{layout} when the plot is faceted.
#' @export
as_plot <- function(obj) {
    if (!inherits(obj, "inzplotoutput")) {
        stop("'obj' must be an inzplotoutput object")
    }

    type <- attr(obj, "plottype")
    if (is.null(type) || !length(type)) {
        stop("'obj' has no plottype")
    }
    type <- as.character(type[[1L]])

    vn <- attr(obj, "varnames")
    if (is.null(vn)) vn <- list()
    gl <- attr(obj, "glevels")
    if (is.null(gl)) gl <- list()

    g2_names <- setdiff(names(obj), c("gen", "xlim", "ylim"))
    if (!length(g2_names)) {
        stop("'obj' has no panels")
    }
    matrix_layout <- !is.null(vn$g2) && (
        level_requests_matrix(gl$g2.level) || length(g2_names) > 1L
    )

    variables <- omit_null(list(
        v1 = scalar_chr(vn$x),
        v2 = scalar_chr(vn$y),
        s1 = scalar_chr(vn$g1),
        s2 = scalar_chr(vn$g2),
        colby = scalar_chr(vn$colby),
        symbolby = scalar_chr(vn$symbolby),
        sizeby = scalar_chr(vn$sizeby)
    ))

    ctx <- list(
        type = type,
        has_y = !is.null(vn$y),
        symbolby = !is.null(vn$symbolby),
        cex.pt = point_cex(obj),
        x_bounds = unname(as.numeric(obj$xlim)),
        y_bounds = unname(as.numeric(obj$ylim)),
        grid_n = grid_n(obj)
    )

    panels <- list()
    for (g2 in g2_names) {
        g1s <- names(obj[[g2]])
        if (is.null(g1s)) next
        for (g1 in g1s) {
            base <- omit_null(list(
                s1 = if (!is.null(vn$g1)) g1 else NULL,
                s2 = if (!is.null(vn$g2) && !identical(g2, "all")) g2 else NULL
            ))
            panels[[length(panels) + 1L]] <- c(
                base,
                panel_payload(obj[[g2]][[g1]], ctx)
            )
        }
    }

    omit_null(list(
        schemaVersion = 1L,
        type = type,
        availablePlotTypes = as.list(available_plot_types(obj)),
        histBins = hist_bin_count(obj),
        variables = variables,
        layout = if (length(panels) > 1L) list(matrix = isTRUE(matrix_layout)) else NULL,
        panels = panels
    ))
}

# Types createPlot will keep for this plot. The same checks live in createPlot's
# plottype switch: a factor axis with no numeric partner is a bar; one numeric
# axis is a dot plot or a histogram; two numeric axes are scatter, grid, or hex.
available_plot_types <- function(obj) {
    varnames <- attr(obj, "varnames")
    vartypes <- attr(obj, "vartypes")
    if (is.null(varnames) || is.null(vartypes)) {
        return(character())
    }
    xname <- varnames$x
    if (is.null(xname) || !length(xname)) {
        return(character())
    }
    xname <- as.character(xname)[[1L]]
    xtype <- vartypes[[xname]]
    if (is.null(xtype) || !length(xtype)) {
        return(character())
    }
    xfact <- identical(as.character(xtype)[[1L]], "factor")
    yname <- varnames$y
    ynull <- is.null(yname) || !length(yname) || !nzchar(as.character(yname)[[1L]])
    yfact <- FALSE
    if (!ynull) {
        yname <- as.character(yname)[[1L]]
        ytype <- vartypes[[yname]]
        if (is.null(ytype) || !length(ytype)) {
            return(character())
        }
        yfact <- identical(as.character(ytype)[[1L]], "factor")
    }
    xnum <- !xfact
    ynum <- !ynull && !yfact
    types <- character()
    if (xfact && (ynull || yfact)) types <- c(types, "bar")
    if ((xnum && !ynum) || (!xnum && ynum)) types <- c(types, "dot", "hist")
    if (xnum && ynum) types <- c(types, "scatter", "grid", "hex")
    types
}

# Bin count the histogram actually used, including R's default.
hist_bin_count <- function(obj) {
    type <- attr(obj, "plottype")
    type <- if (is.null(type) || !length(type)) "" else as.character(type[[1L]])
    if (!identical(type, "hist")) {
        return(NULL)
    }
    nbins <- attr(obj, "nbins")
    if (is.null(nbins) || !length(nbins) || is.na(nbins[[1L]])) {
        return(NULL)
    }
    as.integer(nbins[[1L]])
}

panel_payload <- function(panel, ctx) {
    cls <- class(panel)[[1L]]
    switch(cls,
        inzbar = panel_bar(panel, ctx),
        inzdot = panel_dot(panel, ctx, hist = FALSE),
        inzhist = panel_dot(panel, ctx, hist = TRUE),
        inzscatter = panel_scatter(panel, ctx),
        inzhex = panel_hex(panel),
        inzgrid = panel_grid(panel, ctx),
        stop("No plot payload for class ", cls, call. = FALSE)
    )
}

panel_bar <- function(panel, ctx) {
    tab <- panel$tab
    phat <- panel$phat
    labels <- colnames(tab)
    if (is.null(labels)) labels <- as.character(seq_len(ncol(tab)))
    labels <- as.character(labels)

    if (!is.null(panel$p.colby)) {
        return(list(
            total = as.numeric(panel$ntotal),
            data = bar_frame(labels, as.numeric(tab), as.numeric(phat)),
            segments = bar_segment_frame(panel, labels)
        ))
    }

    if (isTRUE(ctx$has_y)) {
        series <- rownames(tab)
        if (is.null(series)) series <- as.character(seq_len(nrow(tab)))
        series <- as.character(series)
        totals <- panel$series.totals
        if (is.null(totals)) totals <- rowSums(tab)
        total_labels <- names(totals)
        if (is.null(total_labels) || !length(total_labels)) {
            total_labels <- series
        }
        total_labels <- ifelse(
            is.na(total_labels) | !nzchar(total_labels),
            series,
            total_labels
        )
        ## Column-major: one cluster, then each series inside it.
        return(list(
            total = as.numeric(panel$ntotal),
            seriesTotals = mark_frame(list(
                label = as.character(total_labels),
                total = as.numeric(totals)
            )),
            data = mark_frame(list(
                label = rep(labels, each = length(series)),
                series = rep(series, times = length(labels)),
                count = as.numeric(tab),
                proportion = as.numeric(phat)
            ))
        ))
    }

    list(
        total = as.numeric(panel$ntotal),
        data = bar_frame(labels, as.numeric(tab), as.numeric(phat))
    )
}

bar_frame <- function(label, count, proportion) {
    mark_frame(list(
        label = as.character(label),
        count = as.numeric(count),
        proportion = as.numeric(proportion)
    ))
}

bar_segment_frame <- function(panel, labels) {
    counts <- panel$colby.tab
    if (is.null(counts)) {
        stop("segmented bar is missing colby counts")
    }
    props <- panel$p.colby
    ## One row or one column drops to a vector in `p.colby`.
    if (is.null(dim(props))) {
        props <- matrix(unname(props), nrow = nrow(counts), ncol = ncol(counts))
    }
    props <- props[rev(seq_len(nrow(props))), , drop = FALSE]
    ww <- panel$zoom.index
    if (!is.null(ww)) {
        counts <- counts[, ww, drop = FALSE]
        props <- props[, ww, drop = FALSE]
    }
    seg_labels <- rownames(counts)
    if (is.null(seg_labels)) seg_labels <- rownames(props)
    if (is.null(seg_labels)) seg_labels <- as.character(seq_len(nrow(counts)))
    seg_labels <- as.character(seg_labels)
    n_seg <- length(seg_labels)
    n_bar <- ncol(counts)
    if (is.null(n_bar) || !length(n_bar)) n_bar <- 0L
    bar_labels <- as.character(labels)
    if (length(bar_labels) != n_bar) {
        bar_labels <- if (n_bar) colnames(counts) else character()
        if (is.null(bar_labels)) bar_labels <- as.character(seq_len(n_bar))
    }

    mark_frame(list(
        label = rep(bar_labels, each = n_seg),
        segment = rep(seg_labels, times = n_bar),
        count = as.numeric(counts),
        proportion = as.numeric(props)
    ))
}

panel_dot <- function(panel, ctx, hist) {
    toplot <- panel$toplot
    if (is.null(toplot)) toplot <- list()
    nms <- names(toplot)
    if (is.null(nms)) nms <- as.character(seq_along(toplot))

    edges <- NULL
    if (hist) {
        for (tp in toplot) {
            if (!is.null(tp$breaks)) {
                edges <- as.numeric(tp$breaks)
                break
            }
        }
        if (is.null(edges)) edges <- numeric()
    }
    nbin <- max(length(edges) - 1L, 0L)

    groups <- lapply(seq_along(toplot), function(i) {
        tp <- toplot[[i]]
        g <- list()
        if (isTRUE(ctx$has_y)) g$label <- nms[[i]]
        if (hist) {
            g$counts <- if (is.null(tp)) {
                rep(0, nbin)
            } else {
                as.numeric(tp$counts)
            }
        } else {
            xs <- if (is.null(tp)) numeric() else as.numeric(tp$x)
            g$points <- mark_frame(list(x = xs))
        }
        g$boxplot <- box_payload(group_info(panel$boxinfo, i))
        g$mean <- mean_payload(group_info(panel$meaninfo, i))
        omit_null(g)
    })

    if (hist) list(edges = edges, groups = groups) else list(groups = groups)
}

box_payload <- function(box) {
    if (is.null(box)) {
        return(NULL)
    }
    q <- as.numeric(box$quantiles)
    list(
        min = unname(as.numeric(box$min)),
        q1 = unname(q[[1L]]),
        median = unname(q[[2L]]),
        q3 = unname(q[[3L]]),
        max = unname(as.numeric(box$max))
    )
}

mean_payload <- function(info) {
    if (is.null(info)) {
        return(NULL)
    }
    m <- info$mean
    if (inherits(m, "svystat")) m <- stats::coef(m)
    unname(as.numeric(m)[[1L]])
}

panel_scatter <- function(panel, ctx) {
    n <- length(panel$x)
    cols <- list(
        id = if (n) as.integer(panel$point.order) else integer(),
        x = if (n) as.numeric(panel$x) else numeric(),
        y = if (n) as.numeric(panel$y) else numeric()
    )
    if (n && !is.null(panel$propsize)) {
        sizes <- as.numeric(panel$propsize) / ctx$cex.pt
        if (length(unique(sizes[!is.na(sizes)])) > 1L) cols$size <- sizes
    }
    if (n && isTRUE(ctx$symbolby)) cols$symbol <- as.integer(panel$pch)
    if (n && !is.null(panel$colby)) cols$colby <- col_column(panel$colby)
    if (n && !is.null(panel$highlight)) {
        hl <- as.logical(panel$highlight)
        if (any(hl, na.rm = TRUE)) cols$highlight <- hl
    }
    list(data = mark_frame(cols))
}

panel_hex <- function(panel) {
    hb <- panel$hex
    if (is.null(hb)) {
        return(list(
            xBounds = unname(as.numeric(panel$xlim)),
            yBounds = unname(as.numeric(panel$ylim)),
            xBins = as.integer(panel$n.bins),
            shape = 1,
            data = hex_frame()
        ))
    }
    xy <- hcell2xy(hb)
    list(
        xBounds = unname(as.numeric(hb@xbnds)),
        yBounds = unname(as.numeric(hb@ybnds)),
        xBins = as.integer(hb@xbins),
        shape = as.numeric(hb@shape),
        data = hex_frame(
            x = as.numeric(xy$x),
            y = as.numeric(xy$y),
            count = as.numeric(hb@count),
            meanX = as.numeric(hb@xcm),
            meanY = as.numeric(hb@ycm)
        )
    )
}

panel_grid <- function(panel, ctx) {
    n <- ctx$grid_n
    xb <- ctx$x_bounds
    yb <- ctx$y_bounds
    if (!length(n) || !n || length(xb) < 2L || length(yb) < 2L) {
        return(list(
            xBounds = xb,
            yBounds = yb,
            n = as.integer(n),
            data = grid_frame()
        ))
    }
    xbrk <- seq(xb[[1L]], xb[[2L]], length.out = n + 1L)
    ybrk <- seq(yb[[1L]], yb[[2L]], length.out = n + 1L)
    xi <- as.integer(cut(panel$x, xbrk, include.lowest = TRUE))
    yi <- as.integer(cut(panel$y, ybrk, include.lowest = TRUE))
    ok <- !is.na(xi) & !is.na(yi)
    counts <- matrix(0, nrow = n, ncol = n)
    if (any(ok)) {
        tab <- table(
            factor(xi[ok], levels = seq_len(n)),
            factor(yi[ok], levels = seq_len(n))
        )
        counts[] <- as.numeric(tab)
    }
    ## x bin, then y bin. `which` walks the matrix column-major.
    idx <- which(counts > 0, arr.ind = TRUE)
    if (nrow(idx)) {
        idx <- idx[order(idx[, 1L], idx[, 2L]), , drop = FALSE]
    }
    i <- idx[, 1L]
    j <- idx[, 2L]
    list(
        xBounds = xb,
        yBounds = yb,
        n = n,
        data = grid_frame(
            x0 = xbrk[i],
            x1 = xbrk[i + 1L],
            y0 = ybrk[j],
            y1 = ybrk[j + 1L],
            count = as.numeric(counts[idx])
        )
    )
}

grid_n <- function(obj) {
    bins <- obj$gen$opts$scatter.grid.bins
    if (is.null(bins) || !length(bins) || !is.finite(bins[[1L]])) {
        return(0L)
    }
    min(250L, as.integer(round(bins[[1L]])))
}

point_cex <- function(obj) {
    cex <- obj$gen$opts$cex.pt
    if (is.null(cex) || !length(cex) || !is.finite(cex[[1L]]) || cex[[1L]] == 0) {
        return(1)
    }
    cex[[1L]]
}

col_column <- function(x) {
    if (is.factor(x)) x else if (is.character(x)) as.character(x) else as.numeric(x)
}

mark_frame <- function(cols) {
    as.data.frame(cols, stringsAsFactors = FALSE, optional = TRUE)
}

hex_frame <- function(x = numeric(), y = numeric(), count = numeric(),
                      meanX = numeric(), meanY = numeric()) {
    mark_frame(list(x = x, y = y, count = count, meanX = meanX, meanY = meanY))
}

grid_frame <- function(x0 = numeric(), x1 = numeric(), y0 = numeric(),
                       y1 = numeric(), count = numeric()) {
    mark_frame(list(x0 = x0, x1 = x1, y0 = y0, y1 = y1, count = count))
}

scalar_chr <- function(x) {
    if (is.null(x) || !length(x)) NULL else as.character(x[[1L]])
}

## NULL g2.level means no second split. Only an explicit matrix request counts.
level_requests_matrix <- function(level) {
    if (is.null(level)) {
        return(FALSE)
    }
    any(as.character(level) == "_MULTI")
}

group_info <- function(info, i) {
    if (is.null(info)) NULL else info[[i]]
}

omit_null <- function(x) {
    x[!vapply(x, is.null, logical(1L), USE.NAMES = FALSE)]
}
