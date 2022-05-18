#' @importFrom rlang .data
multiplot <- function(df, args) {
    check_suggested_packages()

    # what type of plot?
    d <- df$data

    # do things for Y here, once enabled:
    if ("y" %in% names(d)) {
        stop("multi y plot not yet supported")
    }

    if (tibble::is_tibble(d$x) && ncol(d$x) > 1L) {
        # plotting multiple x-variables
        vtypes <- sapply(d$x, iNZightTools::vartype)

        if (all(vtypes == "cat")) {
            # unique values ?

            return(multiplot_cat(df, args))
        }

        if (all(vtypes == "num")) {
            return(multiplot_num(df, args))
        }
    }

    "Not supported"
}

multiplot_cat <- function(df, args) {
    xvars <- names(df$data$x) <- paste("x", names(df$data$x), sep = "_")
    d <- tidyr::unnest(df$data, seq_len(ncol(df$data)))

    olvls <- levels(d[[1]])

    ## need to remove labels ...
    levels <- sapply(names(d), function(x) expss::var_lab(d[[x]]) %||% x)
    d <- tibble::as_tibble(lapply(d, as.character))

    facet <- ""
    if ("g1" %in% names(d)) facet <- "g1"

    # mutate X's
    d <- tidyr::pivot_longer(d,
        cols = xvars,
        names_to = "x",
        values_to = "value"
    )

    d <- dplyr::mutate(d, x = factor(.data$x, levels = xvars, labels = levels[xvars]))
    # d <- dplyr::mutate(d, x = stringr::str_replace(.data$x, "^x_", ""))

    if (is.null(args$keep_missing)) args$keep_missing <- FALSE

    if (args$keep_missing) {
        d <- dplyr::mutate(d, value = ifelse(is.na(.data$value), "Missing", .data$value))
    } else {
        d <- dplyr::filter(d, !is.na(.data$value))
    }
    lvls <- unique(c(olvls, unique(d$value)))
    if ("Missing" %in% lvls) lvls <- c(lvls[lvls != "Missing"], "Missing")
    d$value <- factor(d$value, levels = lvls)

    if (facet == "g1") {
        d <- dplyr::mutate(d, g1 = ifelse(is.na(.data$g1), "missing", .data$g1))
        d <- dplyr::group_by(d, .data$x, .data$value, .data$g1, .drop = FALSE)
        d <- dplyr::tally(d)
        d <- dplyr::group_by(d, .data$x, .data$g1, .drop = FALSE)
    } else {
        d <- dplyr::group_by(d, .data$x, .data$value, .drop = FALSE)
        d <- dplyr::tally(d)
        d <- dplyr::group_by(d, .data$x, .drop = FALSE)
    }
    d <- dplyr::summarise(d,
        value = .data$value,
        n = .data$n,
        p = .data$n / sum(.data$n) * 100
    )

    # figure out the name of X
    d$x <- factor(d$x, levels = levels[xvars])
    xvar <- setNames(iNZightMR::substrsplit(levels(d$x)), c("name", "levels"))
    title <- xlab <- ""
    if (xvar$name != "" && all(xvar$levels != "")) {
        newlvls <- setNames(levels(d$x), stringr::str_wrap(xvar$levels, width = 30))
        d$x <- forcats::fct_recode(d$x, !!!newlvls)
        title <- xvar$name
    }

    plottype <- args$plottype
    if (is.null(plottype) || plottype %in% c("default", "gg_multi")) {
        # binary answers?
        if (length(levels(d$value)) == 2L) plottype <- "gg_multi_binary"
        else plottype <- "gg_multi_col"
    }

    ylab <- "Percentage (%)"

    p <- switch(plottype,
        "gg_multi_binary" = {
            if (length(levels(d$value)) != 2L) stop("Invalid plot type")
            xlvls <- levels(d$value)
            xlevel <- if ("yes" %in% tolower(xlvls)) xlvls[tolower(xlvls) == "yes"] else unique(xlvls)[1]
            d <- dplyr::filter(d, .data$value == !!xlevel)

            if (!is.null(args$order) && args$order %in% c("desc", "asc"))
                d$x <- factor(d$x, levels = unique(d$x)[order(d$p, decreasing = args$order == "desc")])

            ylab <- sprintf("%s of responses = '%s'", ylab, xlevel)

            ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$p)) +
                ggplot2::geom_bar(stat = "identity", fill = "#18afe3")
        },
        "gg_multi_stack" = {
            ggplot2::ggplot(d,
                ggplot2::aes(.data$x, .data$p, fill = .data$value)
            ) +
                ggplot2::geom_bar(
                    stat = "identity",
                    position = ggplot2::position_stack(reverse = TRUE)
                )
        },
        "gg_multi_col" = {
            ggplot2::ggplot(d,
                ggplot2::aes(.data$value, .data$p, fill = .data$x)
            ) +
                ggplot2::geom_bar(stat = "identity", position = "dodge")
        }
    )

    subtitle <- ""

    if (facet == "g1") {
        p <- p + ggplot2::facet_wrap(~g1)
        subtitle <- sprintf("Faceted by %s",
            df$labels$g1 %||% df$varnames$g1
        )
    }

    if (!is.null(args$gg_theme)) {
        if (is.character(args$gg_theme)) {
            ptheme <- try(
                eval(parse(text = sprintf("ggplot2::theme_%s", args$gg_theme))),
                silent = TRUE
            )
            if (!inherits(ptheme, "try-error")) {
                p <- p + ptheme()
            } else {
                ptheme <- try(
                    eval(parse(text = sprintf("ggthemes::theme_%s", args$gg_theme))),
                    silent = TRUE
                )
                if (!inherits(ptheme, "try-error")) {
                    p <- p + ptheme()
                }
            }
        } else {
            p <- p + do.call(ggplot2::theme, args$gg_theme)
        }
    } else {
        p <- p + ggplot2::theme_classic()
    }

    if (is.null(args$bg)) args$bg <- "lightgray"

    p <- p +
        ggplot2::xlab(xlab) +
        ggplot2::ylab(ylab) +
        ggplot2::labs(fill = "") +
        ggplot2::ggtitle(title, subtitle = subtitle) +
        ggplot2::theme(
            legend.position = "bottom",
            panel.grid.major.x = ggplot2::element_line(
                colour = "#cccccc70",
                size = 0.4
            )
            # panel.background = ggplot2::element_rect(fill = args$bg)
        )

    if (!is.null(args$rotation) && args$rotation) {
        if (plottype %in% c("gg_multi_column"))
            p <- p + ggplot2::coord_flip()
    } else if (plottype %in% c("gg_multi_binary", "gg_multi_stack"))
        p <- p + ggplot2::coord_flip() +
            ggplot2::scale_x_discrete(limits = rev(levels(d$x)))

    if (is.null(args$plot) || isTRUE(args$plot)) {
        dev.hold()
        tryCatch(
            print(p),
            finally = dev.flush()
        )
    }

    attr(p, "plottype") <- plottype
    attr(p, "varnames") <- list(
        x = strsplit(as.character(df$varnames["x"]), " + ", fixed = TRUE)[[1]]
    )
    attr(p, "xlevels") <- levels(d$value)
    attr(p, "data") <- d
    attr(p, "labels") <- list(
        title = title,
        subtitle = subtitle,
        xlab = xlab,
        ylab = ylab
    )
    attr(p, "n") <- nrow(df$data)
    attr(p, "args") <- args

    class(p) <- c(plottype, "inzmulti_gg", class(p))

    invisible(p)
}

multiplot_num <- function(df, args) {

}

check_suggested_packages <- function() {
    pkgs <- c("ggplot2", "ggthemes", "dplyr", "tibble", "tidyr")
    inst <- sapply(pkgs, requireNamespace, quietly = TRUE)
    if (any(!inst)) {
        stop("Please install suggested packages: install.packages('iNZightPlots', dependencies = TRUE)")
    }
}


summary.inzmulti_gg <- function(object, ...) {
    d <- attr(object, "data", exact = TRUE)
    d
}

summary.gg_multi_binary <- function(object, html = FALSE, ...) {
    args <- modifyList(inzpar(), attr(object, "args"))
    varnames <- attr(object, "varnames", exact = TRUE)
    labels <- attr(object, "labels", exact = TRUE)
    d <- attr(object, "data", exact = TRUE)

    if ("g1" %in% colnames(d)) {
        smry <- tidyr::pivot_wider(d,
            names_from = g1,
            values_from = c(n, p)
        )
        smry <- dplyr::select(smry, -value)
        cn <- colnames(smry)[-1]
        cn <- sapply(cn, function(x)
            paste0(paste(rev(strsplit(x, "_")[[1]]), collapse = " ("), ")")
        )
        smry <- setNames(smry, c("", cn))
        smry <- smry[, c(1, order(cn) + 1L)]
        colnames(smry) <- gsub("(p)", "(%)", colnames(smry), fixed = TRUE)
        pcols <- grepl("(%)", colnames(smry), fixed = TRUE)
        digits <- ifelse(pcols, args$round_percent, 0)
    } else {
        lvls <- c(levels(d$x), "Total")
        d <- tibble::add_row(
            dplyr::ungroup(d),
            x = "Total",
            n = attr(object, "n", exact = TRUE)
        )
        d$x <- factor(d$x, levels = lvls)
        smry <- setNames(dplyr::select(d, x, n, p), c(" ", "N", "%"))
        digits <- c(0, args$round_percent, 0)
    }

    smry <- smry[order(smry[[1]]), ]
    smry[[1]] <- stringr::str_replace(as.character(smry[[1]]), "\n", " ")

    if (!requireNamespace("knitr", quietly = TRUE)) return(smry)

    res <- knitr::kable(smry,
        format = ifelse(html, "html", "simple"),
        caption = labels$title,
        digits = digits
    )

    if (!html) return(res)

    if (is.null(args$kable_styling))
        args$kable_styling <- list(bootstrap_options = NULL)

    if (!requireNamespace("kableExtra", quietly = TRUE)) {
        tf <- tempfile(fileext = ".html")
        writeLines(res, tf)
        browseURL(tf)
        return(invisible(tf))
    }

    kableExtra::kable_classic(res, full_width = FALSE)
}

summary.gg_multi_col <- function(object, html = FALSE, ...) {
    args <- modifyList(inzpar(), attr(object, "args"))
    varnames <- attr(object, "varnames", exact = TRUE)
    labels <- attr(object, "labels", exact = TRUE)
    d <- attr(object, "data", exact = TRUE)

    if ("g1" %in% colnames(d)) {
        smry <- tidyr::pivot_wider(d,
            names_from = g1,
            values_from = c(n, p)
        )
        smry$x <- ifelse(seq_along(smry$x) %% 2 == 0, "", smry$x)
        cn <- colnames(smry)[-(1:2)]
        cn <- sapply(cn, function(x)
            paste0(paste(rev(strsplit(x, "_")[[1]]), collapse = " ("), ")")
        )
        smry <- setNames(smry, c("", "", cn))
        smry <- smry[, c(1:2, order(cn) + 2L)]
        colnames(smry) <- gsub("(p)", "(%)", colnames(smry), fixed = TRUE)
        pcols <- grepl("(%)", colnames(smry), fixed = TRUE)
        digits <- ifelse(pcols, args$round_percent, 0)
    } else {
        d$x <- ifelse(seq_along(d$x) %% 2 == 0, "", d$x)
        smry <- setNames(dplyr::select(d, x, value, n, p), c("", "", "N", "%"))
        digits <- c(0, args$round_percent, 0)
    }

    if (!requireNamespace("knitr", quietly = TRUE)) return(smry)

    res <- knitr::kable(smry,
        format = ifelse(html, "html", "simple"),
        caption = labels$title,
        digits = digits
    )

    if (!html) return(res)

    if (is.null(args$kable_styling))
        args$kable_styling <- list(bootstrap_options = NULL)

    if (!requireNamespace("kableExtra", quietly = TRUE)) {
        tf <- tempfile(fileext = ".html")
        writeLines(res, tf)
        browseURL(tf)
        return(invisible(tf))
    }

    kableExtra::kable_classic(res, full_width = FALSE)
}
