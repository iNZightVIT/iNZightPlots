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
    levels <- sapply(names(d), function(x) expss::var_lab(x) %||% x)
    d <- tibble::as_tibble(lapply(d, as.character))

    # mutate X's
    d <- tidyr::pivot_longer(d,
        cols = xvars,
        names_to = "x",
        values_to = "value"
    )

    d <- dplyr::mutate(d, x = factor(.data$x, levels = xvars, labels = levels))
    d <- dplyr::mutate(d, x = stringr::str_replace(.data$x, "^x_", ""))

    if (is.null(args$keep_missing)) args$keep_missing <- FALSE

    if (args$keep_missing) {
        d <- dplyr::mutate(d, value = ifelse(is.na(.data$value), "Missing", .data$value))
    } else {
        d <- dplyr::filter(d, !is.na(.data$value))
    }
    lvls <- unique(c(olvls, unique(d$value)))
    if ("Missing" %in% lvls) lvls <- c(lvls[lvls != "Missing"], "Missing")
    d$value <- factor(d$value, levels = lvls)

    d <- dplyr::group_by(d, .data$x, .data$value, .drop = FALSE)
    d <- dplyr::tally(d)
    d <- dplyr::group_by(d, .data$x)
    d <- dplyr::summarise(d,
        value = .data$value,
        n = .data$n,
        p = .data$n / sum(.data$n) * 100
    )

    # figure out the name of X
    xvar <- setNames(iNZightMR::substrsplit(unique(d$x)), c("name", "levels"))
    xlab <- ""
    if (xvar$name != "" && all(xvar$levels != "")) {
        newlvls <- setNames(unique(d$x), xvar$levels)
        d$x <- forcats::fct_recode(d$x, !!!newlvls)
        xlab <- xvar$name
    }

    plottype <- args$plottype
    if (is.null(plottype) || plottype %in% c("default", "gg_multi")) {
        # binary answers?
        if (length(levels(d$value)) == 2L) plottype <- "gg_multi_binary"
        else plottype <- "gg_multi_col"
    }

    p <- switch(plottype,
        "gg_multi_binary" = {
            if (length(levels(d$value)) != 2L) stop("Invalid plot type")
            xlvls <- unique(d$value)
            xlevel <- unique(xlvls)[1]
            if ("yes" %in% tolower(xlvls)) xlevel[tolower(xlevel) == "yes"]
            d <- dplyr::filter(d, .data$value == !!xlevel)

            d$x <- factor(d$x, levels = unique(d$x)[order(d$p)])

            ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$p)) +
                ggplot2::geom_bar(stat = "identity", fill = "#18afe3") +
                ggplot2::coord_flip()
        },
        "gg_multi_stack" = {
            ggplot2::ggplot(d,
                ggplot2::aes(.data$x, .data$p, fill = .data$value)
            ) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::coord_flip()
        },
        "gg_multi_col" = {
            ggplot2::ggplot(d,
                ggplot2::aes(.data$value, .data$p, fill = .data$x)
            ) +
                ggplot2::geom_bar(stat = "identity", position = "dodge")
        }
    )

    if (!is.null(args$theme)) {
        if (is.character(args$theme)) {
            ptheme <- try(
                eval(parse(text = sprintf("ggplot2::theme_%s", args$theme))),
                silent = TRUE
            )
            if (!inherits(ptheme, "try-error")) {
                p <- p + ptheme()
            } else {
                ptheme <- try(
                    eval(parse(text = sprintf("ggthemes::theme_%s", args$theme))),
                    silent = TRUE
                )
                if (!inherits(ptheme, "try-error")) {
                    p <- p + ptheme()
                }
            }
        } else {
            p <- p + do.call(ggplot2::theme, args$theme)
        }
    } else {
        p <- p + ggplot2::theme_classic()
    }

    p <- p +
        ggplot2::xlab(xlab) +
        ggplot2::ylab("Percentage (%)") +
        ggplot2::theme(
            legend.position = "bottom",
            panel.grid.major.x = ggplot2::element_line(
                colour = "#cccccc70",
                size = 0.4
            )
        )

    dev.hold()
    tryCatch(
        print(p),
        finally = dev.flush()
    )

    attr(p, "plottype") <- plottype
    attr(p, "varnames") <- list(
        x = strsplit(as.character(df$varnames["x"]), " + ", fixed = TRUE)[[1]]
    )

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
