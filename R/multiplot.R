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

    olvls <- levels(df$data[[1]])

    # mutate X's
    d <- tidyr::pivot_longer(d,
        cols = xvars,
        names_to = "x",
        values_to = "value"
    )
    d <- dplyr::mutate(d, x = stringr::str_replace(.data$x, "^x_", ""))
    d <- dplyr::group_by(d, .data$x, .data$value)
    d <- dplyr::tally(d)
    d <- dplyr::group_by(d, .data$x)
    d <- dplyr::summarise(d,
        value = .data$value,
        n = .data$n,
        p = .data$n / sum(.data$n) * 100
    )

    plottype <- args$plottype
    if (is.null(plottype)) {
        # binary answers?
        if (length(levels(d$value)) == 2L) plottype <- "vertical-binary"
        else plottype <- "default"
    }

    p <- switch(plottype,
        "vertical-binary" = {
            xlvls <- unique(d$value)
            xlevel <- unique(xlvls)[1]
            if ("yes" %in% tolower(xlvls)) xlevel[tolower(xlevel) == "yes"]
            d <- dplyr::filter(d, .data$value == !!xlevel)

            d$x <- factor(d$x, levels = unique(d$x)[order(d$p)])

            ggplot2::ggplot(d, ggplot2::aes(.data$x, .data$p)) +
                ggplot2::geom_bar(stat = "identity", fill = "#18afe3") +
                ggplot2::coord_flip()
        },
        "vertical-stack" = {
            ggplot2::ggplot(d,
                ggplot2::aes(.data$x, .data$p, fill = .data$value)
            ) +
                ggplot2::geom_bar(stat = "identity") +
                ggplot2::coord_flip()
        },
        "side-by-side" = ,
        "default" = {
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
        ggplot2::xlab("") +
        ggplot2::ylab("Percentage (%)") +
        ggplot2::theme(
            legend.position = "bottom",
            panel.grid.major.x = ggplot2::element_line(
                colour = "#cccccc70",
                size = 0.4
            )
        )

    p

}

multiplot_num <- function(df, args) {

}

check_suggested_packages <- function() {
    pkgs <- c("ggplot2", "ggthemes", "dplyr", "tibble", "tidyr")
    inst <- sapply(pkgs, requireNamespace(pkgs, quietly = TRUE))
    if (any(!inst)) {
        stop("Please install suggested packages: install.packages('iNZightPlots', dependencies = TRUE)")
    }
}
