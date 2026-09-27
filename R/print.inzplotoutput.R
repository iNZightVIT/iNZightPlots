#' Print an iNZight plot object
#'
#' Draws an \code{inzplotoutput} created by \code{\link{iNZightPlot}}.
#' Called automatically when \code{plot = TRUE}; call explicitly to redraw.
#'
#' @param x an \code{inzplotoutput} object
#' @param ... additional arguments (ignored)
#' @return \code{x}, invisibly
#' @export
print.inzplotoutput <- function(x, ...) {
    ctx <- attr(x, "._print_ctx")
    if (is.null(ctx)) {
        stop(
            "Cannot print this inzplotoutput: missing draw context. ",
            "Recreate the plot with iNZightPlot()."
        )
    }

    ## Unpack create-time context into locals expected by the draw path
    ## Shallow copy so draw-time opts mutations do not corrupt ctx
    opts <- as.list(ctx$opts)
    df <- ctx$df
    varnames <- ctx$varnames
    vartypes <- ctx$vartypes
    missing <- ctx$missing
    g1.level <- ctx$g1.level
    g2.level <- ctx$g2.level
    matrix.plot <- ctx$matrix.plot
    df.vs <- ctx$df.vs
    xfact <- ctx$xfact
    yfact <- ctx$yfact
    ynull <- ctx$ynull
    TYPE <- ctx$TYPE
    barplot <- ctx$barplot
    xlim <- ctx$xlim
    ylim <- ctx$ylim
    xlim.raw <- ctx$xlim.raw
    ylim.raw <- ctx$ylim.raw
    maxcnt <- ctx$maxcnt
    nOutofview <- ctx$nOutofview
    xaxis <- ctx$xaxis
    yaxis <- ctx$yaxis
    layout.only <- ctx$layout.only
    hide.legend <- ctx$hide.legend
    show_units <- ctx$show_units
    missing.info <- ctx$missing.info
    locate.col <- ctx$locate.col
    zoombars <- ctx$zoombars
    dots <- ctx$dots
    xlab <- ctx$xlab
    ylab <- ctx$ylab
    xlab_was_missing <- ctx$xlab_was_missing
    ylab_was_missing <- ctx$ylab_was_missing
    total.missing <- ctx$total.missing
    BARPLOT.N <- ctx$BARPLOT.N
    itsADotplot <- ctx$itsADotplot
    xattr <- ctx$xattr

    ## Prefer unfiltered panel snapshot on redraw; else strip gen/xlim/ylim
    if (!is.null(ctx$panels_unfiltered)) {
        plot.list <- ctx$panels_unfiltered
        g1.level <- ctx$g1.level_orig
        g2.level <- ctx$g2.level_orig
    } else {
        plot.list <- x
        plot.list$gen <- NULL
        plot.list$xlim <- NULL
        plot.list$ylim <- NULL
    }

    ## Snapshot for a later print() before this call filters panels
    panels_unfiltered <- plot.list
    g1.level_orig <- g1.level
    g2.level_orig <- g2.level

    ## Preserve create-time attributes across lapply filtering below
    .keep_attrs <- attributes(x)
    .keep_attrs <- .keep_attrs[setdiff(names(.keep_attrs), c("names", "class"))]

    ## Device / container: skip if iNZightPlot already opened one for create
    if (isTRUE(ctx$already_open)) {
        ## expect to already be inside the "container" viewport
    } else {
        dd <- dev.flush(dev.flush())
        dev.hold()
        grid.newpage()
        pushViewport(
            viewport(
                gp = gpar(cex = opts$cex),
                name = "container"
            )
        )
    }

    # essentially the height of the window
    PAGE.height <- convertHeight(current.viewport()$height, "in", TRUE)

    ## --- there will be some fancy stuff here designing and implementing
    ## a grid which adds titles, labels, and optionally legends

    ## --- first, need to make all of the labels/legends/etc:
    VT <- vartypes
    names(VT) <- names(varnames)

    if (all(c("x", "y") %in% names(VT))) {
        ## switch X/Y for dotplots

        if (VT$y == "numeric" & VT$x == "factor") {
            xn <- varnames$y
            varnames$y <- varnames$x
            varnames$x <- xn
            VT$x <- "numeric"
            VT$y <- "factor"

            my <- missing$y
            missing$y <- missing$x
            missing$x <- my

            # swap labels, if they exist
            l_x <- df$labels$x
            l_y <- df$labels$y
            df$labels$x <- l_y
            df$labels$y <- l_x
        }
    }
    if (xlab_was_missing || is.null(xlab)) {
        xlab <- df$labels$x %||% varnames$x
    }
    if (ylab_was_missing || is.null(ylab)) {
        ylab <- df$labels$y %||% varnames$y
    }

    titles <- list()
    titles$main <-
        if (!is.null(dots$main)) {
            makeTitle(df$labels, VT, g1.level, g2.level,
                template = dots$main
            )
        } else {
            makeTitle(df$labels, VT, g1.level, g2.level)
        }
    titles$xlab <- xlab
    if (!ynull) {
        titles$ylab <-
            if (xfact & yfact) {
                ifelse(opts$bar.counts, "Count", "Percentage (%)")
            } else {
                ylab
            }
    } else if (xfact) {
        titles$ylab <- ifelse(opts$bar.counts, "Count", "Percentage (%)")
    }
    if ("colby" %in% df.vs) {
        titles$legend <- df$labels$colby %||% varnames$colby
    }

    if (show_units) {
        titles$xlab <- add_units(titles$xlab, df$units$x)
        titles$ylab <- add_units(titles$ylab, df$units$y)
        titles$colby <- add_units(titles$legend, df$units$colby)
    }

    ## plot.list still contains all the levels of g1 that wont be plotted
    ## - for axis scaling etc
    ## so figure this one out somehow ...
    panel_scale <- inz_panel_scale(plot.list, df, g1.level, g2.level, matrix.plot)
    N <- panel_scale$N
    multi.cex <- panel_scale$mcex


    # --- WIDTHS of various things
    # first we need to know HOW WIDE the main viewport is, and then
    # split the title text into the appropriate number of lines,
    # then calcualate the height of it.
    VPcontainer.width <- convertWidth(unit(1, "npc"), "in", TRUE)
    main.grob <- textGrob(titles$main,
        gp = gpar(cex = opts$cex.main),
        name = "inz-main-title"
    )
    MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
    MAIN.lnheight <- convertWidth(grobHeight(main.grob), "in", TRUE)
    if (MAIN.width > 0.9 * VPcontainer.width) {
        titles$main <- gsub(",", ",\n", titles$main)
        main.grob <- textGrob(titles$main,
            gp = gpar(cex = opts$cex.main),
            name = "inz-main-title"
        )
        MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
    }
    if (MAIN.width > 0.9 * VPcontainer.width) {
        titles$main <- gsub("subset", "\nsubset", titles$main)
        main.grob <- textGrob(titles$main,
            gp = gpar(cex = opts$cex.main),
            name = "inz-main-title"
        )
        MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
    }
    if (MAIN.width > 0.9 * VPcontainer.width) {
        titles$main <- gsub(" (size prop", "\n (size prop",
            titles$main,
            fixed = TRUE
        )
        main.grob <- textGrob(titles$main,
            gp = gpar(cex = opts$cex.main),
            name = "inz-main-title"
        )
        MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
    }
    MAIN.height <- convertHeight(
        grobHeight(main.grob),
        "in",
        TRUE
    ) + MAIN.lnheight

    # -- xaxis labels
    xlab.grob <- textGrob(titles$xlab,
        y = unit(0.6, "lines"),
        gp = gpar(cex = opts$cex.lab),
        name = "inz-xlab"
    )
    XLAB.height <- convertHeight(grobHeight(xlab.grob), "in", TRUE) * 3
    # -- yaxis labels
    if (!is.null(titles$ylab)) {
        ylab.grob <- textGrob(titles$ylab,
            x = unit(0.6, "lines"),
            name = "inz-ylab",
            rot = 90,
            gp = gpar(cex = opts$cex.lab)
        )
        YLAB.width <- convertWidth(grobWidth(ylab.grob), "in", TRUE) * 3
    } else {
        YLAB.width <- 0
    }

    ## -- xaxis marks
    XAX.height <-
        convertWidth(unit(1, "lines"), "in", TRUE) * 2 *
            opts$cex.axis * xaxis

    ## -- yaxis marks
    YAX.default.width <-
        convertWidth(unit(1, "lines"), "in", TRUE) * 2 * opts$cex.axis

    YAX.width <- if (any(TYPE %in% c("dot", "hist")) &
        !ynull & !opts$internal.labels) {
        ## need to grab the factoring variable -> might be x OR y
        yf <- if (is.factor(df$data$y)) df$data$y else df$data$x
        yl <- levels(yf)
        yWidths <- sapply(
            yl,
            function(L) {
                convertWidth(
                    grobWidth(
                        textGrob(L,
                            gp = gpar(cex = opts$cex.axis * multi.cex)
                        )
                    ),
                    "in",
                    TRUE
                )
            }
        )
        max(yWidths)
    } else if (any(TYPE %in% c("scatter", "hex", "grid"))) {
        ax <- transform_axes(df$data$y, "y", opts,
            label = TRUE, adjust.vp = FALSE
        )
        convertWidth(
            grobWidth(
                textGrob(ax$labs,
                    gp = gpar(cex = opts$cex.axis * multi.cex)
                )
            ),
            "in",
            TRUE
        )
    } else {
        0
    }

    YAX.width <- ifelse(yaxis, YAX.width + YAX.default.width, 0.1)

    ## -- legend(s)
    leg.grob1 <- leg.grob2 <- leg.grob3 <- leg.grob4 <- NULL
    cex.mult <- ifelse(
        "g1" %in% df.vs,
        1,
        ifelse(
            "g1.level" %in% df.vs,
            ifelse(
                length(levels(df$g1.level)) >= 6,
                0.7,
                1
            ),
            1
        )
    )


    xnum <- !xfact
    yfact <- if (ynull) FALSE else yfact
    ynum <- if (ynull) FALSE else !yfact

    col.args <- inz_col_args(
        plot.list, opts, df, varnames, TYPE, barplot,
        xfact, yfact, ynull, locate.col
    )
    if ("colby" %in% names(varnames) &&
        (any(TYPE %in% c("dot", "scatter", "hex")) ||
            (any(TYPE %in% c("grid", "hex")) && !is.null(opts$trend) &&
                opts$trend.by) ||
            (any(TYPE == "bar") && ynull && is.factor(df$data$colby)))) {
        if (any(TYPE == "hex")) {
            df$data$colby <- convert.to.factor(df$data$colby)
        }

        if (is.factor(df$data$colby)) {
            nby <- length(levels(as.factor(df$data$colby)))
            if (length(opts$col.pt) >= nby) {
                ptcol <- opts$col.pt[1:nby]
            } else {
                ptcol <-
                    if (!is.null(opts$col.fun)) {
                        opts$col.fun(nby)
                    } else {
                        opts$col.default$cat(nby)
                    }
            }

            if (all(TYPE != "bar")) {
                misscol <- any(
                    sapply(
                        plot.list,
                        function(x) sapply(x, function(y) y$nacol)
                    )
                )
            } else {
                misscol <- FALSE
            }

            legPch <-
                if (barplot) {
                    22
                } else if (!is.null(varnames$symbolby)) {
                    if (varnames$colby == varnames$symbolby) {
                        tmp <- (21:25)[1:length(levels(df$data$symbolby))]
                        if (any(is.na(df$data$symbolby))) {
                            tmp <- c(tmp, 3)
                        }
                        tmp
                    } else {
                        opts$pch
                    }
                } else {
                    opts$pch
                }

            leg.grob1 <- drawLegend(
                f.levels <- levels(as.factor(df$data$colby)),
                col = ptcol,
                pch = legPch,
                title = df$labels$colby %||% varnames$colby,
                any.missing = misscol,
                opts = opts
            )

            if (misscol) {
                ptcol <- c(ptcol, opts$col.missing)
                f.levels <- c(f.levels, "missing")
            }
        } else {
            misscol <- any(
                sapply(
                    plot.list,
                    function(x) sapply(x, function(y) y$nacol)
                )
            )
            leg.grobL <- drawContLegend(
                df$data$colby,
                title = add_units(
                    df$short_labels$colby %||% varnames$colby,
                    df$units$colby
                ),
                height = 0.4 * PAGE.height,
                cex.mult = cex.mult,
                any.missing = misscol,
                opts = opts
            )
            leg.grob1 <- leg.grobL$fg
        }
    } else if (xfact & yfact) {
        nby <- length(levels(as.factor(df$data$y)))
        if (length(opts$col.pt) >= nby) {
            barcol <- opts$col.pt[1:nby]
        } else {
            barcol <-
                if (!is.null(opts$col.fun)) {
                    opts$col.fun(nby)
                } else {
                    opts$col.default$cat(nby)
                }
        }

        leg.grob1 <- drawLegend(
            levels(as.factor(df$data$y)),
            col = barcol, pch = 22,
            title = df$short_labels$y %||% varnames$y,
            opts = opts
        )
    }

    if ("sizeby" %in% names(varnames) & any(TYPE %in% c("scatter"))) {
        misssize <- any(
            sapply(
                plot.list,
                function(x) sapply(x, function(x2) x2$nasize)
            )
        )
        if (misssize) {
            misstext <- paste0("missing ", varnames$sizeby)
            leg.grob2 <- drawLegend(
                misstext,
                col = "grey50",
                pch = 4,
                cex.mult = cex.mult * 0.8,
                opts = opts
            )
        }
    }

    if (xnum & ynum) {
        df.lens <- lapply(
            plot.list,
            function(a) {
                mm <- sapply(
                    a,
                    function(b) {
                        sum(
                            apply(
                                cbind(b$x, b$y), 1,
                                function(c) all(!is.na(c))
                            )
                        )
                    }
                )
                A <- a[[which.max(mm)]]
                cbind(A$x, A$y)
            }
        )
        ddd <- df.lens[[which.max(sapply(df.lens, nrow))]]
        leg.grob3 <- drawLinesLegend(ddd,
            opts = opts,
            cex.mult = cex.mult * 0.8
        )
    }

    if ("symbolby" %in% names(varnames) &
        any(TYPE %in% c("scatter", "dot"))) {
        skip <- FALSE
        if (!is.null(varnames$colby)) {
            if (varnames$colby == varnames$symbolby) {
                skip <- TRUE
            }
        }

        if (!skip) {
            legPch <- (21:25)[1:length(levels(df$data$symbolby))]
            legLvls <- levels(df$data$symbolby)
            if (any(is.na(df$data$symbolby))) {
                legPch <- c(legPch, 3)
                legLvls <- c(legLvls, "missing")
            }
            leg.grob4 <- drawLegend(legLvls,
                col = rep("#333333", length(legLvls)),
                pch = legPch,
                title = varnames$symbolby,
                opts = opts
            )
        }
    }

    hgts <- numeric(4)
    wdth <- 0

    if (hide.legend) {
        leg.grob1 <- leg.grob2 <- leg.grob3 <- leg.grob4 <- NULL
    }
    if (!is.null(leg.grob1)) {
        hgts[1] <- convertHeight(grobHeight(leg.grob1), "in", TRUE)
        wdth <- max(wdth, convertWidth(grobWidth(leg.grob1), "in", TRUE))
    }
    if (!is.null(leg.grob2)) {
        hgts[2] <- convertHeight(grobHeight(leg.grob2), "in", TRUE)
        wdth <- max(wdth, convertWidth(grobWidth(leg.grob2), "in", TRUE))
    }
    if (!is.null(leg.grob3)) {
        hgts[3] <- convertHeight(grobHeight(leg.grob3), "in", TRUE)
        wdth <- max(wdth, convertWidth(grobWidth(leg.grob3), "in", TRUE))
    }
    if (!is.null(leg.grob4)) {
        hgts[4] <- convertHeight(grobHeight(leg.grob4), "in", TRUE)
        wdth <- max(wdth, convertWidth(grobWidth(leg.grob4), "in", TRUE))
    }

    ## --- Figure out a subtitle for the plot:

    if (!is.null(dots$subtitle)) {
        SUB <- textGrob(
            dots$subtitle,
            gp = gpar(cex = opts$cex.text * 0.8),
            name = "inz-main-sub-bottom"
        )
    } else {
        subtitle <- ""
        if (missing.info & length(missing) > 0) {
            POS.missing <- missing[missing != 0]
            names(POS.missing) <- unlist(
                varnames[match(names(POS.missing), names(varnames))]
            )
            missinfo <-
                if (length(missing) > 1) {
                    paste0(
                        " (",
                        paste0(
                            POS.missing,
                            " in ",
                            names(POS.missing),
                            collapse = ", "
                        ),
                        ")"
                    )
                } else {
                    ""
                }

            if (total.missing > 0) {
                subtitle <- paste0(
                    total.missing,
                    " missing values",
                    missinfo
                )
            }
        }

        if (nOutofview > 0) {
            subtitle <- ifelse(subtitle == "", "", paste0(subtitle, " | "))
            subtitle <- paste0(subtitle, nOutofview, " points out of view")
        } else if (!is.null(zoombars)) {
            subtitle <- ifelse(subtitle == "", "", paste0(subtitle, " | "))
            subtitle <- paste0(
                subtitle, zoombars[2], " out of ",
                length(levels(df$data$x)),
                " levels of ", varnames$x, " visible"
            )
        }

        if (any(TYPE == "bar") && !ynull &&
            opts$bar.relative.width && !opts$bar.counts) {
            subtitle <- paste(subtitle,
                sprintf("Bar widths relative to %s counts", varnames$y),
                "",
                sep = ifelse(subtitle == "", "", ". ")
            )
        }

        if (subtitle == "") {
            SUB <- NULL
        } else {
            SUB <- textGrob(
                subtitle,
                gp = gpar(cex = opts$cex.text * 0.8),
                name = "inz-main-sub-bottom"
            )
        }
    }


    ## --- CREATE the main LAYOUT for the titles + main plot window
    MAIN.hgt <- unit(MAIN.height, "in")
    XAX.hgt <- unit(XAX.height, "in")
    XLAB.hgt <- unit(XLAB.height, "in")
    PLOT.hgt <- unit(1, "null")
    SUB.hgt <-
        if (is.null(SUB)) {
            unit(0, "null")
        } else {
            convertUnit(grobHeight(SUB) * 2, "in")
        }

    YLAB.wd <- unit(YLAB.width, "in")
    YAX.wd <- unit(YAX.width, "in")
    PLOT.wd <- unit(1, "null")
    LEG.wd <-
        if (wdth > 0) {
            unit(wdth, "in") + unit(1, "char")
        } else {
            unit(0, "null")
        }

    TOPlayout <- grid.layout(
        nrow = 6, ncol = 5,
        heights = unit.c(
            MAIN.hgt, XAX.hgt, PLOT.hgt,
            XAX.hgt, XLAB.hgt, SUB.hgt
        ),
        widths = unit.c(
            YLAB.wd, YAX.wd, PLOT.wd,
            if (any(TYPE %in% c("scatter", "grid", "hex"))) {
                YAX.wd
            } else {
                unit(0.5, "in")
            }, LEG.wd
        )
    )

    ## Send the layout to the plot window
    pushViewport(viewport(layout = TOPlayout, name = "VP:TOPlayout"))

    ## Sort out XAX height:
    pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 3))
    plotWidth <- convertWidth(current.viewport()$width, "in", TRUE)
    upViewport()

    if (any(TYPE == "bar")) {
        ## If the labels are too wide, we rotate them (and shrink slightly)
        x.lev <- levels(df$data$x)
        nLabs <- length(x.lev)
        maxWd <- 0.8 * plotWidth / nLabs
        rot <- any(
            sapply(
                x.lev,
                function(l) {
                    convertWidth(
                        grobWidth(textGrob(l, gp = gpar(cex = opts$cex.axis))),
                        "in",
                        TRUE
                    ) > maxWd
                }
            )
        )
        opts$rot <- rot

        # transform?
        opts$transform$y <-
            ifelse(opts$bar.counts, "bar_counts", "bar_percentage")
        if (opts$bar.counts) {
            opts$bar.n <- nrow(df$data)
        }

        if (rot) {
            ## Unable to update the viewport, so just recreate it:
            XAXht <- drawAxes(
                df$data$x,
                which = "x",
                main = TRUE,
                label = TRUE, opts,
                heightOnly = TRUE,
                layout.only = layout.only
            )
            XAX.hgt2 <- convertWidth(XAXht, "in")

            ## destroy the old one
            popViewport()
            TOPlayout <- grid.layout(
                nrow = 6,
                ncol = 5,
                heights = unit.c(
                    MAIN.hgt,
                    XAX.hgt,
                    PLOT.hgt,
                    XAX.hgt2,
                    XLAB.hgt,
                    SUB.hgt
                ),
                widths = unit.c(
                    YLAB.wd,
                    YAX.wd,
                    PLOT.wd,
                    YAX.wd,
                    LEG.wd
                )
            )

            ## Send the layout to the plot window
            pushViewport(
                viewport(
                    layout = TOPlayout,
                    name = "VP:TOPlayout"
                )
            )
        }
    }

    ## place the title
    pushViewport(viewport(layout.pos.row = 1))
    grid.draw(main.grob)

    ## place axis labels
    if (!is.null(titles$ylab)) {
        seekViewport("VP:TOPlayout")
        pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
        grid.draw(ylab.grob)
    }
    seekViewport("VP:TOPlayout")
    pushViewport(viewport(layout.pos.row = 5, layout.pos.col = 3))
    grid.draw(xlab.grob)

    ## place the legend
    if (wdth > 0) {
        seekViewport("VP:TOPlayout")
        pushViewport(viewport(layout.pos.col = 5, layout.pos.row = 3))
        leg.layout <- grid.layout(4, heights = unit(hgts, "in"))
        pushViewport(viewport(layout = leg.layout, name = "VP:LEGlayout"))

        if (hgts[1] > 0) {
            seekViewport("VP:LEGlayout")
            pushViewport(viewport(layout.pos.row = 1))
            grid.draw(leg.grob1)
        }
        if (hgts[2] > 0) {
            seekViewport("VP:LEGlayout")
            pushViewport(viewport(layout.pos.row = 2))
            grid.draw(leg.grob2)
        }
        if (hgts[3] > 0) {
            seekViewport("VP:LEGlayout")
            pushViewport(viewport(layout.pos.row = 3))
            grid.draw(leg.grob3)
        }
        if (hgts[4] > 0) {
            seekViewport("VP:LEGlayout")
            pushViewport(viewport(layout.pos.row = 4))
            grid.draw(leg.grob4)
        }
    }

    ## --- next, it will break the plot into subregions for g1
    ## (unless theres only one, then it wont)

    ## break up plot list
    if (any(g2.level == "_MULTI")) g2.level <- names(plot.list)
    if (!matrix.plot & !is.null(g2.level)) {
        plot.list <- plot.list[g2.level]
    }

    plot.list <- lapply(plot.list, function(x) x[g1.level])

    ## and subtitle
    if (!is.null(SUB)) {
        seekViewport("VP:TOPlayout")
        pushViewport(viewport(layout.pos.row = 6, layout.pos.col = 3))
        grid.draw(SUB)
    }

    ## create a layout
    if (matrix.plot) {
        nr <- length(g2.level)
        nc <- length(g1.level)
    } else {
        dim1 <- floor(sqrt(N))
        dim2 <- ceiling(N / dim1)

        if (dev.size()[1] < dev.size()[2]) {
            nr <- dim2
            nc <- dim1
        } else {
            nr <- dim1
            nc <- dim2
        }
    }

    ## if the plots are DOTPLOTS or BARPLOTS, then leave a little bit of
    ## space between each we will need to add a small amount of space
    ## between the columns of the layout
    hspace <- ifelse(any(TYPE %in% c("scatter", "grid", "hex")), 0, 0.01)
    wds <- rep(unit.c(unit(hspace, "npc"), unit(1, "null")), nc)[-1]

    subt <- textGrob(
        "dummy text",
        gp = gpar(cex = opts$cex.lab, fontface = "bold"),
        name = "inz-dummy-txt"
    )
    sub.hgt <- unit(convertHeight(grobHeight(subt), "in", TRUE) * 1.2, "in")
    vspace <- if (matrix.plot) sub.hgt else unit(0, "in")
    hgts <- rep(unit.c(vspace, unit(1, "null")), nr)

    PLOTlayout <- grid.layout(
        nrow = length(hgts),
        ncol = length(wds),
        heights = hgts,
        widths = wds
    )
    seekViewport("VP:TOPlayout")
    pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 3))
    pushViewport(viewport(layout = PLOTlayout, name = "VP:PLOTlayout"))

    ## --- within each of these regions, we simply plot!
    ax.gp <- gpar(cex = opts$cex.axis)

    ## --- START from the BOTTOM and work UP; LEFT and work RIGHT
    ## (mainly makes sense for continuous grouping variables)
    g1id <- 1 # keep track of plot levels
    g2id <- 1
    NG2 <- length(plot.list)
    NG1 <- length(plot.list[[1]])

    if (xfact & ynum) {
        X <- df$data$y
        Y <- df$data$x
    } else {
        X <- df$data$x
        Y <- df$data$y
    }

    for (r in nr:1) {
        R <- r * 2 # skip the gaps between rows
        if (matrix.plot) {
            ## add that little thingy
            seekViewport("VP:PLOTlayout")
            pushViewport(
                viewport(
                    layout.pos.row = R - 1,
                    gp = gpar(cex = multi.cex, fontface = "bold")
                )
            )
            grid.rect(
                gp = gpar(
                    fill = rep(opts$col.sub, length = 2)[2]
                )
            )
            grid.text(
                paste(varnames$g2, "=", g2.level[g2id]),
                gp = gpar(
                    cex = opts$cex.lab,
                    col = "#ffffff",
                    fontface = "bold"
                )
            )
        }

        for (c in 1:nc) {
            ## store row and column number
            opts$rowNum <- r
            opts$colNum <- c

            if (g2id > NG2) next()
            C <- c * 2 - 1

            ## This is necessary to delete the "old" viewport so we can
            ## create a new one of the same name, but retain it long enough
            ## to use it for drawing the axes
            if (any(TYPE %in% c("dot", "hist")) & !layout.only) {
                vp2rm <- try(
                    switch(TYPE,
                        "dot" = {
                            seekViewport("VP:dotplot-levels")
                            popViewport()
                        },
                        "hist" = {
                            seekViewport("VP:histplot-levels")
                            popViewport()
                        }
                    ),
                    silent = TRUE
                )
            }

            seekViewport("VP:PLOTlayout")
            pushViewport(
                viewport(
                    layout.pos.row = R,
                    layout.pos.col = C,
                    xscale = xlim,
                    yscale = ylim,
                    gp = gpar(cex = multi.cex)
                )
            )
            ## grid.rect(gp = gpar(fill = "transparent"))

            subt <- g1.level[g1id]

            ## calculate the height of the subtitle if it is specified
            p.title <- if (subt == "all") NULL else subt
            hgt <- unit.c(
                if (!is.null(p.title)) {
                    subt <- textGrob(
                        p.title,
                        gp = gpar(cex = opts$cex.lab, fontface = "bold"),
                        name = paste("inz-sub", r, c, sep = ".")
                    )
                    if (matrix.plot) {
                        sub.hgt
                    } else {
                        unit(convertHeight(
                            grobHeight(subt), "in", TRUE
                        ) * 2, "in")
                    }
                } else {
                    unit(0, "null")
                },
                unit(1, "null")
            )
            pushViewport(
                viewport(
                    layout = grid.layout(2, 1, heights = hgt)
                )
            )

            ## I found "VP:locate.these.points" so far is just using here
            ## and no other depencies so I think giving the its a
            ## uniqe name would be a good idea here.
            nameVP <-
                if (NG1 == 1 && NG2 == 1) {
                    "VP:locate.these.points"
                } else {
                    paste0("VP:locate.these.points", g2id, g1id)
                }
            pushViewport(
                viewport(
                    layout.pos.row = 2,
                    xscale = xlim,
                    yscale = ylim,
                    clip = "on",
                    name = nameVP
                )
            )

            if (!layout.only) {
                ## background color:
                grid.rect(
                    gp = gpar(fill = opts$bg, lty = 0),
                    name = paste("inz-plot-bg", r, c, sep = ".")
                )
                plot(
                    plot.list[[g2id]][[g1id]],
                    gen = list(
                        opts = opts,
                        mcex = multi.cex,
                        col.args = col.args,
                        maxcount = maxcnt,
                        LIM = c(xlim.raw, ylim.raw)
                    )
                )
            }
            upViewport()

            if (!is.null(p.title)) {
                pushViewport(viewport(layout.pos.row = 1))
                grid.rect(
                    gp = gpar(fill = opts$col.sub[1]),
                    name = paste("inz-sub-bg", r, c, sep = ".")
                )
                grid.draw(subt)
                upViewport()
            }

            grid.rect(
                gp = gpar(fill = "transparent"),
                name = paste("inz-rect-tp", r, c, sep = ".")
            )


            ## add the appropriate axes:
            ## Decide which axes to plot:

            ## -------------
            ## For dotplots + histograms: the axis are at the bottom of
            ## every column, and on the far left
            ##
            ## For scatterplots + gridplots + hexplots: the axis
            ## alternative on both axis, left and right
            ##
            ## For barplot: the axis is on the bottom of every column,
            ## and left and right of every row - also, must rotate
            ## if too big!
            ## ------------


            if (barplot) {
                opts$bar.nmax <- BARPLOT.N[[g2id]][[g1id]]
            }

            pushViewport(
                viewport(
                    layout.pos.row = 2,
                    xscale = xlim,
                    yscale = ylim
                )
            )
            opts$ZOOM <- zoombars
            if (r == nr & xaxis) { # bottom
                drawAxes(
                    X, "x", TRUE,
                    c %% 2 == 1 |
                        !any(TYPE %in% c("scatter", "grid", "hex")),
                    opts,
                    layout.only = layout.only,
                    pos = "bottom"
                )
            }

            if (c == 1 & (!opts$internal.labels |
                !any(TYPE %in% c("dot", "hist"))) & yaxis) { # left column
                drawAxes(
                    if (any(TYPE == "bar")) ylim else Y,
                    "y",
                    TRUE,
                    (nr - r) %% 2 == 0,
                    opts,
                    layout.only = layout.only,
                    pos = "left"
                )
            }

            if (!any(TYPE %in% c("dot", "hist")) & yaxis) {
                # right column (or last plot in top row)
                if (c == nc | g1id == NG1) {
                    drawAxes(
                        if (any(TYPE == "bar")) ylim else Y,
                        "y",
                        FALSE,
                        (nr - r) %% 2 == 1,
                        opts,
                        layout.only = layout.only,
                        pos = "right"
                    )
                }
            }
            upViewport()

            if (any(TYPE %in% c("scatter", "grid", "hex")) & xaxis) {
                pushViewport(
                    viewport(
                        layout.pos.row = 1,
                        xscale = xlim,
                        yscale = ylim
                    )
                )
                if (r == 1) {
                    drawAxes(X, "x", FALSE, c %% 2 == 0,
                        opts,
                        sub = vspace,
                        layout.only = layout.only
                    )
                }
                upViewport()
            }
            opts$ZOOM <- NULL
            opts$rowNum <- NULL
            opts$colNum <- NULL
            opts$bar.nmax <- NULL

            ## update the counters
            if (g1id < NG1) {
                g1id <- g1id + 1
            } else {
                g1id <- 1
                g2id <- g2id + 1
            }
        }
    }

    opts$rot <- NULL

    dev.flush()

    ## Restore create-time attributes dropped by panel filtering
    for (nm in names(.keep_attrs)) {
        attr(plot.list, nm) <- .keep_attrs[[nm]]
    }

    ## Refresh $gen after draw (opts may have changed; mcex/col.args match create)
    plot.list$gen <- list(
        opts = opts,
        mcex = multi.cex,
        col.args = col.args,
        maxcount = maxcnt
    )
    plot.list$xlim <- xlim
    plot.list$ylim <- ylim

    ## Persist updated varnames / missing after any axis swap in draw
    attr(plot.list, "varnames") <- varnames
    attr(plot.list, "missing") <- missing
    attr(plot.list, "nplots") <- if (exists("N", inherits = FALSE)) N else .keep_attrs$nplots

    if (itsADotplot) {
        attr(plot.list, "dotplot.redraw") <-
            round(xattr$symbol.width, 5) !=
                round(convertWidth(unit(opts$cex.dotpt, "char"),
                    "native",
                    valueOnly = TRUE
                ), 5)
    }

    ## Keep ctx so a later print(x) can redraw; reopen device next time.
    ## `[<-` keeps NULL levels; `$<- NULL` would drop the element.
    ctx2 <- ctx
    ctx2$already_open <- FALSE
    ctx2$panels_unfiltered <- panels_unfiltered
    ctx2["g1.level_orig"] <- list(g1.level_orig)
    ctx2["g2.level_orig"] <- list(g2.level_orig)
    attr(plot.list, "._print_ctx") <- ctx2

    class(plot.list) <- "inzplotoutput"
    invisible(plot.list)
}

#' @rdname print.inzplotoutput
#' @param y ignored
#' @exportS3Method graphics::plot
plot.inzplotoutput <- function(x, y, ...) {
    print(x, ...)
}

## Panel count and multi-plot cex. Uses the unfiltered panel list so
## g1/g2 subsets still size text from the full grid.
inz_panel_scale <- function(plot.list, df, g1.level, g2.level, matrix.plot) {
    ng1 <- ifelse("g1" %in% names(df$data), length(g1.level), 1)
    ng2 <- ifelse(
        "g2" %in% names(df$data),
        ifelse(
            matrix.plot,
            ifelse(
                g2.level == "_MULTI",
                length(plot.list),
                length(g2.level)
            ),
            1
        ),
        1
    )
    N <- ng1 * ng2
    NN <- if (matrix.plot) length(plot.list) * length(plot.list[[1]]) else N
    # this has absolutely no theoretical reasoning,
    # it just does a reasonably acceptable job (:
    list(
        N = N,
        mcex = max(1.2 * sqrt(sqrt(NN) / NN), 0.5)
    )
}

## Colour mapping stored on $gen$col.args. Legend grobs are built separately
## in print(); this does not draw.
inz_col_args <- function(plot.list, opts, df, varnames, TYPE, barplot,
                         xfact, yfact, ynull, locate.col) {
    yfact <- if (ynull) FALSE else yfact
    col.args <- list(missing = opts$col.missing)

    if ("colby" %in% names(varnames) &&
        (any(TYPE %in% c("dot", "scatter", "hex")) ||
            (any(TYPE %in% c("grid", "hex")) && !is.null(opts$trend) &&
                opts$trend.by) ||
            (any(TYPE == "bar") && ynull && is.factor(df$data$colby)))) {
        colby <- df$data$colby
        if (any(TYPE == "hex")) {
            colby <- convert.to.factor(colby)
        }

        if (is.factor(colby)) {
            nby <- length(levels(as.factor(colby)))
            if (length(opts$col.pt) >= nby) {
                ptcol <- opts$col.pt[1:nby]
            } else {
                ptcol <-
                    if (!is.null(opts$col.fun)) {
                        opts$col.fun(nby)
                    } else {
                        opts$col.default$cat(nby)
                    }
            }

            if (all(TYPE != "bar")) {
                misscol <- any(
                    sapply(
                        plot.list,
                        function(x) sapply(x, function(y) y$nacol)
                    )
                )
            } else {
                misscol <- FALSE
            }

            f.levels <- levels(as.factor(colby))
            if (misscol) {
                ptcol <- c(ptcol, opts$col.missing)
                f.levels <- c(f.levels, "missing")
            }
            col.args$f.cols <- structure(ptcol, .Names = f.levels)
        } else {
            col.args$n.range <- range(colby, na.rm = TRUE)
            col.args$n.cols <-
                if (!is.null(opts$col.fun)) {
                    opts$col.fun(200)
                } else {
                    opts$col.default$cont(200)
                }
        }
    } else if (xfact & yfact) {
        nby <- length(levels(as.factor(df$data$y)))
        if (length(opts$col.pt) >= nby) {
            barcol <- opts$col.pt[1:nby]
        } else {
            barcol <-
                if (!is.null(opts$col.fun)) {
                    opts$col.fun(nby)
                } else {
                    opts$col.default$cat(nby)
                }
        }
        col.args$b.cols <- barcol
    }

    if (!is.null(locate.col)) col.args$locate.col <- locate.col
    col.args
}
