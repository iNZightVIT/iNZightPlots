iNZightPlot <- function(x, y = NULL, g1 = NULL, g1.level = NULL,
                        g2 = NULL, g2.level = NULL, varnames = list(),
                        colby = NULL, sizeby = NULL,
                        data = NULL, design = NULL, freq = NULL,
                        missing.info = TRUE,
                        xlab = varnames$x, ylab = varnames$y,
                        new = TRUE,  # compatibility arguments
                        inzpars = inzpar(), layout.only = FALSE, ...) {

  # ------------------------------------------------------------------------------------ #
  #   iNZightPlots v2.0, written by Tom Elliott (2014, University of Auckland)
  #
  # This function will `attempt` to take a large variety of data configurations and
  # attempt to make a suitable plot. It can take into account the data structure (for
  # example, frequency or survey data), and make use of these in the final plot. It also
  # contains a suite of additional options such a trend lines and inference information
  # which can also be added to plots as required.
  # 
  # A Summary and Inference method will be associated with the output of this file, so
  # users can easily get numerical information about any particular plot they produce. The
  # inference information will be either theoretically or bootstrap based.
  #
  # ------------------------------------------------------------------------------------ #
  # ++++++++ FOR (future) DEVELOPERS ++++++++
  #
  # First of all, welcome to the iNZight team!3
  # Second, have fun reading through all the code :D
  #
  # +++ How iNZightPlots works +++
  #
  # There are (going to be) several methods for getting to his function: directly, via a
  # formula, or through the iNZight software (this will just use the direct method). That
  # is considered a separate project, and any changes made to this function *should not*
  # interfere with the overall workings of iNZight (and if it does, you'll need to make
  # the relevant changes within the iNZight package so it knows about them).
  #
  # There is also a global `inzpar()` settings list which contains all of the necessary
  # style, colour, and additional information for plots, which will be used by all of the
  # drawing functions (i.e., cex, pch, col, etc.)
  #
  # 1. The data step:
  #    - organise the data into a data.frame, which will allow for much simpler use later
  #      on.
  #    - subset the data according to g2(.level) --- this *only* shows the specified
  #      level(s), unless it is set to "_MULTI" in which case we do a matrix plot.
  #
  # 2. The plot setup step
  #    - now create a list, one element for every level of g1 (if g1 is NULL, simply a
  #      list with one level) --- the plot methods will take a list argument.
  #    - remove missing values, *keeping a record of them for the summary function*
  #    - use the list of plot objects to calculate axis limits etc.
  #    - now figure out all the necessary sizing of margins, number of plots for subsets,
  #      and spacing for the legend(s).
  #
  # 3. The plot draw step
  #    - once this is all set up, do an lapply over the plot list and plot each one in the
  #      appropriate sub-window
  #
  # 4. The text step
  #    - add all of the titles, legends, etc. to the plot
  #
  # 5. The return step
  #    - now return an object with a class of `inzPlot`, which will have `summary()` and
  #      `inference()` methods (and also a `plot()` method to redraw the plot later, so it
  #      would be nice if the plot object contained all of the necessary information so it
  #      can simply jump to step 3 and save redoing calculations)
  #    - return this invisibly, unless `summary = TRUE` or `inference = TRUE` is
  #      specified.
  #
  # ------------------------------------------------------------------------------------ #
  #
  # And that's how this function should work. The *most important* aim of everything is
  # making everything as extensible as possible. That is, if we want to add new features
  # at a later date, it needs to be written in such as way that this is as simple as
  # possible. Use methods where possible (i.e., plot.inzscatter() rather than
  # iNZscatterPlot()) so the actual code simply states `plot(obj)`, which will call the
  # appropriate function.
  #
  # Have fun coding!
  # ------------------------------------------------------------------------------------ #
  #               Original author: Tom Elliott <tell029@aucklanduni.ac.nz>               #
  # ------------------------------------------------------------------------------------ #

  ########################################################################################
  ########################################################################################
    
  # ------------------------------------------------------------------------------------ #
  # 1. The data step
  # ----------------
    
    # grab the arguments and the data frame is supplied:
    m <- match.call(expand.dots = FALSE)
    env <- parent.frame()

    if ("design" %in% names(m)) {
        md <- eval(m$design, env)
    } else {
        md <- eval(m$data, env)
    }
    
    # we now want to create a data object which contains *ALL* of the necessary
    # information, including survey design, or frequency information:
    df <- inzDataframe(m, data = md, names = varnames, g1.level, g2.level, env = env)

    # df will have a class: inz.simple, inz.freq, inz.survey
    # each of these classes will have appropriate methods for extracting the information

    varnames <- as.list(df$varnames)
    vartypes <- lapply(df$data[, names(varnames), drop = FALSE],
                       function(x) ifelse(is.factor(x), "factor", "numeric"))
    names(vartypes) <- unlist(varnames)
    df.vs <- colnames(df$data)
    missing <- list()  # a container to save missing value information

    ## ensure it matches what comes back from inzDataframe()
    g.level <- df$glevels
    g1.level <- g.level$g1.level
    g2.level <- g.level$g2.level

    # do some type checks
    xfact <- is.factor(df$data$x)
    ynull <- ! "y" %in% df.vs
    yfact <- if (ynull) NULL else is.factor(df$data$y)

    # check the number of levels for a barchart:
    if (xfact) {
        if (ynull) {
            if (length(levels(df$data$x)) > params("max.levels")) {
                msg <- paste0("Too many levels in ", varnames$x,
                              " to draw a barchart.\n",
                              "(", varnames$x, " has ",
                              length(levels(df$data$x)), " levels.)")
                stopPlot(msg)
                return(invisible(NULL))
            }
        } else if (yfact) {
            if (length(levels(df$data$x)) * length(levels(df$data$y)) > params("max.levels")) {
                msg <- paste0("Too many levels in ", varnames$x, " and ",
                              varnames$y, " to draw a barchart.\n",
                              "(", varnames$x, " has ",
                              length(levels(df$data$x)), " levels, ",
                              varnames$y, " has ", length(levels(df$data$y)),
                              "levels.)")
                stopPlot("Too many levels in x and y to draw a barchart.")
                return(invisible(NULL))
            }
        }
    }

    # subset the data by g2 (keep everything, so xlims can be calculated)
    # g2 can take values (0 = "_ALL", 1:ng2, ng2+1 = "_MULTI")
    dfsub <- gSubset(df, g1.level, g2.level, df.vs, missing)
    matrix.plot <- dfsub$matrix
    missing <- dfsub$missing
    g1.level <- dfsub$g1.level
    g2.level <- dfsub$g2.level

    df.list <- dfsub$df
    
    # now, everything simply gets applied to the list of dataframes to
    # generate the necessary plots
    
  # ------------------------------------------------------------------------------------ #
  # 2. The plot setup step
  # ----------------------

    # The aim of this step is to produce a list of things to plot, each element pertaining to a
    # level of g1 and g2, containing the necessary information.
    
    dots <- list(...)  # capture the additional arguments
    opts <- inzpars
    wopt <- names(dots) %in% names(opts)  # which additional settings have been specified
    opts <- modifyList(opts, dots[wopt])
    
    if (!xfact) xx <- df$data$x
    if (!ynull) if (!yfact) yy <- df$data$y

    xattr <- list(class = class(df), v = colnames(df$data), varnames = as.list(df$varnames),
                  vartypes = structure(vartypes, .Names = names(varnames)))
    if (!xfact) xattr$xrange = range(xx[is.finite(xx)])
    if (!ynull) if (!yfact) xattr$yrange <- range(yy[is.finite(yy)])
    if (!is.null(df$max.freq))
        xattr$max.freq <- df$max.freq

    if (opts$matchplots) {
      # this is the case where the data is subset by g1/g2, but we want the plots to be the same
      # across all levels

      # we just need to go through all plots and test if they should be LARGESAMPLE or not:
        if (is.null(opts$largesample)) {
            maxRow <- max(sapply(df.list, function(df) sapply(df, nrow)))
            opts$largesample <- maxRow > opts$large.sample.size  # essentially override the
                                                                 # largesample argument
        }
    }
    plot.list <- lapply(df.list, function(df)
                        lapply(df, createPlot, opts, xattr))

    ## X and Y axis limits:
    xlim <- range(sapply(plot.list, function(x) sapply(x, function(y) y$xlim)), finite = TRUE)
    ylim <- range(sapply(plot.list, function(x) sapply(x, function(y) y$ylim)), finite = TRUE)
    ylim.raw <- ylim
    xlim.raw <- xlim

    TYPE <- gsub("inz", "", class(plot.list[[1]][[1]]))
    if (!TYPE %in% c("bar")) xlim <- extendrange(xlim)
    ylim <-
        if (TYPE %in% c("scatter", "grid", "hex")) extendrange(ylim)
        else c(0, extendrange(ylim)[2])

    maxcnt <- NULL
    if (TYPE %in% c("grid", "hex")) {
      # if there is a `counts` need to get the max:
        maxcnt <- switch(TYPE,
                         "grid" = {
                             warning("Frequency density not constant scale across multiple plots yet.")
                         }, "hex" = {
                             max(sapply(plot.list, function(x) sapply(x, function(y) {
                                 if (class(y) == "inzhex")
                                     max(y$hex@count, na.rm = TRUE)
                                 else 0
                             })))
                         })
    } else if (TYPE %in% c("dot", "hist")) {
        maxcnt <- ylim.raw[2]
    }
    
    if (is.numeric(df$data$colby))
        opts$trend.by <- FALSE

    # Set up the plot layout

    ## --- The Main Viewport: this one is simply the canvas, and global CEX value
    dd <- dev.flush(dev.flush())  # flush everything ...

    dev.hold()
    grid.newpage()
    pushViewport(viewport(gp = gpar(cex = opts$cex), name = "container"))
    grid.rect(gp = gpar(fill = opts$bg, col = opts$bg))

    PAGE.height <- convertHeight(current.viewport()$height, "in", TRUE)  # essentially the height of the window
    
    ## --- there will be some fancy stuff here designing and implementing a grid which adds titles,
    ## labels, and optionally legends

    # --- first, need to make all of the labels/legends/etc:
    VT <- vartypes
    names(VT) <- names(varnames)
    if (all(c("x", "y") %in% names(VT))) {
        if (VT$y == "numeric" & VT$x == "factor") {
            xn <- varnames$y
            varnames$y <- varnames$x
            varnames$x <- xn
            VT$x <- "numeric"
            VT$y <- "factor"
        }
    }
    
    titles <- list()
    titles$main <-
        if (!is.null(dots$main)) dots$main
        else makeTitle(varnames, VT, g1.level, g2.level)
    titles$xlab <- if (!is.null(dots$xlab)) dots$xlab else varnames$x
    if (!ynull) {
        titles$ylab <-
            if (!is.null(dots$ylab)) dots$ylab
            else if (xfact & yfact) "Proportion (%)" else varnames$y
    } else if (xfact) {
        titles$ylab <- "Proportion (%)"
    }
    if ("colby" %in% df.vs) titles$legend <- varnames$colby


    ## plot.list still contains all the levels of g1 that wont be plotted - for axis scaling etc
    ## so figure this one out somehow ...
    N <- length(plot.list) * length(g1.level)
    multi.cex <- max(1.2 * sqrt(sqrt(N) / N), 0.5)  # this has absolutely no theoretical reasoning,
                                                    # it just does a reasonably acceptable job (:

    
    # --- WIDTHS of various things
    # first we need to know HOW WIDE the main viewport is, and then
    # split the title text into the appropriate number of lines,
    # then calcualate the height of it.
    VPcontainer.width <- convertWidth(unit(1, "npc"), "in", TRUE)
    main.grob <- textGrob(titles$main, gp = gpar(cex = opts$cex.main))
    MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
    MAIN.lnheight <- convertWidth(grobHeight(main.grob), "in", TRUE)
    if (MAIN.width > 0.9 * VPcontainer.width) {
        titles$main <- gsub(",", ",\n", titles$main)
        main.grob <- textGrob(titles$main, gp = gpar(cex = opts$cex.main))
        MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
    }
    if (MAIN.width > 0.9 * VPcontainer.width) {
        titles$main <- gsub("subset", "\nsubset", titles$main)
        main.grob <- textGrob(titles$main, gp = gpar(cex = opts$cex.main))
        MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
    }
    if (MAIN.width > 0.9 * VPcontainer.width) {
        titles$main <- gsub(" (size prop", "\n (size prop", titles$main, fixed = TRUE)
        main.grob <- textGrob(titles$main, gp = gpar(cex = opts$cex.main))
        MAIN.width <- convertWidth(grobWidth(main.grob), "in", TRUE)
    }
    MAIN.height <- convertHeight(grobHeight(main.grob), "in", TRUE) + MAIN.lnheight

    # -- xaxis labels
    xlab.grob <- textGrob(titles$xlab, y = unit(0.6, "lines"),
                          gp = gpar(cex = opts$cex.lab))
    XLAB.height <- convertHeight(grobHeight(xlab.grob), "in", TRUE) * 3
    # -- yaxis labels
    if (!is.null(titles$ylab)) {
        ylab.grob <- textGrob(titles$ylab, x = unit(0.6, "lines"),
                              rot = 90, gp = gpar(cex = opts$cex.lab))
        YLAB.width <- convertWidth(grobWidth(ylab.grob), "in", TRUE) * 3
    } else {
        YLAB.width <- 0
    }

    # -- xaxis marks
    XAX.height <- convertWidth(unit(1, "lines"), "in", TRUE) * 2 * opts$cex.axis

    # -- yaxis marks
    YAX.width <- if (TYPE %in% c("dot", "hist") & !ynull) {
        # need to grab the factoring variable -> might be x OR y
        yf <- if (is.factor(df$data$y)) df$data$y else df$data$x
        yl <- levels(yf)
        yWidths <- sapply(yl, function(L)
                          convertWidth(grobWidth(
                              textGrob(L, gp = gpar(cex = opts$cex.axis * multi.cex))
                              ), "in", TRUE))
        max(yWidths)
    } else 0

    YAX.default.width <- convertWidth(unit(1, "lines"), "in", TRUE) * 2 * opts$cex.axis
    YAX.width <- YAX.width + YAX.default.width

    # -- legend(s)
    barplot <- FALSE
    leg.grob1 <- leg.grob2 <- leg.grob3 <- NULL
    cex.mult = ifelse("g1" %in% df.vs, 1,
        ifelse("g1.level" %in% df.vs,
               ifelse(length(levels(df$g1.level)) >= 6, 0.7, 1), 1))


    xnum <- !xfact
    yfact <- if (ynull) FALSE else yfact
    ynum <- if (ynull) FALSE else !yfact

    col.args <- list(missing = opts$col.missing)
    if ("colby" %in% names(varnames) &
        (TYPE %in% c("dot", "scatter") || (TYPE %in% c("grid", "hex") & !is.null(opts$trend) & opts$trend.by))) {
        if (is.factor(df$data$colby)) {
            nby <- length(levels(as.factor(df$data$colby)))
            if (length(opts$col.pt) >= nby) {
                ptcol <- opts$col.pt[1:nby]
            } else {
                ptcol <- genCols(nby)
            }

            misscol <- any(sapply(plot.list, function(x) sapply(x, function(y) y$nacol)))
            leg.grob1 <- drawLegend(f.levels <- levels(as.factor(df$data$colby)), col = ptcol,
                                    pch = ifelse(barplot, 22, opts$pch),
                                    title = varnames$colby, any.missing = misscol, opts = opts)
            if (misscol) {
                ptcol <- c(ptcol, opts$col.missing)
                f.levels <- c(f.levels, "missing")
            }
            col.args$f.cols <- structure(ptcol, .Names = f.levels)
        } else {
            misscol <- any(sapply(plot.list, function(x) sapply(x, function(y) y$nacol)))
            leg.grobL <- drawContLegend(df$data$colby, title = varnames$colby,
                                        height = 0.4 * PAGE.height, cex.mult = cex.mult,
                                        any.missing = misscol, opts = opts)
            leg.grob1 <- leg.grobL$fg
            col.args$n.range <- range(df$data$colby, na.rm = TRUE)
            col.args$n.cols <- leg.grobL$n.cols
        }
    } else if (xfact & yfact) {
        nby <- length(levels(as.factor(df$data$y)))
        if (length(opts$col.pt) >= nby) {
            barcol <- opts$col.pt[1:nby]
        } else {
            barcol <- genCols(nby)
        }
        
        leg.grob1 <- drawLegend(levels(as.factor(df$data$y)), col = barcol, pch = 22,
                                title = varnames$y, opts = opts)
        col.args$b.cols <- barcol
    }
    
    if ("sizeby" %in% names(varnames) & TYPE %in% c("scatter")) {
        misssize <- any(sapply(plot.list, function(x) sapply(x, function(x2) x2$nasize)))
        if (misssize) {
            misstext <- paste0("missing ", varnames$sizeby)
            leg.grob2 <- drawLegend(misstext, col = "grey50", pch = 4,
                                    cex.mult = cex.mult * 0.8, opts = opts)
        }        
    }

    if (xnum & ynum) {
        df.lens <- lapply(plot.list, function(a) {
            mm <- sapply(a, function(b) 
                         sum(apply(cbind(b$x, b$y), 1, function(c) all(!is.na(c)))))
            A <- a[[which.max(mm)]]
            cbind(A$x, A$y)
        })
        ddd <- df.lens[[which.max(sapply(df.lens, nrow))]]
        leg.grob3 <- drawLinesLegend(ddd, opts = opts, cex.mult = cex.mult * 0.8)
    }

    hgts <- numeric(3)
    wdth <- 0
    
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

    ## --- Figure out a subtitle for the plot:
    missing <- missing[missing != 0]
    if ("subtitle" %in% names(dots)) {
        SUB <- textGrob(dots$subtitle, gp = gpar(cex = opts$cex.text * 0.8))
    } else if (missing.info & length(missing) > 0) {
        names(missing) <- unlist(varnames[match(names(missing), names(varnames))])
        total.missing <- sum(sapply(missing, sum))
        missinfo <- paste0(missing, " in ", names(missing), collapse = ", ")
        subtitle <- paste0(total.missing, " missing values (",
                           missinfo, ")")
        SUB <- textGrob(subtitle, gp = gpar(cex = opts$cex.text * 0.8))
    } else {
        SUB <- NULL
    }

    ## --- CREATE the main LAYOUT for the titles + main plot window
    MAIN.hgt <- unit(MAIN.height, "in")
    XAX.hgt <- unit(XAX.height, "in")
    XLAB.hgt <- unit(XLAB.height, "in")
    PLOT.hgt <- unit(1, "null")
    SUB.hgt <- if (is.null(SUB)) unit(0, "null") else convertUnit(grobHeight(SUB) * 2, "in")

    YLAB.wd <- unit(YLAB.width, "in")
    YAX.wd <- unit(YAX.width, "in")
    PLOT.wd <- unit(1, "null")
    LEG.wd <-
        if (wdth > 0) unit(wdth, "in") + unit(1, "char")
        else unit(0, "null") 
    
    TOPlayout <- grid.layout(nrow = 6, ncol = 5,
                             heights = unit.c(MAIN.hgt, XAX.hgt, PLOT.hgt,
                                 XAX.hgt, XLAB.hgt, SUB.hgt),
                             widths = unit.c(YLAB.wd, YAX.wd, PLOT.wd, YAX.wd, LEG.wd))

    ## Send the layout to the plot window
    pushViewport(viewport(layout = TOPlayout, name = "VP:TOPlayout"))
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
        leg.layout <- grid.layout(3, heights = unit(hgts, "in"))
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
    }

    ## --- next, it will break the plot into subregions for g1 (unless theres only one, then it
    ## wont)
    
    ## break up plot list
    if (any(g2.level == "_MULTI")) g2.level <- names(plot.list)
    if (!matrix.plot & !is.null(g2.level)) {
        plot.list <- plot.list[g2.level]
    }

    print(g1.level)
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
    
    ## if the plots are DOTPLOTS or BARPLOTS, then leave a little bit of space between each
  # we will need to add a small amount of space between the columns of the layout
    hspace <- ifelse(TYPE %in% c("scatter", "grid", "hex"), 0, 0.01)
    wds <- rep(unit.c(unit(hspace, "npc"), unit(1, "null")), nc)[-1]

    dummy <- textGrob("some text", gp = gpar(cex = opts$cex.lab * multi.cex))
    sub.hgt <- convertHeight(grobHeight(dummy), "in", TRUE) * 2
    vspace <- ifelse(matrix.plot, sub.hgt, 0)
    hgts <- rep(unit.c(unit(vspace, "in"), unit(1, "null")), nr)


    PLOTlayout <- grid.layout(nrow = length(hgts), ncol = length(wds),
                              heights = hgts, widths = wds)
    seekViewport("VP:TOPlayout")
    pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 3))
    pushViewport(viewport(layout = PLOTlayout, name = "VP:PLOTlayout"))    

    ## --- within each of these regions, we simply plot!
    ax.gp <- gpar(cex = opts$cex.axis)

    ## --- START from the BOTTOM and work UP; LEFT and work RIGHT (mainly makes sense for continuous
    ## grouping variables)
    g1id <- 1  # keep track of plot levels
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
        R <- r * 2  # skip the gaps between rows
        if (matrix.plot) {
            # add that little thingy
            seekViewport("VP:PLOTlayout")
            pushViewport(viewport(layout.pos.row = R - 1,
                                  gp = gpar(cex = multi.cex, fontface = "bold")))
            grid.rect(gp = gpar(fill = "lightblue"))
            grid.text(paste(varnames$g2, "=", g2.level[g2id]), gp = gpar(cex = opts$cex.lab))
        }

        for (c in 1:nc) {
            if (g2id > NG2) next()
            C <- c * 2 - 1

            seekViewport("VP:PLOTlayout")
            pushViewport(viewport(layout.pos.row = R, layout.pos.col = C,
                                  xscale = xlim, yscale = ylim,
                                  gp = gpar(cex = multi.cex)))
            grid.rect()

            subt <- g1.level[g1id]

            ## calculate the height of the subtitle if it is specified
            p.title <- if (subt == "all") NULL else subt
            hgt <- unit.c(
                if (!is.null(p.title)) {
                    subt <- textGrob(p.title, gp = gpar(cex = opts$cex.lab, fontface = "bold"))
                    unit(convertHeight(grobHeight(subt), "in", TRUE) * 2, "in")
                } else {
                    unit(0, "null")
                },
                unit(1, "null"))
            pushViewport(viewport(layout = grid.layout(2, 1, heights = hgt)))
            
            if (!is.null(p.title)) {
                pushViewport(viewport(layout.pos.row = 1))
                grid.rect(gp = gpar(fill = opts$col.sub))
                grid.draw(subt)
                upViewport()
            }

            ### I found "VP:locate.these.points" so far is just using here and no other
            ## depencies so I think giving the its an uniqe name would be a good idea here.
            nameVP <- paste0("VP:locate.these.points", g2id, g1id)
            pushViewport(viewport(layout.pos.row = 2, xscale = xlim, yscale = ylim, clip = "on",
                                  name = nameVP)) 
            #pushViewport(viewport(layout.pos.row = 2, xscale = xlim, yscale = ylim, clip = "on",
            #                      name = "VP:locate.these.points"))
            if (!layout.only)
                plot(plot.list[[g2id]][[g1id]], gen =
                     list(opts = opts, mcex = multi.cex, col.args = col.args,
                          maxcount = maxcnt))
            upViewport()
            

            # add the appropriate axes:
            # Decide which axes to plot:

            ### -------------
            # For dotplots + histograms: the axis are at the bottom of every column, and on the far
            # left
            #
            # For scatterplots + gridplots + hexplots: the axis alternative on both axis, left and
            # right
            #
            # For barplot: the axis is on the bottom of every column, and left and right of every
            # row 
            ### ------------
            
            pushViewport(viewport(layout.pos.row = 2, xscale = xlim,
                                  yscale = if (TYPE == "bar") 100 * ylim else ylim))
            if (r == nr)  # bottom
                drawAxes(X, "x", TRUE, c %% 2 == 1 | !TYPE %in% c("scatter", "grid", "hex"), opts)

            if (c == 1)  # left column
                drawAxes(if (TYPE == "bar") ylim else Y, "y", TRUE, (nr - r) %% 2 == 0, opts)

            if (!TYPE %in% c("dot", "hist")) {
                if (c == nc | g1id == NG1) # right column (or last plot in top row)
                    drawAxes(if (TYPE == "bar") ylim else Y, "y", FALSE, (nr - r) %% 2 == 1, opts)
            }
            upViewport()

            if (TYPE %in% c("scatter", "grid", "hex")) {
                pushViewport(viewport(layout.pos.row = 1, xscale = xlim, yscale = ylim))
                if (r == 1)
                    drawAxes(X, "x", FALSE, c %% 2 == 0, opts, sub = vspace)
                upViewport()
            }

            ## This is necessary to delete the "old" viewport so we can create a new one
            ## of the same name, but retain it long enough to use it for drawing the axes
            if (TYPE %in% c("dot", "hist") & !layout.only)
                switch(TYPE,
                       "dot" = {
                           seekViewport("VP:dotplot-levels")
                           popViewport()
                       }, "hist" = {
                           seekViewport("VP:histplot-levels")
                           popViewport()
                       })
            

            ## update the counters
            if (g1id < NG1) {
                g1id <- g1id + 1
            } else {
                g1id <- 1
                g2id <- g2id + 1
            }

            
        }
    }
    
    dev.flush()
    cat("Success...\n")
    plot.list$gen <- list(opts = opts, mcex = multi.cex, col.args = col.args,
                          maxcount = maxcnt)
    plot.list$xlim <- xlim
    plot.list$ylim <- ylim
    return(invisible(plot.list))
}
