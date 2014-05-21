iNZightPlot <- function(x, y = NULL, g1 = NULL, g1.level = NULL,
                        g2 = NULL, g2.level = NULL, varnames = list(),
                        colby = NULL, sizeby = NULL,
                        data = NULL, structure = NULL,
                        inzpars = inzpar(), ...) {

  # ------------------------------------------------------------------------------------ #
  #   iNZightPlots v1.1, written by Tom Elliott (2014, University of Auckland)
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
  # First of all, welcome to the iNZight team!
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

   # dev.hold()  # wait until finished drawing, THEN plot
    
  # ------------------------------------------------------------------------------------ #
  # 1. The data step
  # ----------------

    # grab the arguments and the data frame is supplied:
    m <- match.call(expand.dots = FALSE)
    env <- parent.frame()
    md <- eval(m$data, env)

    # returns dataframe with all of the supplied variables, and additionally does checking
    # of g1, g2, colby etc. to ensure everything is cross-compatible
    df <- inzDataframe(m, data = md, names = varnames, g1.level, g2.level,
                       structure = structure, env = env)
    varnames <- as.list(attr(df, "varnames"))
    vartypes <- lapply(df, function(x) ifelse(is.factor(x), "factor", "numeric"))
    names(vartypes) <- colnames(df)
    df.vs <- colnames(df)
    missing <- list()  # a container to save missing value information

    # do some type checks
    xfact <- is.factor(df$x)
    ynull <- ! "y" %in% df.vs
    yfact <- if (ynull) NULL else is.factor(df$y)

    # check the number of levels for a barchart:
    if (xfact) {
        if (ynull) {
            if (length(levels(df$x)) > params("max.levels")) {
                msg <- paste0("Too many levels in ", varnames$x,
                              " to draw a barchart.\n",
                              "(", varnames$x, " has ",
                              length(levels(df$x)), " levels.)")
                stopPlot(msg)
                return()
            }
        } else if (yfact) {
            if (length(levels(df$x)) * length(levels(df$y)) > params("max.levels")) {
                msg <- paste0("Too many levels in ", varnames$x, " and ",
                              varnames$y, " to draw a barchart.\n",
                              "(", varnames$x, " has ",
                              length(levels(df$x)), " levels, ",
                              varnames$y, " has ", length(levels(df$y)),
                              "levels.)")
                stopPlot("Too many levels in x and y to draw a barchart.")
                return()
            }
        }
    }
    
    # subset the data by g2 (keep everything, so xlims can be calculated)
    # g2 can take values (0 = "_ALL", 1:ng2, ng2+1 = "_MULTI")
    matrix.plot <- FALSE
    if ("g2" %in% df.vs) {
        if (is.null(g2.level)) g2.level <- "_ALL"
        ng2 <- length(g2l <- if (is.null(g2.level)) "all" else levels(df$g2))

        # if g2 specified numerically, check the value is ok, and then convert it to
        # character level anyway
        if (is.numeric(g2.level)) {
            if (as.integer(g2.level) != g2.level)
                warning(paste0("g2.level truncated to ", g2.level, "."))
            
            if (g2.level == 0) {
                g2.level <- "_ALL"
            } else if (g2.level == ng2 + 1) {
                g2.level <- "_MULTI"
            } else if (g2.level > ng2 + 1) {
                stop(paste("g2.level must be a number between 0 and", ng2 + 1))
            } else {
                g2.level <- g2l[g2.level]
            }
        }

        # separate function for drawing the matrix version
        if (g2.level == "_ALL") {
            df1 <- list(all = df)
        } else {
            if (g2.level == "_MULTI") {
                matrix.plot <- TRUE
            }                
            
            missing$g2 <- sum(is.na(df$g2))
            df1 <- lapply(g2l,
                          function(l) {
                              dft <- subset(df, df$g2 == l)
                              dft[, colnames(dft) != "g2"]
                          })
            names(df1) <- g2l
        }
    } else {
        g2l <- "all"
        df1 <- list(all = df)
    }

    # now, `df` is a list of data.frame of all levels of g2 (unless
    # g2.level = NULL/_ALL/0).  `missing` constains the number of
    # observations lost by subsetting g2 due to missing values of g2.

    if ("g1" %in% df.vs) {
        # take two methods of specifying g1.level (numeric or level names), and convert to a vector
        # of only character names to be plotted
        g1l <- levels(df$g1)  # all levels of variable
        if (is.null(g1.level)) g1.level <- "_MULTI"
        
        if (is.numeric(g1.level))
            g1.level <- if (any(g1.level == 0)) "_MULTI" else levels(df$g1)[g1.level]

        if (any(g1.level == "_MULTI"))
            g1.level <- levels(df$g1)

        # track missing values due to missingness in g1
        missing$g1 <- sum(is.na(df$g1))
    } else {
        g1l <- "all"
        g1.level <- "all"
    }

    # this converts each data.frame in the list to a list of data
    # frames for all levels of g1
    df.list <- lapply(df1, function(df2) {
        df3 <- lapply(g1l, function(x) inzDataList(df2, x))
        names(df3) <- g1l
        df3
    })

    # now, everything simply gets applied to the list of dataframes to
    # generate the necessary plots

  # ------------------------------------------------------------------------------------ #
  # 2. The plot setup step
  # ----------------------

    # The aim of this step is to produce a list of things to plot, each element pertaining to a
    # level of g2 and g2, containing the necessary information.

    dots <- list(...)  # capture the additional arguments
    opts <- inzpars
    wopt <- names(dots) %in% names(opts)  # which additional settings have been specified
    opts <- modifyList(opts, dots[wopt])
    plot.list <- lapply(df.list, function(df) lapply(df, createPlot, opts))

    # sort out the axis limits
    xlim <- extendrange(range(sapply(plot.list,
                                     function(pl) range(sapply(pl,
                                                               function(x) x$xlim)))),
                        f = 0.04)  # add 4% to each axis
    ylim <- extendrange(range(sapply(plot.list,
                                     function(pl) range(sapply(pl,
                                                               function(x) x$ylim)))),
                        f = 0.04)
    
    # Set up the plot layout

    ## --- The Main Viewport: this one is simply the canvas, and global CEX value
    grid.newpage()
    pushViewport(viewport(gp = gpar(cex = opts$cex), name = "container"))
    grid.rect(gp = gpar(fill = opts$bg, col = opts$bg))

    ## --- there will be some fancy stuff here designing and implementing a grid which adds titles,
    ## labels, and optionally legends

    # --- first, need to make all of the labels/legends/etc:
    titles <- list()
    titles$main <-
        if ("main" %in% names(dots)) dots$main
        else makeTitle(varnames, vartypes, g1.level, g2.level)
    titles$xlab <- if ("xlab" %in% names(dots)) dots$xlab else varnames$x
    if (!ynull) titles$ylab <- if ("ylab" %in% names(dots)) dots$ylab else varnames$y    
    if ("colby" %in% df.vs) titles$legend <- varnames$colby

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
    MAIN.height <- convertHeight(grobHeight(main.grob), "in", TRUE) + MAIN.lnheight

    # -- xaxis labels
    xlab.grob <- textGrob(titles$xlab, gp = gpar(cex = opts$cex.lab))
    XLAB.height <- convertHeight(grobHeight(xlab.grob), "in", TRUE) * 2
    # -- yaxis labels
    if (!ynull) {
        ylab.grob <- textGrob(titles$ylab, rot = 90, gp = gpar(cex = opts$cex.lab))
        YLAB.width <- convertWidth(grobWidth(ylab.grob), "in", TRUE) * 2
    } else {
        YLAB.width <- 0
    }

    # -- xaxis marks
    XAX.height <- convertWidth(unit(1, "lines"), "in", TRUE) * 2 * opts$cex.axis

    # -- yaxis marks
    YAX.width <- convertWidth(unit(1, "lines"), "in", TRUE) * 2 * opts$cex.axis
    

    ## --- CREATE the main LAYOUT for the titles + main plot window
    MAIN.hgt <- unit(MAIN.height, "in")
    XAX.hgt <- unit(XAX.height, "in")
    XLAB.hgt <- unit(XLAB.height, "in")
    PLOT.hgt <- unit(1, "null")
    SUB.hgt <- unit(0, "null")

    YLAB.wd <- unit(YLAB.width, "in")
    YAX.wd <- unit(YAX.width, "in")
    PLOT.wd <- unit(1, "null")
    LEG.wd <- unit(0, "null")
    
    TOPlayout <- grid.layout(nrow = 6, ncol = 5,
                             heights = unit.c(MAIN.hgt, XAX.hgt, PLOT.hgt, XAX.hgt, XLAB.hgt, SUB.hgt),
                             widths = unit.c(YLAB.wd, YAX.wd, PLOT.wd, YAX.wd, LEG.wd))

    ## Send the layout to the plot window
    pushViewport(viewport(layout = TOPlayout, name = "VP:TOPlayout"))
    ## place the title
    pushViewport(viewport(layout.pos.row = 1))
    grid.draw(main.grob)
    
    ## place axis labels
    if (!ynull) {
        seekViewport("VP:TOPlayout")
        pushViewport(viewport(layout.pos.row = 3, layout.pos.col = 1))
        grid.draw(ylab.grob)
    }
    seekViewport("VP:TOPlayout")
    pushViewport(viewport(layout.pos.row = 5, layout.pos.col = 3))
    grid.draw(xlab.grob)
    
    ## --- next, it will break the plot into subregions for g1 (unless theres only one, then it
    ## wont)

    ## break up plot list
    if (any(g2.level == "_MULTI")) g2.level <- names(plot.list)
    if (!matrix.plot & !is.null(g2.level)) {
        plot.list <- plot.list[g2.level]
    }
    
    plot.list <- lapply(plot.list, function(x) x[g1.level])

    ## create a layout
    N <- sum(sapply(plot.list, length))
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
    multi.cex <- sqrt(sqrt(N) / N)  # this has absolutely no theoretical reasoning, it just does a
                                    # reasonably acceptable job (:

    ## if the plots are DOTPLOTS or BARPLOTS, then leave a little bit of space between each
    plot.type <- class(plot.list[[1]][[1]])
  # we will need to add a small amount of space between the columns of the layout
    hspace <- ifelse(plot.type == "inzscatter", 0, 0.01)
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
    
    for (r in nr:1) {
        R <- r * 2  # skip the gaps between rows
        if (matrix.plot) {
            # add that little thingy
            seekViewport("VP:PLOTlayout")
            pushViewport(viewport(layout.pos.row = R - 1, gp = gpar(cex = multi.cex)))
            grid.rect(gp = gpar(fill = "lightblue"))
            grid.text(g2.level[r], gp = gpar(cex = opts$cex.lab))
        }

        for (c in 1:nc) {
            C <- c * 2 - 1
            seekViewport("VP:PLOTlayout")
            pushViewport(viewport(layout.pos.row = R, layout.pos.col = C,
                                  xscale = xlim, yscale = ylim,
                                  gp = gpar(cex = multi.cex)))
            grid.rect()
           # grid.text(g1.level[g1id], y=unit(1, "npc"), vjust = 1, gp = gpar(cex = opts$cex.lab))

            # add the appropriate axes:
            # Decide which axes to plot:
            axis <- rep(0, 4)
            if (r == nr)
                axis[1] <- 1
            if (r == nr & c %% 2 == 1)
                axis[1] <- 2
            if (c == 1)
                axis[2] <- 1
            if (c == 1 & (nr - r) %% 2 == 0)
                axis[2] <- 2
            if (r == 1)
                axis[3] <- 1
            if (r == 1 & c %% 2 == 0)
                axis[3] <- 2
            if (c == nc)
                axis[4] <- 1
            if ((c == nc) & (nr - r) %% 2 == 1)
                axis[4] <- 2

            plot(plot.list[[g2id]][[g1id]], inzpar = opts,
                 axis = axis, title = g1.level[g1id], sub = sub.hgt)

            ## update the counters
            if (g1id < NG1) {
                g1id <- g1id + 1
            } else {
                g1id <- 1
                g2id <- g2id + 1
            }
        }
    }
    
    ## pushViewport(viewport(xscale = xlim, yscale = ylim))
    ## grid.xaxis(gp = ax.gp)
    ## grid.xaxis(main = FALSE, label = FALSE, gp = ax.gp)
    ## grid.yaxis(name = "gyaxis", gp = ax.gp)
    ## grid.yaxis(main = FALSE, label = FALSE, gp = ax.gp)
    ## grid.edit("gyaxis", edits = gEdit("labels", rot = 90, hjust = 0.5, vjust = 0))

    dev.flush()
    out <- list(data = df.list, toplot = plot.list, missing = missing, inzpar = opts)
    invisible(out)
}
