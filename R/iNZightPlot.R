iNZightPlot <- function(x, y = NULL, g1 = NULL, g1.level = NULL,
                        g2 = NULL, g2.level = NULL, varnames = list(),
                        colby = NULL, sizeby = NULL,
                        data = NULL, structure = NULL,
                        inzpars = inzpar(), ...) {

  # ------------------------------------------------------------------------------------ #
  #   iNZightPlots 1.1.0, written by Tom Elliott (2014, University of Auckland)
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
  #    - create the plots using grid *without plotting them* i.e., save them in a list
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
    md <- eval(m$data, env)

    # returns dataframe with all of the supplied variables, and additionally does checking
    # of g1, g2, colby etc. to ensure everything is cross-compatible
    df <- inzDataframe(m, data = md, names = varnames, g1.level, g2.level,
                       structure = structure, env = env)
    varnames <- as.list(attr(df, "varnames"))
    missing <- list()  # a container to save missing value information

    # check the number of levels for a barchart:
    if (is.factor(df$x)) {
        if (is.null(df$y)) {
            if (length(levels(df$x)) > params("max.levels")) {
                msg <- paste0("Too many levels in ", varnames$x,
                              " to draw a barchart.\n",
                              "(", varnames$x, " has ",
                              length(levels(df$x)), " levels.)")
                stopPlot(msg)
                return()
            }
        } else if (is.factor(df$y)) {
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
    
    # subset the data by g2
    # g2 can take values (0 = "_ALL", 1:ng2, ng2+1 = "_MULTI")
    if ("g2" %in% colnames(df)) {
        ng2 <- length(levels(df$g2))

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
                g2.level <- levels(df$g2)[g2.level]
            }
        }

        # separate function for drawing the matrix version
        if (g2.level == "_MULTI")
            return(iNZmatrix(df, ...))

        # subset by g2 if it isn't set to _ALL
        if (g2.level != "_ALL") {
            missing$g2 <- sum(is.na(df$g2))
            df <- subset(df, df$g2 == g2.level)
            df <- df[, colnames(df) != "g2"]
        }
    }

    # now, `df` is a data.frame of the specified level of g2.level.
    # `missing` constains the number of observations lost by
    # subsetting g2 due to missing values of g2.

    if ("g1" %in% colnames(df)) {
        # take two methods of specifying g1.level (numeric or level names), and convert to a vector
        # of only character names to be plotted
        if (is.null(g1.level)) g1.level <- "_MULTI"
        
        if (is.numeric(g1.level))
            g1.level <- if (any(g1.level == 0)) "_MULTI" else levels(df$g1)[g1.level]

        if (any(g1.level == "_MULTI"))
            g1.level <- levels(df$g1)

        # track missing values due to missingness in g1
        missing$g1 <- sum(is.na(df$g1))
    } else {
        g1.level <- "all"
    }

    # this converts the single dataframe into a list of dataframes
    df.list <- lapply(g1.level, function(x) inzDataList(df, x))
    names(df.list) <- g1.level

    # now, everything simply gets applied to the list of dataframes to
    # generate the necessary plots

  # ------------------------------------------------------------------------------------ #
  # 2. The plot setup step
  # ----------------------

    # The aim of this step is to produce a list of things to plot, each element pertaining to a
    # level of g1.

    dots <- list(...)  # capture the additional arguments
    opts <- inzpars
    wopt <- names(dots) %in% names(opts)  # which additional settings have been specified
    opts <- modifyList(opts, dots[wopt])
    plot.list <- lapply(df.list, createPlot, opts)

    invisible(list(toplot = plot.list, missing = missing, inzpar = opts))
}
