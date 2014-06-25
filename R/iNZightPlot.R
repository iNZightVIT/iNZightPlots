iNZightPlot <- function(x, y = NULL, g1 = NULL, g1.level = NULL,
                        g2 = NULL, g2.level = NULL, varnames = list(),
                        colby = NULL, sizeby = NULL,
                        data = NULL, design = NULL, freq = NULL,
                        missing.info = TRUE,
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
    vartypes <- lapply(df$data[, names(varnames)],
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
    xattr <- list(class = class(df))
    if (!is.null(df$max.freq))
        xattr$max.freq <- df$max.freq
    
    plot.list <- lapply(df.list, function(df)
                        lapply(df, createPlot, opts, xattr))
    #print(plot.list)
    return(plot.list)
    return(invisible(NULL))
}
