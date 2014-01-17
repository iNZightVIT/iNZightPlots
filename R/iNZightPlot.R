iNZightPlot <-
function(x, y = NULL, g1 = NULL, g2 = NULL,
             g1.level = NULL, g2.level = NULL,
             varnames = list(), xlab = varnames$x, ylab = varnames$y,
             by = NULL, prop.size = NULL, 
             ...) { 
      # --------------------------------------------------------------------------- #
      # 1. Creates lists for each subplot, containing the necessary
      #    information for the relevant plot.
      # 2. Makes the plot window and breaks it up into the necessary layout,
      #    depending on g1, legend, etc.
      # 3.
      # --------------------------------------------------------------------------- #

      # Home of all of the plotting options.
        opts <- modifyList(inzPlotDefaults(), list(...))

      # --------------------------------------------------------------------------- #
      #                                              set up required variable names

      # For variables which are supplied, but have no varname supplied, just use
      # the name of the variable

        getName <- function(name) {
            name <- gsub("[)]", "", name)
            fn <- strsplit(name, "[(]")[[1]]
            paste(sapply(fn, function(x)            
                         if (grepl("\\$", x)) strsplit(x, "\\$")[[1]][2] else x),
                  collapse = ".")
        }
        
        if (is.null(varnames$x))
            varnames$x <- getName(deparse(substitute(x)))
        if (!is.null(y) & is.null(varnames$y))
            varnames$y <- getName(deparse(substitute(y)))
        if (!is.null(g1) & is.null(varnames$g1))
            varnames$g1 <- getName(deparse(substitute(g1)))
        if (!is.null(g2) & is.null(varnames$g2))
            varnames$g2 <- getName(deparse(substitute(g2)))
        if (!is.null(by) & is.null(varnames$by))
            varnames$by <- getName(deparse(substitute(by)))
        if (!is.null(prop.size) & is.null(varnames$prop.size))
            varnames$prop.size <- getName(deparse(substitute(prop.size)))

      # Convert -Inf and Inf values to NA
        x[is.infinite(x)] <- NA
        if (!is.null(y)) y[is.infinite(y)] <- NA

      # This is a temporary fix for a "Vertical Dot Plot"
      # 
        if (is.factor(x) & is.numeric(y)) {
            x.tmp <- x
            x <- y
            y <- x.tmp
            x.name <- varnames$x
            varnames$x <- varnames$y
            varnames$y <- x.name
        }

      # --------------------------------------------------------------------------- #
      #                                                       Remove missing values
        
        na <- is.na(x)
        if (!is.null(y))
            na <- na | is.na(y)
        if (!is.null(g1))
            na <- na | is.na(g1)
        if (!is.null(g2))
            na <- na | is.na(g2)
        if (!is.null(by))
            na <- na | is.na(by)
        if (!is.null(prop.size))
            if (is.numeric(prop.size))
                na <- na | is.na(prop.size)
            else
                warning("Point sizes can only be made proportional to a numeric variable.")
        
        x <- x[!na]
        if (!is.null(y))  y  <-  y[!na]
        if (!is.null(g1)) g1 <- g1[!na]
        if (!is.null(g2)) g2 <- g2[!na]
        if (!is.null(by)) by <- by[!na]
        if (!is.null(prop.size)) prop.size <- prop.size[!na]

      # Add jitter
        if ("x" %in% strsplit(opts$jitter, '')[[1]])
            x <- jitter(x)
        if ("y" %in% strsplit(opts$jitter, '')[[1]])
            y <- jitter(y)
                
      # --------------------------------------------------------------------------- #
      #                                                                Subset by g2

      # ========================================================================= #
      # GROUPING VARIABLE 2
      # -------------------
      # Simply subset the data so only those corresponding the g2.level
      # are plotted (if g2.level not given, ignore it).
      #
      # TODO: This will become a second dimension for a plot matrix for pairwise
      #       comparison of two (or even three) factor variables.
      # ========================================================================= #

        if (!is.null(g2) & !is.null(g2.level)) {
          # Check for iNZightCentral value:
            if (g2.level != "_ALL") {
            
              # Only use the observations according to g2.level
                if (is.numeric(g2))
                    g2 <- convert.to.factor(g2)

                x <- subset(x, g2 == g2.level)
                if (!is.null(y))
                    y <- subset(y, g2 == g2.level)
                if (!is.null(by))
                    by <- subset(by, g2 == g2.level)
                if (!is.null(g1))
                    g1 <- subset(g1, g2 == g2.level)
                if (!is.null(prop.size))
                    prop.size <- subset(prop.size, g2 == g2.level)
            } else {
                g2.level <- NULL
            }
        }
        
      # --------------------------------------------------------------------------- #
      #                                                Account for by (legend, ...)

      # ========================================================================= #
      # LEGEND FOR BY VARIABLE
      # ----------------------
      # If by is numeric (continuous), create a factor with 10 levels, and use
      # heat colours to show this.
      # Take factor, and create legend list to be passed to a drawLegend()
      # function.
      # ========================================================================= #
        
      # Get some plotting options that will be used
        barplot  <- (is.factor(x) & is.factor(y)) | (is.factor(x) & is.null(y))
        pch      <- opts$pch
        col.pt   <- if (barplot) opts$bar.fill else opts$col.pt
        cex      <- opts$cex
        cex.pt   <- opts$cex.pt
        cex.lab  <- opts$cex.lab
        cex.text <- opts$cex.text

      # Code up 'by' colours and create necessary legend.
      # using hcl(), with varying h(ue), and holding
      # c(olour) and l(ightness) constant

      # For barplots, need to code y as by for colouring:
        if (barplot) {
            by <- if (!is.null(y)) y else by
            varnames$by <- if (is.null(by)) varnames$y else varnames$by
        }
      
        if (!is.null(by)) {
            if (is.factor(by)) {
              # categorical units for by
                n.by <- length(levels(by))
              # set up colours
                if (length(col.pt) >= n.by) {
                    col.pt <- col.pt[1:n.by]
                } else {
                    col.pt <- hcl((1:n.by) / n.by * 360, c = 80, l = 50)
                }
              
            } else {
              # continuous `by' variable, so use a smooth range of colours
                n.by <- if (barplot) 4 else min(10, floor(0.5 * length(unique(by))))
                by <- cut(by, n.by)  ### *** needs fixing
                col.pt <- hcl(1:n.by / n.by * 360, c = 80, l = 50)
            }

          # every point needs a colour, unless it's a barplot
            if (!barplot)
                cols <- col.pt[as.numeric(by)]
            else
                cols <- col.pt

          # design the legend
            legend <- list(labs      = levels(by),
                           cols      = if (barplot) rep(opts$bar.col, length = n.by)
                                       else col.pt,
                           pch       = ifelse(barplot, 22, pch),
                           cex       = cex.text,
                           cex.pt    = cex,
                           cex.title = cex.lab,
                           lwd       = ifelse(barplot, opts$bar.lwd, opts$lwd.pt),
                           title     = varnames$by,
                           fill      = cols)

            leg.grob <- drawLegend(legend)
        } else {
            cols <- rep(col.pt, length = length(x))
        }
        
      # --------------------------------------------------------------------------- #
      #                                                             Subdivide by g1

      # ========================================================================= #
      # GROUPING VARIABLE 1
      # -------------------
      # If this variable is numeric, first convert it to a factor with 4 levels.
      # If g1.level is ALL or NULL, then plot every level in a separate subplot
      # Otherwise, only plot the requested levels of g1 (can be more than 1, and
      # plot exactly the same as all levels).
      # Data becomes a list where each level of each list corresponds to a level
      # of g1.
      # ========================================================================= #
        
        if (!is.null(g1)) {
          # Check for iNZightCentral value
            if (!is.null(g1.level))
                if (g1.level == "_MULTI") g1.level <- NULL
            
          # Necessary to allow continuous variables to be used to subset
            if (!is.factor(g1))
                g1 <- convert.to.factor(g1)

          # If level is supplied as a number, convert to text
            if (is.numeric(g1.level)) {
                if (g1.level > length(levels(g1)))
                    stop("g1.level must not be greater than the number of levels in g1")

                if (as.integer(g1.level) != g1.level) {
                    g1.level <- as.integer(g1.level)
                    warning(paste0("g1.level truncated to ", g1.level, "."))
                }
                
                g1.level <- if (g1.level == 0) levels(g1) else levels(g1)[g1.level]

            }

          # If plotting all levels, or nothing specified, supply all
          # levels of the factor
            if (is.null(g1.level))
                g1.level <- levels(g1)

          # Create a list for all other variables, for each level of g1
            x.list <- lapply(g1.level,
                             function(l) subset(x, g1 == l))
            names(x.list) <- g1.level
            
            if (!is.null(y)) {
                y.list <- lapply(g1.level,
                                 function(l) subset(y, g1 == l))
                names(y.list) <- g1.level
            }
            if (!is.null(by)) {
                by.list <- lapply(g1.level,
                                  function(l) subset(by, g1 == l))
                names(by.list) <- g1.level
            }
            if (!is.null(prop.size)) {
                prop.size.list <- lapply(g1.level,
                                         function(l) subset(prop.size, g1 == l))
                names(prop.size.list) <- g1.level
            }

            col.list <-
                if (barplot) {
                    col.list <- lapply(g1.level,
                                       function(l) cols)
                } else {
                    lapply(g1.level,
                           function(l) subset(cols, g1 == l))
                }
            names(col.list) <- g1.level            
        } else {
            x.list <- list(all = x)
            if (!is.null(y))
                y.list <- list(all = y)
            if (!is.null(by))
                by.list <- list(all = by)
            if (!is.null(prop.size))
                prop.size.list <- list(all = prop.size)

            col.list <- list(all = cols)
        }
        
      # --------------------------------------------------------------------------- #
      #                                            create the top-level plot layout

      # ========================================================================= #
      # LAYOUT 1  [3 x 4]
      # -----------------
      # Every plot has a title row at the top, and a footer row at the bottom.
      # row 1: if y is numeric, need the space for axis ticks and labels,
      #        otherwise just the title.
      # The central row contains everything else.
      # col 1: if there is a y-variable, this is big enough for the ylabel, and
      #        the y axis tick marks and values; otherwise, narrow.
      # col 2: this is as big as possible, containing all of the plots
      # col 3: if y is numeric, then this is big enough for the axis marks and
      #        labels; otherwise, narrow like col 1
      # col 4: if by is provided, then this contains the legend, otherwise
      #        has no width
      # ========================================================================= #

      # Calculate the width of the y-axis, so to make it as compact as possible
        w1 <-
            if (is.null(y)) {
                if (is.numeric(x)) {
                    unit(0.05, "npc")
                } else {
                    yy <- makeBars(x)
                    wmm <- max(sapply(pretty(range(yy)),
                                      function(yr)
                                      convertWidth(grobWidth(textGrob(yr, gp =
                                                                      gpar(cex = 1 *
                                                                           opts$cex.axis))),
                                                   "mm")
                                      ))
                    unit(wmm, "mm") + unit(1.5, "lines")
                }
            }
            else { # create the axis ... and measure it
                w11 <- convertHeight(grobHeight(textGrob(ylab, gp =
                                                         gpar(cex = opts$cex.lab))),
                                                "mm")
                w22 <-
                    if (is.numeric(y)) {
                      # numbers
                        wmm <-
                            max(sapply(pretty(range(y)),
                                       function(yr)
                                       convertWidth(grobWidth(textGrob(yr, gp =
                                                                       gpar(cex = 1 *
                                                                            opts$cex.axis))),
                                                    "mm")
                                       ))
                        unit(wmm, "mm") + unit(1.5, "lines")
                    } else if (is.numeric(x)) {
                      # text labels
                        wmm <- max(sapply(levels(y),
                                          function(x)
                                          convertWidth(grobWidth(textGrob(x, gp =
                                                                          gpar(cex = 1 *
                                                                               opts$cex.axis))),
                                                       "mm")
                                          ))
                        unit(wmm, "mm")
                    } else {
                        yy <- makeBars(x)
                        wmm <-
                            max(sapply(pretty(range(yy)),
                                       function(yr)
                                       convertWidth(grobWidth(textGrob(yr, gp =
                                                                       gpar(cex = 1 *
                                                                            opts$cex.axis))),
                                                    "mm")
                                       ))
                        unit(wmm, "mm") + unit(1.5, "lines")
                    }
                w11 + w22
            }
        w2 <-                    unit(1, "null")
        w3 <- if (is.numeric(y)) unit(2, "lines")  else unit(0.05, "npc")
        w4 <- if (is.null(by))   unit(0, "npc")    else convertWidth(grobWidth(leg.grob), "mm")
        widths <- unit.c(w1, w2, w3, w4 * 1.05)

        h1 <- if (is.numeric(y)) unit(5, "lines")  else unit(3, "lines")
        if (!is.null(g2.level)) h1 <- h1 + unit(1, "lines")
        h2 <-                    unit(1, "null")
        h3 <-                    unit(5, "lines")
        heights <- unit.c(h1, h2, h3)
        
        layout1 <- grid.layout(nrow = length(heights),
                               ncol = length(widths),
                               widths = widths,
                               heights = heights)

      # Check for any parameters of interest
        bg       <- opts$bg
        cex.text <- opts$cex.text
        cex.main <- opts$cex.main

        grid.newpage()
        pushViewport(viewport(layout = layout1, name = "toplevel",
                              gp = gpar(cex = cex)))
        grid.rect(gp = gpar(fill = bg))

      # --------------------------------------------------------------------------- #
      #                                             subdivide plotting region by g2

      # ========================================================================= #
      # LAYOUT 2
      # --------
      # Divide the plot up into N sub-plots
      # Check the device dimentions and put more plots along the longer axis
      # IF Y is numeric, then similar layout to lattice::xyplot(),
      # otherwise have a slight gap between columns and only x-axes
      # ========================================================================= #

        N    <- ifelse(is.null(g1), 1, length(g1.level))
        dim1 <- floor(sqrt(N))
        dim2 <- ceiling(N / dim1)

        if (dev.size()[1] < dev.size()[2]) {
            nr <- dim2
            nc <- dim1
        } else {
            nr <- dim1
            nc <- dim2
        }

      # If y is a factor, have 5% space between columns
        hspace <- if (is.numeric(y)) unit(0, "npc") else unit(0.01, "npc")
      # Repeat FULL+SPACE for each column, then remove first FULL
        widths <- rep(unit.c(hspace, unit(1, "null")), nc)[-1]

      # Never space between rows
        heights <- rep(unit(1, "null"), nr)
        
        layout2 <- grid.layout(ncol = length(widths),
                               nrow = length(heights),
                               widths = widths,
                               heights = heights)

      # Create the layout
        pushViewport(viewport(layout = layout2,
                              layout.pos.col = 2,  # position in toplevel VP
                              layout.pos.row = 2,  #
                              name = "subdivisionLayout",
                              gp = gpar(cex = sqrt(sqrt(N) / N))))
                
      # --------------------------------------------------------------------------- #
      #                                                  Draw the appropriate plots

      # set the axis limits to be the same for all plots
        xlim <-
            if (is.numeric(x)) {
                r <- range(x)
              # if the x-axis goes negative, then need to ensure it
              # gets made more negative
                neg <- r < 0
                mult <- c(-1, 1) * ifelse(neg, -1, 1)
                r + mult * 0.04 * r
            } else {
                c(0, length(levels(x)))
            }

        ylim <-
            if (is.numeric(y)) {
                r <- range(y)
                neg <- r < 0
                mult <- c(-1, 1) * ifelse(neg, -1, 1)
                r + mult * 0.04 * r
            } else if (is.null(y)) {
                if (is.numeric(x)) {
                  # to ensure we account for g1.level being set:
                    full.x.list <-
                        if (!is.null(g1))
                            lapply(levels(g1), function(l) subset(x, g1 == l))
                        else
                            list(x)
                    
                    r <- range(lapply(full.x.list,
                                      function(x) makePoints(x)$y))
                    r[2] <- max(0.2, r[2])
                    neg <- r < 0
                    mult <- c(-1, 1) * ifelse(neg, -1, 1)
                    o <- r + mult * 0.04 * (r[2] - r[1])
                } else {
                    o <- c(0, max(sapply(x.list, function(x) {
                      # need to include the error bars!
                        phat <- table(x) / length(x)
                        se <- sqrt(phat * (1 - phat) / length(x))
                        max(phat + 1.96 * se)
                    })))
                }
                o
            } else {
              # Y is a factor, so need to break x.list down even more:
                if (is.numeric(x)) {
                    x.list2 <- vector("list", length(x.list))
                    names(x.list2) <- names(x.list)
                    for (i in 1:length(x.list))
                        x.list2[[i]] <- lapply(levels(y),
                                               function(l) subset(x.list[[i]],
                                                                  y.list[[i]] == l))
                    r <-
                        range(lapply(x.list2,
                                     function(x.list)
                                     lapply(x.list,
                                            function(x) makePoints(x)$y )))
                    r[2] <- max(0.2, r[2])
                    neg <- r < 0
                    mult <- c(-1, 1) * ifelse(neg, -1, 1)
                    o <- r + mult * 0.04 * (r[2] - r[1])
                } else {
                  # need the y-values for the appropriate barplot
                    o <- c(0, max(sapply(1:length(x.list), function(i) {
                      # need to include the error bars!
                        tab <- table(x.list[[i]], y.list[[i]])
                        phat <- apply(tab, 2, function(x) x / sum(x))
                        se <- sqrt(phat * (1 - phat) /
                                   length(x.list[[i]]))
                        max(phat + 1.96 * se, na.rm = TRUE)
                    })))
                }
                o
            }

      # Cycle through all N plots. For each, draw the appropriate axes,
      # as well as a subtitle if g1 is given.

        if (is.null(g1)) {
            layout3 <- grid.layout(2, 1,
                                   heights = unit(c(0, 1), "null"))
        } else {
            subtitle <- textGrob(levels(g1)[1],
                                 gp =
                                 gpar(cex = opts$cex.lab,
                                      fontface = "bold"))
            subheight <- convertHeight(grobHeight(subtitle), "mm") * 2
            layout3 <- grid.layout(2, 1,
                                   heights = unit.c(subheight, unit(1, "null")))

        }

      # showLines: TRUE if any counts  0, otherwise FALSE
        tabs <- lapply(1:length(x.list), function(i)
                       if (is.null(y)) table(x.list[[i]])
                       else  table(x.list[[i]], y.list[[i]]))
        showLines <- any(sapply(tabs, function(x) any(x == 0)))

      # --- FOR BARPLOTS:
      # If inference is requested, need to ensure all of the scales are the same ...
        if (barplot) {
            if (!is.null(opts$inference.type)) {
              # We want the axes to be the same for all of the plots, including
              # if we only draw ONE level of g1 (still needs the same ylimits).
              # So, we need to create a temporary x/y list containing all of the
              # levels to get this information.

                x.list.tmp <-
                    if (is.null(g1)) {
                        list(all = x)
                    } else {
                        tmp <- lapply(levels(g1),
                                      function(l) subset(x, g1 == l))
                        names(tmp) <- levels(g1)
                        tmp
                    }

                if (!is.null(y)) {
                    y.list.tmp <-
                        if (is.null(g1)) {
                            list(all = y)
                        } else {
                            tmp <- lapply(levels(g1),
                                          function(l) subset(y, g1 == l))
                            names(tmp) <- levels(g1)
                            tmp
                        }
                }
                
                inference.list <- lapply(1:max(1, length(levels(g1))), function(i) {
                    y2 <- if (is.null(y)) NULL else y.list.tmp[[i]]
                    drawBarInference(x.list.tmp[[i]], y2, opts)
                })
                
                ylim <- c(0, min(1, max(sapply(inference.list, function(l) l$max))))
            } else {
                ylim <- c(0, ylim[2])
            }
        }
        
        id <- 1  # stop once all plots drawn
        for (i in nr:1) {  # start at bottom
            for (j in 1:nc) {
                if (id > N) break
                
                pushViewport(viewport(layout.pos.row = i,
                                      layout.pos.col = j * 2 - 1))
                grid.rect()

              # Decide which axes to plot:
                axis <- rep(0, 4)
                if (i == nr)
                    axis[1] <- 1
                if (i == nr & j %% 2 == 1)
                    axis[1] <- 2
                if (j == 1)
                    axis[2] <- 1
                if (j == 1 & (nr - i) %% 2 == 0)
                    axis[2] <- 2
                if (i == 1)
                    axis[3] <- 1
                if (i == 1 & j %% 2 == 0)
                    axis[3] <- 2
                if (j == nc | id == N)
                    axis[4] <- 1
                if ((j == nc | id == N) & (nr - i) %% 2 == 1)
                    axis[4] <- 2

              # Here (and only here) is the logic to decide which type of plot:
                if (is.numeric(x)) {
                  # X is a continuous variable
                    if (is.numeric(y)) {
                        iNZscatterplot(x.list[[id]], y.list[[id]],
                                       axis = axis,
                                       lab = g1.level[id],
                                       layout = layout3,
                                       xlim = xlim, ylim = ylim,
                                       col = col.list[[id]],
                                       prop.size = if (is.null(prop.size)) NULL
                                                   else prop.size.list[[id]],
                                       opts = opts)
                    } else {
                        y2 <- if (is.null(y)) NULL else y.list[[id]]

                        axis <- rep(0, 2)
                        if (i == nr) axis[1] <- 2
                        if (j == 1)  axis[2] <- 2
                        
                        iNZdotplot(x.list[[id]], y2,
                                   axis = axis,
                                   lab = g1.level[id],
                                   xlim = xlim,
                                   ylim = ylim,
                                   layout = layout3,
                                   col = col.list[[id]],
                                   opts = opts)
                    }
                } else {
                  # X is a factor
                    if (is.numeric(y)) {
                        grid.text("VERTICAL DOTPLOT")
                    } else {
                        y2 <- if (is.null(y)) NULL else y.list[[id]]

                        axis <- rep(0, 2)
                        if (i == nr) axis[1] <- 2
                        if (j == 1)
                            axis[2] <- 2
                        else if (j == nc | id == N)
                            axis[2] <- 1
                        
                        iNZbarplot(x.list[[id]], y2,
                                   axis = axis,
                                   lab = g1.level[id],
                                   by = if (is.null(by)) NULL else by.list[[id]],
                                   x.lev <- levels(x),
                                   y.lev <- if (is.null(y)) NULL else levels(y),
                                   xlim = xlim, ylim = ylim,
                                   layout = layout3,
                                   col = col.list[[id]],
                                   showLines = showLines,
                                   opts = opts)
                    }
                }
                
                upViewport()

                id <- id + 1
            }
        }
    
        upViewport()  # back to toplevel
        
      # --------------------------------------------------------------------------- #
      #                              plot the title, x and y labels, and the legend

      # Title
      # --- this consists of the names of the variables being plotted
      # --- Y versus X by G1, for G2 = G2.LEVEL
        pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 1))

        if (is.factor(x)) {
          # Need a different title for barplots:
          # Distribution of X by Y
            title1 <- paste0("Distribution of ", varnames$x)
            title2 <- ifelse(is.null(y), '', paste0(" by ", varnames$y))
        } else {
            title1 <- ifelse(is.null(y), '',
                             paste0(varnames$y, ' versus '))
            title2 <- varnames$x
        }
        title3 <- ifelse(is.null(g1), '',
                         paste0(' subset by ', varnames$g1))
        title4 <- ifelse(is.null(g2), '',
                         ifelse(is.null(g2.level), '',
                                paste0(',\n for ', varnames$g2, ' = ', g2.level)))
        if (is.numeric(x) & is.numeric(y)) {
            title5 <- ifelse(is.null(prop.size), '',
                             paste0(' (sized by ', varnames$prop.size, ')'))
        } else title5 <- ''
        
        grid.text(paste0(title1, title2, title3, title4, title5),
                  y = unit(1, "npc") - unit(0.5, "lines"),
                  just = "top",
                  gp = gpar(cex = opts$cex.main))

        upViewport()  # return to toplevel layout


      # Legend
        if (!is.null(by)) {
            pushViewport(viewport(layout.pos.col = 4, layout.pos.row = 2))
            grid.draw(leg.grob)

            upViewport()  # return to toplevel layout
        }

      # X axis label
        pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 3))
        grid.text(xlab,
                  y = unit(0.4, "npc"),
                  gp = gpar(cex = opts$cex.lab))

        upViewport()  # return to toplevel layout

      # Y axis label
        if (!is.null(y) & is.numeric(y)) {
            pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))
            grid.text(ylab,
                      x = unit(0.5, "lines"),
                      rot = 90,
                      gp = gpar(cex = opts$cex.lab))

            upViewport()  # return to toplevel layout
        }
        
    }
