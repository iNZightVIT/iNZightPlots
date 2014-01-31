iNZmatrix <-
function(x, y = NULL, g1 = NULL, g2 = NULL,
             g1.level = NULL, g2.level = NULL,
             varnames = list(), xlab = varnames$x, ylab = varnames$y,
             by = NULL, prop.size = NULL, 
             opts) {
  # --------------------------------------------------------------------------- #
  # Produces a matrix of each level of g1 plotted against each level of g2
  # Checking: - if g1.level is a single level, then send back with g2 = g1
  # --------------------------------------------------------------------------- #

    if (is.null(g1.level)) g1.level <- levels(g1)
    
    if (length(g1.level) == 1)
        if (g1.level != "_MULTI") {
            v <- varnames
            v$g1 <- varnames$g2
            v$g2 <- varnames$g1
            arglist <- c(list(x = x, y = y, g1 = g2, g2 = g1,
                              g1.level = g2.level, g2.level = g1.level,
                              varnames = v, xlab = xlab, ylab = ylab,
                              by = by, prop.size = prop.size),
                         opts)
            do.call(iNZightPlot, arglist)
            return(invisible(NULL))
        }

  # Now we do all the fancy stuff.

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

  # For barplots, need to code y as by for colouring & legend:
    if (barplot) {
        if (!is.null(y)) {
            by <- y
            varnames$by <- varnames$y
        }
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

      # decide on a scaling factor for the legend
        cex.mult <- ifelse(is.null(g1), 1,
                           ifelse(is.null(g1.level),
                                  ifelse(length(levels(g1)) >= 6, 0.7, 1), 1))
        
      # design the legend
        legend <- list(labs      = levels(by),
                       cols      = if (barplot) rep(opts$bar.col, length = n.by)
                       else col.pt,
                       pch       = ifelse(barplot, 22, pch),
                       cex       = cex.text,
                       cex.pt    = cex,
                       cex.title = cex.lab,
                       cex.mult  = cex.mult,
                       lwd       = ifelse(barplot, opts$bar.lwd, opts$lwd.pt),
                       title     = varnames$by,
                       fill      = cols)
        
        leg.grob <- drawLegend(legend)
    } else {
        cols <- rep(col.pt, length = length(x))
    }

  # --------------------------------------------------------------------------- #
  #                                                      Subdivide by g1 and g2

    if (is.numeric(g1)) g1 <- convert.to.factor(g1)
    if (is.numeric(g2)) g2 <- convert.to.factor(g2)
    if (length(g2.level) == 1) g2.level <- levels(g2)
    if (length(g1.level) == 1) g1.level <- levels(g1)

    x.list <- structure(vector("list", length(g1.level)),
                        .Names = g1.level)
    x.list <- lapply(x.list,
                     function(x) structure(vector("list", length(g2.level)),
                                           .Names = g2.level))

    y.list <- by.list <- col.list <- prop.size.list <- x.list

    for (G1 in g1.level) {
        for (G2 in g2.level) {
          # break down by each level
            w <- g1 == G1 & g2 == G2
          # ---
            x.list[[G1]][[G2]] <- x[w]
            y.list[[G1]][[G2]] <- y[w]
          # ---
            if (!is.null(by))
                by.list[[G1]][[G2]] <- by[w]
          # ---
            if (barplot)
                col.list[[G1]][[G2]] <- cols
            else
                col.list[[G1]][[G2]] <- cols[w]
          # ---
            if (!is.null(prop.size))
                prop.size.list[[G1]][[G2]] <- prop.size[w]
        }
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

    grid.newpage()
    pushViewport(viewport(gp = gpar(cex = 0.8)))
    
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
    
    h1 <- if (is.numeric(y)) unit(3, "lines")  else unit(3, "lines")
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
    
    upViewport()
    pushViewport(viewport(layout = layout1, name = "toplevel",
                          gp = gpar(cex = cex)))
    grid.rect(gp = gpar(fill = bg))

  # --------------------------------------------------------------------------- #
  #                                             subdivide plotting region by g1
    
  # ========================================================================= #
  # LAYOUT 2
  # --------
  # Divide the plot up into nr x nc sub-plots
  # G1 in rows; G2 in columns
  # IF Y is numeric, then similar layout to lattice::xyplot(),
  # otherwise have a slight gap between columns and only x-axes
  # Additionally, need headings along each ROW specifying the level of that
  # factor.
  # ========================================================================= #

    nr <- length(g1.level)
    nc <- length(g2.level)
    N <- nr * nc

  # If y is a factor, have 5% space between columns
    hspace <- if (is.numeric(y)) unit(0, "npc") else unit(0.01, "npc")
  # Repeat FULL+SPACE for each column, then remove first FULL
    widths <- rep(unit.c(hspace, unit(1, "null")), nc)[-1]

    subtitle <- textGrob(levels(g1)[1],
                         gp =
                         gpar(cex = opts$cex.lab,
                              fontface = "bold"))
    subheight <- convertHeight(grobHeight(subtitle), "mm") * 2 * sqrt(sqrt(N) / N)

  # Never space between rows
    heights <- rep(unit.c(subheight, unit(1, "null")), nr)    
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
    
  # Cycle through all N plots. For each, draw the appropriate axes,
  # as well as a subtitle
    
    layout3 <- grid.layout(2, 1,
                           heights = unit.c(subheight, unit(1, "null")))
  # set the axis limits to be the same for all plots
    xlim <-
        if (is.numeric(x)) {
            r <- range(x)
          # if the x-axis goes negative, then need to ensure it
          # gets made more negative
            mult <- c(-1, 1)
            r + mult * 0.04 * diff(r)
        } else {
            c(0, length(levels(x)))
        }
    
    ylim <-
        if (is.numeric(y)) {
            r <- range(y)
            neg <- r < 0
            mult <- c(-1, 1) * ifelse(neg, -1, 1)
            r + mult * 0.04 * diff(r)
        } else if (is.null(y)) {
            if (is.numeric(x)) {
              # to ensure we account for g1.level being set:
                r <- range(lapply(x.list,
                                  function(X) {
                                      lapply(X,
                                             function(x) {
                                                 makePoints(x, xlim = xlim,
                                                            opts = opts)$y
                                             })
                                  }))
                r[2] <- max(0.001, r[2])
                neg <- r < 0
                mult <- c(-1, 1) * ifelse(neg, -1, 1)
                o <- r + mult * 0.04 * (r[2] - r[1])
            } else {
                o <- c(0, max(sapply(x.list,
                                     function(X) {
                                         max(sapply(X,
                                                function(x) {
                                                  # need to include the error bars!
                                                    phat <- table(x) / length(x)
                                                    se <- sqrt(phat * (1 - phat) / length(x))
                                                    max(phat + 1.96 * se)
                                                }))
                                         })))
            }
            o
        } else {
          # Y is a factor, so need to break x.list down even more:
            if (is.numeric(x)) {
                r <- c(0, 0)                
                for (i in 1:length(x.list)) {
                    for (j in 1:length(x.list[[i]])) {
                        xl <- x.list[[i]][[j]]
                        yl <- y.list[[i]][[j]]
                        x.tmp <- lapply(levels(y), function(l) subset(xl, yl == l))
                        rr <- range(lapply(x.tmp,
                                           function(x) makePoints(x, xlim = xlim,
                                                                  opts = opts)$y))
                        r <- range(rr, r)
                    }
                }
                                 
                r[2] <- max(0.001, r[2])
                neg <- r < 0
                mult <- c(-1, 1) * ifelse(neg, -1, 1)
                o <- r + mult * 0.04 * (r[2] - r[1])
            } else {
              # need the y-values for the appropriate barplot
                up <- 0
                for (i in 1:length(x.list)) {
                    for (j in 1:length(x.list[[i]])) {
                        tab <- table(x.list[[i]][[j]], y.list[[i]][[j]])
                        phat <- apply(tab, 2, function(x) x / sum(x))
                        se <- sqrt(phat * (1 - phat) /
                                   length(x.list[[i]][[j]]))
                        up <- max(up, max(phat + 1.96 * se, na.rm = TRUE))
                    }
                }
                o <- c(0, up)
            }
            o
        }

    seekViewport("subdivisionLayout")  # future proofing temporarily ...


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
    
    for (i in 1:nr) {
        pushViewport(viewport(layout.pos.row = (i * 2) - 1))
        grid.rect(gp = gpar(fill = "lightblue"))  # the g1 level subtitle
        grid.text(g1.level[i], gp = gpar(cex = opts$cex.lab))
        upViewport()
        
        for (j in 1:nc) {
            pushViewport(viewport(layout.pos.row = (i * 2),
                                  layout.pos.col = (j * 2) - 1))
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
            if (j == nc)
                axis[4] <- 1
            if ((j == nc) & (nr - i) %% 2 == 1)
                axis[4] <- 2
            
          # Here (and only here) is the logic to decide which type of plot:
            if (is.numeric(x)) {
              # X is a continuous variable
                if (is.numeric(y)) {
                    iNZscatterplot(x.list[[i]][[j]], y.list[[i]][[j]],
                                   axis = axis, ax.add = subheight,
                                   lab = g2.level[j],
                                   layout = layout3,
                                   xlim = xlim, ylim = ylim,
                                   col = col.list[[i]][[j]],
                                   prop.size = if (is.null(prop.size)) NULL
                                   else prop.size.list[[i]][[j]],
                                   opts = opts)
                } else {
                    y2 <- if (is.null(y)) NULL else y.list[[i]][[j]]
                    
                    axis <- rep(0, 2)
                    if (i == nr) axis[1] <- 2
                    if (j == 1)  axis[2] <- 2
                    
                    iNZdotplot(x.list[[i]][[j]], y2,
                               axis = axis,
                               lab = g2.level[j],
                               xlim = xlim,
                               ylim = ylim,
                               layout = layout3,
                               col = col.list[[i]][[j]],
                               opts = opts)
                }
            } else {
              # X is a factor
                if (is.numeric(y)) {
                    grid.text("VERTICAL DOTPLOT")
                } else {
                    y2 <- if (is.null(y)) NULL else y.list[[i]][[j]]
                    
                    axis <- rep(0, 2)
                    if (i == nr) axis[1] <- 2
                    if (j == 1)
                        axis[2] <- 2
                    else if (j == nc)
                        axis[2] <- 1
                    
                    iNZbarplot(x.list[[i]][[j]], y2,
                               axis = axis,
                               lab = g2.level[j],
                               by = if (is.null(by)) NULL else by.list[[i]][[j]],
                               x.lev <- levels(x),
                               y.lev <- if (is.null(y)) NULL else levels(y),
                               xlim = xlim, ylim = ylim,
                               layout = layout3,
                               col = col.list[[i]][[j]],
                               showLines = showLines,
                               opts = opts)
                }
            }
            
            grid.rect(gp = gpar(fill = "transparent"))

            upViewport()
        }
    }
    
    upViewport()  # back to toplevel

  # --------------------------------------------------------------------------- #
  #                              plot the title, x and y labels, and the legend
    
  # Title
  # --- this consists of the names of the variables being plotted
  # --- Y versus X by G1, for G2 = G2.LEVEL
    pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 1,
                          gp = gpar(cex = 0.8)))
    
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
    title3 <- paste0(' subset by ', varnames$g1, ' and ', varnames$g2)
    title4 <- ''
    if (is.numeric(x) & is.numeric(y)) {
        title5 <- ifelse(is.null(prop.size), '',
                         paste0('\n(sized by ', varnames$prop.size, ')'))
    } else title5 <- ''
    
    grid.text(paste0(title1, title2, title3, title4, title5),
              y = unit(1, "npc") - unit(0.5, "lines"),
              just = "top",
              gp = gpar(cex = opts$cex.main))
    
    upViewport()  # return to toplevel layout


  # Legend
    if (!is.null(by)) {
        pushViewport(viewport(layout.pos.col = 4, layout.pos.row = 2,
                              gp = gpar(cex = 0.8)))
        grid.draw(leg.grob)
        
        upViewport()  # return to toplevel layout
    }
    
  # X axis label
    pushViewport(viewport(layout.pos.col = 2, layout.pos.row = 3,
                          gp = gpar(cex = 0.8)))
    grid.text(xlab,
              y = unit(0.4, "npc"),
              gp = gpar(cex = opts$cex.lab))
    
    upViewport()  # return to toplevel layout
    
  # Y axis label
    if (!is.null(y) & is.numeric(y)) {
        pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2,
                              gp = gpar(cex = 0.8)))
        grid.text(ylab,
                  x = unit(0.5, "lines"),
                  rot = 90,
                  gp = gpar(cex = opts$cex.lab))
        
        upViewport()  # return to toplevel layout
    }
    

  # --------------------------------------------------------------------------- #
  #                                                                Return value
    
    ret <-
        list(x = x, y = y, g1 = g1, g2 = g2, g1.level = g1.level,
             g2.level = g2.level, varnames = varnames, xlab = xlab,
             ylab = ylab, by = by, prop.size = prop.size,
             other = opts)
    class(ret) <- "iNZightObject"
  # ret
    
    return(invisible(NULL))
}

