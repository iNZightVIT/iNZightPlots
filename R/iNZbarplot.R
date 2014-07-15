iNZbarplot <-
    function(x, y = NULL, axis = c(0, 0), by = NULL, lab = NULL, x.lev, y.lev = NULL,
             layout, xlim, ylim, col = opts$col.bar, showLines = FALSE, opts) {
  # --------------------------------------------------------------------------- #
  # Makes a bar plot of the supplied X data, possibly broken down by Y
  # Can only be called from the iNZplot() function.
  # Includes the functionlity to:
  #  - draw the appropriate axes, depending on the position in the layout grid
  #  - draws the subtitle if grouping variable 1 is being defined
  #  - add any additional features
  # --------------------------------------------------------------------------- #
        
    pushViewport(viewport(layout = layout,
                          xscale = xlim))

  # --------------------------------------------------------------------------- #
  #                                                             Draw the x-axes
    if (axis[1] == 2) {
        labText <- textGrob(x.lev,
                            x = unit((0:length(x.lev))[-1] - 0.5, "native"),
                            y = unit(-1, "lines"),
                            gp = gpar(cex = opts$cex.axis),
                            name = "labelText")  # label is important!

        wm <- which.max(nchar(as.character(x.lev)))
        tt <- textGrob(levels(x)[wm])
      # save label widths
        labwid <- convertWidth(grobWidth(tt), "mm", valueOnly = TRUE)  
    }
    
  # --------------------------------------------------------------------------- #
  #                                                            Add the subtitle
    if (!is.null(lab)) {
        pushViewport(viewport(layout.pos.row = 1))
        grid.rect(gp = gpar(fill = opts$col.sub))
        grid.text(lab, gp = gpar(cex = opts$cex.lab))
        upViewport()
    }

  # =========================================================================== #
  #                                                             Start main plot

  # COMPARISON INTERVALS: these are essentially phat \pm 1.96 * se(phat)
  # CONFIDENCE INTERVALS: these are confidence intervals for the phats
  # For both of these intervals, they are only drawn if there are at
  # least 5 observations in the group.
    if (!is.null(opts$inference.type)) {
        inference.lines <- drawBarInference(x, y, opts = opts)
    }

    pushViewport(viewport(layout.pos.row = 2,
                          xscale = xlim,
                          yscale = ylim))

  # --------------------------------------------------------------------------- #
  #                                                      set up layout for bars

    layout4 <- grid.layout(nrow = 1, ncol = length(x.lev))
    pushViewport(viewport(layout = layout4))

  # --------------------------------------------------------------------------- #
  #                                                               Add the yaxis

    pushViewport(viewport(yscale = ylim * 1.05))
    ypos <- pretty(ylim)
    ypos <- ypos[ypos != 0]
    ypos <- ypos[ypos < max(ylim) * 1.05]
    if (axis[2] == 2) {
        grid.yaxis(at = ypos, label = paste0(100 * ypos, '%'),
                   gp = gpar(cex = opts$cex.axis))
    } else if (axis[2] == 1) {
        grid.yaxis(at = ypos, label = paste0(100 * ypos, '%'),
                   main = FALSE,
                   gp = gpar(cex = opts$cex.axis))
    }
    upViewport()


    
    if (!is.null(y)) {
      # calculate multiple bars, multiple x-points
        hgt <- makeBars(x, y)
    } else {
        hgt <- makeBars(x)
    }
    
    for (i in 1:length(x.lev)) {
        pushViewport(viewport(layout.pos.col = i,
                              xscale = c(-0.1, 1.1),
                              yscale = ylim * 1.05))

      # For ease of reading, draw thin vertical lines between x-groups if any of
      # the counts are 0
      # ALTERNATIVELY show even columns in a light grey
       # if (showLines) {
       #    # grid.rect(gp = gpar(lwd = 0.1, lty = 3))
       #     if (i %% 2 == 0) {
       #       # grid.rect(gp = gpar(fill = "grey95", lwd = 0))
       #       # need to cover over the border
       #       #  grid.lines(x = c(0, 1), y = 1)
       #       #  grid.lines(x = c(0, 1), y = 0)
       #     }
       # }

       # Commented out because .... because!

   # --------------------------------------------------------------------------- #
   #                                                                   Draw bars

   # There is a single bar in each plotting region (i going along the
   # x-axis). If only one bar is drawn, it is drawn in the middle. If
   # multiple bars are drawn, then the area must be broken into equal
   # bars for the levels of y. The bars must also be coloured.
        
        if (i == 1)  # save the width of bars
            barwid <- convertWidth(unit(1, "npc"), "mm", valueOnly = TRUE)
        
        if (is.null(y)) {
          # Plotting a single bar for each level of g1
            xx <- 0.5

          # If by is set, then the bar needs to be segmented!
            if (is.null(by)) {
                grid.rect(x = xx, y = 0,
                          height = unit(hgt[i], "native"),
                          width = unit(1, "native"),
                          just = "bottom",
                          gp =
                          gpar(fill = opts$bar.fill, col = opts$bar.col,
                               lwd = opts$bar.lwd))
            } else {
              # Calculate the relative proportions for each group:
                newhgt <- makeBars(by, x)
                
                for (s in 1:ncol(newhgt)) {
                    H <- hgt[i]  # the overall height of the bar
                    S <- c(0, cumsum(newhgt[i, ]) * H)  # the location of segments

                    for (t in 2:length(S)) {
                        grid.rect(x = xx,
                                  y = unit(S[t - 1], "native"),
                                  height = unit(S[t] - S[t - 1], "native"),
                                  width = unit(1, "native"),
                                  just = "bottom",
                                  gp =
                                  gpar(fill = col[t - 1], col = opts$bar.col,
                                       lwd = opts$bar.lwd))
                    }
                }
            }
            
        } else {
          # Plotting a bar for each level of y, for each level of g1
            
          # Calculate the barwidths
            wd <- table(y)
            wd <- wd / sum(wd) * 0.9      # add spacing between bar groups

            x.right <- cumsum(wd) + 0.05  # shift bars to center of panel
            x.left <- x.right - wd
            xx <- x.right - wd / 2        # bar mids
            
            yy <- hgt[, i]

          # sort out the colours
            cols <- if (length(col) < nrow(hgt)) opts$bar.col else col
            
            if (length(cols) < nrow(hgt)) {
                col <- hcl(1:nrow(hgt) / nrow(hgt) * 360, c = 80, l = 50)
            } else {
                col <- cols[1:nrow(hgt)]
            }

            grid.rect(x = x.left, y = 0,
                      height = unit(yy, "native"),
                      width = wd,
                      just = c("left", "bottom"),
                      gp =
                      gpar(fill = col, col = "white", # opts$bar.col,
                           lwd = opts$bar.lwd))

        }

      # These two lines just to make it easier to see bar groups
        grid.lines(c(0, 1), y = 0, gp = gpar(col = "white"))
        grid.lines(c(0.05, 0.95), y = 0)

  # --------------------------------------------------------------------------- #
  #                                       Add inference information to the bars

      # We only want guide lines for comparison intervals
        guides <-
            if (is.null(opts$inference.type))
                "none"
            else if ("comp" %in% opts$inference.type)
                ifelse(is.null(y), "all", "group")
            else
                "none"  # nothing for confidence intervals
        
        if (!is.null(opts$inference.type)) {
            drawInferenceLines(inference.lines, i, xx, opts,
                               guides = guides)

          # If y is null, then we add all guides
            if (guides == "all")
                g.loc <- c(inference.lines$comp$low, inference.lines$comp$upp)
        }
        
        upViewport()        
    }

  # Rotate labels maybe; and make them smaller if they still don't fit in the axis space
    if (axis[1] == 2) {
        if (labwid > barwid) {
            labText <- editGrob(labText, y = unit(-0.5, "mm"),
                                rot = 30, just = c("right", "top"))
          #  grid.edit("labelText", y = unit(-0.5, "mm"),
          #            rot = 30, just = c("right", "top"))
            labheight <- convertHeight(grobHeight(labText), "lines", valueOnly = TRUE)
            if (labheight > 5)
                labText <- editGrob(labText,
                                    gp = gpar(cex = 5 / labheight * opts$cex.axis))
        }

        pushViewport(viewport(xscale = xlim))
        grid.draw(labText)
        upViewport()
    }


    if (guides == "all") {
      # Draw inference guide lines
        pushViewport(viewport(yscale = ylim * 1.05))
        guides <- g.loc[!is.na(g.loc)]
        
        if (length(guides) > 0)
            grid.polyline(x = rep(c(0, 1), length(guides)),
                          y = unit(rep(guides, each = 2), "native"),
                          id = rep(1:length(guides), each = 2),
                          gp = gpar(lty = 3, col = "grey50", lwd = 0.5))
        
        upViewport()
    }
    
    upViewport()  # back to layout4
    upViewport()  # back to sub plot/layout3
    upViewport()  # back to layout2
}

