iNZgridplot <-
    function(x, y, axis = c(0, 0, 0, 0), lab = NULL,
             layout, xlim = range(x), ylim = range(y),
             col = opts$col.pt, prop.size = NULL, opts) {
  # --------------------------------------------------------------------------- #
  # Makes a discrete 'grid' plot when sample sizes are large.
  # Creates a gridand fills boxes with colour based on the number of
  # points in the box.
  # --------------------------------------------------------------------------- #

    pushViewport(viewport(layout = layout,
                          xscale = xlim))

  # --------------------------------------------------------------------------- #
  #                                                             Draw the x-axes
  # these need to be ABOVE the labels
    agp <- gpar(cex = opts$cex.axis)
    if (axis[1] == 1)
        grid.xaxis(label = FALSE, gp = agp)
    if (axis[1] == 2)
        grid.xaxis(gp = agp)
    if (axis[3] == 1)
        grid.xaxis(main = FALSE, label = FALSE, gp = agp)
    if (axis[3] == 2)
        grid.xaxis(main = FALSE, gp = agp)

  # --------------------------------------------------------------------------- #
  #                                                            Add the subtitle
    if (!is.null(lab)) {
        pushViewport(viewport(layout.pos.row = 1))
        grid.rect(gp = gpar(fill = opts$col.sub))
        grid.text(lab, gp = gpar(cex = opts$cex.lab))
        upViewport()
    }

  # =========================================================================== #
  # Start main plot
    pushViewport(viewport(layout.pos.row = 2,
                          xscale = xlim, yscale = ylim))

  # --------------------------------------------------------------------------- #
  #                                                                  Add y-axes
    if (axis[2] == 1)
        grid.yaxis(label = FALSE, gp = agp)
    if (axis[2] == 2)
        grid.yaxis(gp = agp)
    if (axis[4] == 1)
        grid.yaxis(main = FALSE, label = FALSE, gp = agp)
    if (axis[4] == 2)
        grid.yaxis(main = FALSE, gp = agp)

  # --------------------------------------------------------------------------- #
  #                                                        Draw the scatterplot

  # Check that there are some points to plot:
    if (length(x) > 0) {
        pushViewport(viewport(clip = "on",
                              xscale = xlim,
                              yscale = ylim))  # so nothing goes outside the box

      # Set up the grid
        Npt <- 100
        scatter.grid <- matrix(0, nrow = Npt, ncol = Npt)
        xx <- cut(x, Npt)
        yy <- cut(y, Npt)
        scatter.grid <- as.matrix(table(yy, xx))[Npt:1, ]

        shade <- heat.colors(n = max(scatter.grid) + 1)[c(scatter.grid) + 1]

        xv = (rep(1:Npt, each = Npt) - 0.5) / Npt
        yv = (rep(Npt:1, Npt) - 0.5) / Npt
     #   zv = 

        grid.xaxis()
        grid.yaxis()

      #  grid.points(unit(xv, "npc"), unit(yv, "npc"),
      #              size = unit(1 / Npt, "npc"), pch = 15,
      #              gp = gpar(col = shade))
        grid.rect(gp = gpar(fill = levels(as.factor(shade))[1]))
        invisible(lapply((1:length(xv))[c(scatter.grid) != 0], function(i) {
            grid.rect(unit(xv[i], "npc"), unit(yv[i], "npc"),
                      width = unit(1 / Npt, "npc"), height = unit(1 / Npt, "npc"),
                      gp = gpar(fill = shade[i], lwd = 0))
        }))

       # for (i in 1:100) {
       #     for (j in 100:1) {
       #         if (shade[i, j] == 0) next
       #         grid.rect(x = (i - 1) / 100, y = j / 100,
       #                   width = 0.01, height = 0.01,
       #                   just = c("left", "top"),
       #                   gp =
       #                   gpar(fill = rgb(1 - shade[i, j], 1-shade[i,j], 1-shade[i,j]),
       #                        lwd = 0))
       #     }
       # }

        upViewport()  # end clipping
    }
    
    upViewport()  # end main plot
    upViewport()  # end this subplot
}
