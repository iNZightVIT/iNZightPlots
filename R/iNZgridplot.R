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

       # hcols <- rev(heat.colors(n = max(scatter.grid) + 1))
       # hcols <- rainbow(n = max(scatter.grid) + 1)[c(scatter.grid) + 1]
        hcols <- hcl(0, 0, seq(100, 0, length = max(scatter.grid) + 1))
        shade <- hcols[c(scatter.grid) + 1]

        xv = (rep(1:Npt, each = Npt) - 0.5) / Npt
        yv = (rep(Npt:1, Npt) - 0.5) / Npt

        grid.xaxis()
        grid.yaxis()

        is0 <- c(scatter.grid) == 0

        grid.rect(gp = gpar(fill = hcols[1]))
        grid.points(unit(xv[!is0], "npc"), unit(yv[!is0], "npc"),
                    size = unit(1 / Npt, "npc") * 1.35, pch = 15,
                    gp = gpar(col = shade[!is0]))

        upViewport()  # end clipping
    }
    
    upViewport()  # end main plot
    upViewport()  # end this subplot
}

test <- function() {
    d <- read.csv("~/iNZight/data/Census at School-500.csv")
    x = d$armspan
    y = d$height
    iNZgridplot(x, y)
}
