\name{inzPlotDefaults}
\alias{inzPlotDefaults}
\title{
  Default Plotting Settings for iNZightPlots
}
\description{
  Contains a list of plot setting to be used by \code{iNZightPlot}. This
  includes visual control such as colour, size etc., as well as
  inference and analysis information such as trend line, smoothers, and
  bootstrapped versions of these.
}
\usage{
inzPlotDefaults()
}
\arguments{
  \item{pch}{
    the plotting symbol used. see \code{\link{par}}.
  }
  \item{col}{
    character, an overall colour. Currently I don't think this does much.
  }
  \item{col.pt}{
    character, the colour of points in dotplots and scatterplots.
  }
  \item{cex}{
    numeric, this is the overall scaling parameter for the plot.
  }
  \item{cex.pt}{
    numeric, the scaling parameter of points. NOTE: the effect is multiplicative
    with any \code{cex} value specified above.
  }
  \item{cex.lab}{
    numeric, the scaling parameter for the labels (axis labels, subplot labels,
    and legend labels).
  }
  \item{cex.axis}{
    numeric, the scaling parameter for the axes (tick marks and tick labels).
  }
  \item{cex.main}{
    numeric, the scaling parameter for the main title.
  }
  \item{cex.text}{
    numeric, the scaling parameter for any other text the appears.
  }
  \item{bg}{
    character, the background colour of the device.
  }
  \item{fill.pt}{
    character, the fill colour of points. (this only applies for \code{pch} values
    20--25).
  }
  \item{lwd}{
    numeric, the width of lines drawn on the plot (trend, etc.).
  }
  \item{lty}{
    numeric, the line type of lines (dotted, dashed, etc.).
  }
  \item{lwd.pt}{
    numeric, the width of the lines of points.
  }
  \item{col.line}{
    character, the colour of lines drawn on the plot.
  }
  \item{col.sub}{
    character, the colour of the background fill for subplot labels.
  }
  \item{jitter}{
    character, the jitter for the points. Can take the values
    \code{"x"}, \code{"y"} or \code{"xy"}.
  }
  \item{trend}{
    character vector, the trend lines to be plotted on the
    plots. Possible options are \code{c("linear", "quadratic",
      "cubic")}.
  }
  \item{smooth}{
    numeric, the smoothing required (an \code{f} value between 0 and
    1). If \code{NULL}, no smoothing line is drawn.
  }
  \item{LOE}{
    logical, the Line of Equality, \code{x = y}.
  }
  \item{join}{
    logical, if \code{TRUE}, then lines are drawn connecting points
    (only makes sense for ordered data).
  }
  \item{col.trend}{
    list, containing the colours for trend lines. If all three trend
    lines are desired, then:
    
    \code{list(linear = "blue",
               quadratic = "red",
               cubic ="green4")}
	     
    will define the appropriate colours.
  }
  \item{col.smooth}{
    character, the colour of the smoothing line.
  }
  \item{col.LOE}{
    character, the colour of the line of equality.
  }
  \item{lty.LOE}{
    numeric, the line type of the line of equality.
  }
    \item{box}{
    logical, if \code{TRUE}, then a box plot is drawn below dow plots.
  }
  \item{bow.lwd}{
    numeric vector, the line width for the box. This accepts a vector of
    up to length 2, where the first value is the width of the box, and the
    second is of the whiskers.
  }
  \item{box.col}{
    character, the colour of the box lines.
  }
  \item{box.fill}{
    character, the fill colour of the box.
  }
  \item{bar.lwd}{
    numeric, the width of bar plot lines.
  }
  \item{bar.col}{
    character, the colour of bar plot lines.
  }
  \item{bar.fill}{
    character, the fill colour of bar plots.
  }
  \item{inf.lwd.comp}{
    numeric, the width of comparison inference lines.
  }
  \item{inf.col.comp}{
    character, the colour of comparison inference lines.
  }
  \item{inference.type}{
    character vector, the type of inference intervals required. One or
    both of \code{c("comp", "conf")}. If \code{NULL}, then no inference
    intervals are drawn.
  }
  \item{inference.par}{
    character, if \code{inference.type} is not \code{NULL}, then the
    inference is supplied to the desired parameter. One of
    \code{"mean"} or \code{"median"} for dotplots, or \code{"proportion"}
    for barplots. If left \code{NULL} when \code{inference.type} is
    supplied, then uses the median for dotplots and proportion for
    barplots. NB: This can take a vector of possibilities but it doens't
    make any difference (at the moment anyway).
  }
  \item{bs.inference}{
    logical, if \code{TRUE}, then bootstrap inference is perfomed for
    any inference added to the plot (e.g., trend lines, confidence
    intervals, etc.).
  }
  \item{n.boot}{
    the number of bootstrap simulations to do.
  }
}
\details{
  This function currently takes no value, as it simply sets the initial
  values. These can then be accessed from anywhere inside iNZightPlot,
  unless they are overridden.

  The possible values for each attribute are outlined in the
  \code{arguments} section above.
}
\value{
  A list.
}
\author{
  Tom Elliott
}

\seealso{
  \code{\link{iNZightPlot}}
}
