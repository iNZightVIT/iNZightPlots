\name{iNZightPlot}
\alias{iNZightPlot}
\title{
  iNZight Plot for Exploratory Data Analysis
}
\description{
  iNZightPlot provides a simple function to allow users to
  quickly obtain appropriate plots of their data. There is the ability to
  supply continuous or factor variables, as well as several methods of
  subsetting the data. The plots are either dotplots, scatter plots, or
  bar charts --- depending on the type of variables supplied.
}
\usage{
iNZightPlot(x, y = NULL, g1 = NULL, g2 = NULL,
            g1.level = NULL, g2.level = NULL,
            varnames = list(),
            xlab = varnames$x, ylab = varnames$y,
            by = NULL, prop.size = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{
    numeric or factor, an explanatory variable.
  }
  \item{y}{
    numeric or factor, a response variable, or a variable of interest.
  }
  \item{g1}{
    a grouping variable. If numeric, then converted into a 4-level
    factor variable. A separate plot is drawn for every level of \code{g1},
    unless \code{g1.level} is supplied.
  }
  \item{g2}{
    a second grouping variable. Currently, this only provides the option
    to subset the entire dataset and plot only the level of \code{g2}
    selected by \code{g2.level}.
  }
  \item{g1.level}{
    character or numeric. If a character vector is supplied, then only
    the specified levels of \code{g1} are plotted. If a numeric, then
    the respective levels of \code{g1} are plotted (NOTE: this option
    depends on the order of the levels!). If \code{0} or \code{NULL},
    then all levels are plotted.
  }
  \item{g2.level}{
    character or numeric. Similar to \code{g1.level}, however only the
    data values corresponding to this level of the factor are
    drawn. Currently only accepts a single value.
  }
  \item{varnames}{
    an optional list of variable names to be used in the plot. Each
    element of the list must be named by the appropriate argument name
    (see Details below).
  }
  \item{xlab}{
    a label to be drawn on the x axis.
  }
  \item{ylab}{
    a label to be drawn on the y axis.
  }
  \item{by}{
    a variable used to code a further factor (or numeric, converted to a
    factor) by differing colours.
  }
  \item{prop.size}{
    a variable used to resize the points in a scatter plot.
  }
  \item{\dots}{
    further arguments to be passed to the plotting functions. See
    \code{inzPlotDefaults()} for a full list of arguments.
}
}
\details{
  The iNZightPlot function allows simple data exploration for users who
  want to spend less time writing the correct R-code to obtain the
  correct plot. It automatically draws a plot based on the type of data
  supplied. For factors, bar charts are drawn; for numeric, scatter
  plots and dotplots.

  There is also a large amount of extra functionality included in the
  function, which allows users to subset the data in several ways, vary
  colors, sizes, and many other aspects of the plot. More information
  about this can be found in \code{iNZpar}.
}
\value{
  None.
}
\author{
  Tom Elliott. Parts have been copied from the original iNZight, written
  by Dineika Chandranada, Simon Potter, and David Banks.
}

\seealso{
  \code{\link[iNZight]{iNZight}}
}