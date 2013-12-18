\name{iNZightPlot}
\alias{iNZightPlot}
\title{
  iNZight Plot for Exploratory Data Analysis
}
\description{
  The iNZightPlot function provides a simple function to allow users to
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
            by = NULL, ...)
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
  \item{\dots}{
    further arguments to be passed to the plotting functions. See
    \code{inzPlotDefaults()} for a full list of arguments.
}
}
\details{
  This will be writted later.
}
\value{
  None.
}
\author{
  Tom Elliott. Based off of the iNZight functions written by Dineika
  Chandrananda and others.
}

\seealso{
  \code{\link[iNZight]{iNZight}}
}