% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/exportHTML.R
\name{exportHTML}
\alias{exportHTML}
\alias{exportHTML.function}
\alias{exportHTML.ggplot}
\alias{exportHTML.inzplotoutput}
\title{ExportHTML}
\usage{
exportHTML(
  x,
  file = file.path(dir, "index.html"),
  data,
  local = FALSE,
  dir = tempdir(),
  extra.vars,
  ...
)

\method{exportHTML}{`function`}(
  x,
  file = file.path(dir, "index.html"),
  data = NULL,
  local = FALSE,
  dir = tempdir(),
  extra.vars = NULL,
  width = dev.size()[1],
  height = dev.size()[2],
  ...
)

\method{exportHTML}{ggplot}(
  x,
  file = file.path(dir, "index.html"),
  data = NULL,
  local = FALSE,
  dir = tempdir(),
  extra.vars = NULL,
  mapObj,
  ...
)

\method{exportHTML}{inzplotoutput}(
  x,
  file = file.path(dir, "index.html"),
  data = NULL,
  local = FALSE,
  dir = tempdir(),
  extra.vars = NULL,
  ...
)
}
\arguments{
\item{x}{An iNZight plot object that captures iNZight environment}

\item{file}{Name of temporary HTML file generated (defaults to `index.html`
in a temporary directory, or other as specified using `dir`)}

\item{data}{dataset/dataframe that you wish to investigate and export more variables from}

\item{local}{Logical for creating local files for offline use (default to false)}

\item{dir}{A directory to store the file and output}

\item{extra.vars}{extra variables specified by the user to be exported}

\item{...}{extra arguments}

\item{width}{the desired width of the SVG plot}

\item{height}{the desired height of the SVG plot}

\item{mapObj}{iNZightMap object (from iNZightMaps)}
}
\value{
an inzHTML object consisting of a link to an HTML rendering
 of \code{x} with filename \code{file},
 which can be loaded in the browser (for example using \code{browseURL},
 or calling the \code{print()} method of the returned object.
}
\description{
\code{exportHTML} is designed to export the iNZight plot as a dynamic, interactive HTML page.
Currently only handles single panel plots. Coloured hex plots are currently not available yet.
}
\section{Methods (by class)}{
\itemize{
\item \code{exportHTML(`function`)}: method for an iNZightPlot-generating function

\item \code{exportHTML(ggplot)}: method for iNZightMaps or other supported ggplot graphs

\item \code{exportHTML(inzplotoutput)}: method for output from iNZightPlot

}}
\examples{
\dontrun{
x <- iNZightPlot(Petal.Width, Petal.Length, data = iris, colby = Species)
exportHTML(x, "index.html")

#to export more variables for scatterplots:
 exportHTML(x, "index.html", data = iris, extra.vars = c("Sepal.Length", "Sepal.Width"))
}

}
\author{
Yu Han Soh
}
