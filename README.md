# iNZightPlots
![R-CMD-check](https://github.com/iNZightVIT/iNZightPlots/workflows/R-CMD-check/badge.svg)
[![codecov](https://codecov.io/gh/iNZightVIT/iNZightPlots/branch/dev/graph/badge.svg)](https://codecov.io/gh/iNZightVIT/iNZightPlots)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)
[![CRAN](https://www.r-pkg.org/badges/version/iNZightPlots)](https://CRAN.R-project.org/package=iNZightPlots)


Graphics functions which provide a simple, easy-to-learn interface, allowing beginners to explore and visualize data easily.


## Install

Currently, install is only available from Github and depends on two other iNZight packages:
```r
devtools::install_github("iNZightVIT/iNZightTools")
devtools::install_github("iNZightVIT/iNZightMR")
devtools::install_github("iNZightVIT/iNZightPlots")

library(iNZightPlots)
```

## Usage

`iNZightPlots` provides a simple interface to graphics, which are most easily produced using the following function:
```r
iNZPlot(Sepal.Length, data = iris)
```

From there, formula notation can be used to explore relationships between variables:
```r
iNZPlot(Sepal.Length ~ Species, data = iris)
iNZPlot(Sepal.Length ~ Sepal.Width, data = iris)
iNZPlot(Sepal.Length ~ Sepal.Width | Species, data = iris)
```

These functions all produce different graphs (dot plot and scatter plot), which means the focus is on exploring data, not on which type of graph to use. Subsetting works as expected, and can include up to two variables (e.g., `y ~ x | g1 + g2`).

There are additionally two companion functions for summary and inference information, which are
```r
iNZSummary(Sepal.Length, data = iris)
iNZInference(Sepal.Length ~ Species, data = iris)
```
These use the same notation as the plot, and are intended to be used alongside.


## Features

There are many, many features available in `iNZightPlots`, all of which are added simply through additional arguments to `iNZPlot()`. For a full list, check `?iNZightPlot` (this is the base function for which `iNZPlot()` is a convenience wrapper) and `?inzpar`.


### Colour and size

Probably the most common features to add to a graph, we have the `sizeby` and `colby` arguments which map an additional variable to size and colour, respectively:
```r
iNZPlot(Sepal.Length ~ Sepal.Width, data = iris, colby = Species)
```

### Inference lines

Usually you want to know if what you see has any statistical significance, and `iNZightPlots` makes this easy. We also provide __comparison intervals__ which can be, very simply, interpreted as "overlapping comparison intervals indicate no significant difference between groups".
```r
# Add confidence intervals
iNZPlot(Sepal.Length ~ Species, data = iris,
  inference.type = c("conf", "comp"), # red and black, respectively
  inference.par = "mean"
)
```

For scatter plots, inference uses linear trends. You can add a bootstrap sample of trend estimates using the `bs.inference` argument:
```r
iNZPlot(Sepal.Length ~ Sepal.Width, data = iris,
  trend = "linear",
  bs.inference = TRUE
)
```
