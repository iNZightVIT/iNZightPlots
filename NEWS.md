# iNZightPlots 2.15.3

- modify tests that were randomly creating spurious messages on CRAN

# iNZightPlots 2.15.1

- fix a bug in `inzinference()` where spaces in categories led to some weird formatting
- update some deprecated functions and arguments from 'dplyr' and 'ggplot2'

# iNZightPlots 2.15.0

- new `multiplot()` method for handling multiple `y` variables, so formulas like `inzplot(~x1 + x2 + x3, ...)` are now supported
- ability to specify default plots types with `options(inzight.default.palette.cat, inzight.default.palette.cont)`
- new plot modifications:
  - mean indicator on gg box plots
  - mean indicator on gg density plots
- handle variable labels (from 'expss' package) in plot titles/axes/legends
- handle units attached to variables

# iNZightPlots 2.14.4

- fix bug in CI width calculation for two-way comparisons

# iNZightPlots 2.14.3

- fix bug where survey smoothers were not plotting (#291)

# iNZightPlots 2.14.2

- fix bug introduced by fix in 2.14.1

# iNZightPlots 2.14.1

- fix bug in surveys where ID not consecutive
- fix bug in surveys using reserved column names (x, y, etc)

# iNZightPlots 2.14

## Major changes

- add `ci.width` argument to `inzpar()` for plots and inferences for their confidence intervals
- allow specification of random rounding and suppression rules (developmental feature)
  - one- and two-way tables
  - suppress values based on small counts/large RSE, and other privacy controls
- add `table.direction` argument to `inzsummary()` to display tables horizontally (default) or vertically
- reorganise output for inference (differences, CIs, and p-values) into a single table, rather than 2 or 3 matrices
- one- and two-way tables' inference displays a single table, and uses correct method for calculating CIs for differences in proportions in a survey design

## Minor changes

- survey anova inference now provides CIs and adjusted p-values using 'emmeans' package
- clarify wording for two-way difference tables

## Bug fixes

- suppress warnings from `RColorBrewer::brewer.pal` if `n <= 2`
- fix bug in ggplot2 plots where discrete axis ordering was different to a regular iNZightPlot (#270, @daniel-barnett)

# iNZightPlots 2.13.5

- patch issue with summary/inference if the graphics device has minimal/no dimensions

# iNZightPlots 2.13.4

- fix bug in `inzsummary` and `inzinference` for survey regression output (response/predictor wrong way around)
- disable plots for surveys with x and/or y with only 1 level (#258, @tmelliott)
- fix handling of axis transformations for survey data (#227, @tmelliott)

# iNZightPlots 2.13.3

- update use of `svyquantile` for changes in 'survey' 4.1

# iNZightPlots 2.13.2

- fix CRAN issues on R-devel (windows), and M1 mac (all related to tests)

# iNZightPlots 2.13.1

- fix issue in exporting as HTML with local assets (@tmelliott, #252)

# iNZightPlots 2.13

- replace `inzsummary` and `inzinference` with S3 methods
- only show survey information (exclude the full call) in summary/inference
- fix bug where plots failed if there we no non-missing x/y values in a subset
- fix issue introduced by change in 'readr' package (uses 'hms' instead of 'time')
- fix y-axis rendering of large values (now accounts for width of tick labels)

# iNZightPlots 2.12.6

- fix wording in inference information output ('mean' -> 'estimate') (@tmelliott, #242)

# iNZightPlots 2.12.5

- add Mantel-Haenszel tests when `g1` is specified in `getPlotSummary`
- fix ordering of anova output (col - row instead of row - col)

# iNZightPlots 2.12.4

- pass `na.rm = TRUE` to survey methods so summaries and inference show the necessary information
- fix bug in `inzplot()` causing axes to be the wrong way around in hex plots for surveys
- `smooth` is respected for survey data (passed to `svysmooth`)

# iNZightPlots 2.12.3

- fix bug in survey inference where levels were the wrong way around (A - B instead of B - A)

# iNZightPlots 2.12.2

- fix bug in plots with axis range of zero (i.e., all x/y-values equal)

# iNZightPlots 2.12.1

- fix bug so mean indicator on dot plots works with surveys

# iNZightPlots 2.12

This release mainly consists of compatibility changes to accompany 'iNZight' 4.0.

## Breaking changes

- New `inzplot`, `inzsummary`, and `inzinference` replace `iNZPlot`, `iNZSummary`, and `iNZInference`, providing a better interface to the package which better first the R ecosystem.

## Minor changes

- refactors exportHTML() and exportSVG() to writing file to (by default) a temporary directory, and without changing directories in the process
- handle `col.emph`, `col.emphn`, and (by default) plot emphasized points on top
- allow `col.fun` to be a character palette name
- when dot plots show inference, override `boxplot` and `mean_indicator` (depending on specification of `inference.par`)
- add normal inference to scatter plot trend lines
- (survey) pop size estimates shown even if weights seem odd

## Bug fixes

- fix handling of env argument
- handle "id" value for `locate` argument
- mean indicator scales with overall plot scale, not dot point size
- fix handling of `local` argument (for exportHTML())

---

# iNZightPlots 2.11.6

- fix bug resulting in an invalid p-value being displayed for some two sided tests

# iNZightPlots 2.11.5

- provide design effects for survey summaries
- fix bug where regions cannot be selected in interactive sparkline plots due to id issue
- allow `main` to include templates for `%subet%` and `%sizeby%`
- fix y-axis to correctly display percentages (as percentages)
- rename `const_palette_names()` to `cont_palette_names()` (originally misspelled)
- fix bug in checking design call is not `as.svrepdeign`

# iNZightPlots 2.11.4

- fix bug that causes incorrect interactive map labels due to multipolygon regions

# iNZightPlots 2.11.3

- fix bug in exporting of a single numeric variable

# iNZightPlots 2.11.2

- fix a bug in the arguments of the getInfo() method causing exportHTML() to fail

# iNZightPlots 2.11.1

- fix formula interface to handle `colby`, etc.
- fix FT plots for numeric/factor so either order displays the same plot (to match base iNZightPlots graphs)
- Move Financial Times graph package dependencies to Suggests (from Imports)
- Specify `stringsAsFactors = TRUE` for upcoming R 4.0.0

# iNZightPlots 2.11

- Add formula interface function, `iNZPlot`, for `iNZightPlot`, along with similar wrappers `iNZSummary` and `iNZInference` for summary and inferential information, respectively.
- Get Summary for trends no longer includes superfluous "+" in front of negative coefficients
- Make subsetting/faceting for ggplots more in line with base iNZightPlots
- Fix bug in summary output where "Population" was being displayed for non-survey data
- Fix bug in axis labelling of large values where scientific notation used inconsistently

# iNZightPlots 2.10.4

**Release date**: 11 November 2019

- Add `use.plotly` attribute to ggplots
- New method of plotting barcode plots using `geom_spoke`
- Add cutpoint option for diverging stacked bar charts
- Sortable plots now can be sorted in either ascending or descending
- Gridplots now ignore background colours
  Fix bug where only complete cases over all variables were used for ggplots
- Add count of missing values to numeric lollipop/column plots
- Fix issue with brushing points on interactive dotplot when boxplots have been disabled
- Fix up inference information for surveys (+ tests)
- Fix bug where colour by ranks was returning an error
- Fix bug where x-axis transformation was wrong for dotplot by factor when x was the factor and y the numeric
- Enable transparency options for ridgeline plots
- Add colour options for barcode and dot strip plots
- Change default title of cumulative curve plots to "Cumulative Curve ..." (was "Cumulative Count ...")

# iNZightPlots 2.10.3

**Release date**: 23 September 2019

- Rotate spine plot by default
- Change default colouring behaviour for categorical lollipop plots
- Add interactivity for mean indicators on dotplots/histograms
- Add ability to rotate x/y axis labels for ggplots

# iNZightPlots 2.10.2

**Release date**: 2 September 2019

- Fix a bug where level labels weren't shown in one-way table inference
- Add hypothesis test for proportions (normal and exact)
- Show simulated p-value for Chi-square test when expected counts < 5 or requested by user
- Fix bug in exported interactive plots when boxplots are disabled
- Add ability to rotate x-axis label text
- Add ability to change the number of observations each square represents for gridplots
- Rotate some numeric plots to have variable of interest on x-axis, e.g. violins (#106, daniel-barnett)
- Add new `quasirandom` plot instead of `beeswarm` to enable control of swarm width/method

# iNZightPlots 2.10.1

**Release date**: 26 August 2019

- Fix greyscale palette for ggplot heatmaps (#121, @daniel-barnett)
- Add rotation functionality for gridplots (#123, @daniel-barnett)
- Add useful summary information for dates and times

# iNZightPlots 2.10

**Release date**: 13 August 2019

- Fixes small bug where CIs for two-sample equal var didn't match the p-value
- Add option to change ggplot theme of Financial Times plots
- Add ability to include captions for sources in Financial Times plots
- Add beeswarm option for plotting (using ggbeeswarm)

# iNZightPlots 2.9

**Release date**: 15 July 2019

- Add a suite of new plot types from Financial Times (implemented using `ggplot`) (#90, @daniel-barnett)
- Improvements for survey plots and summaries
- Handle frequency variable

# iNZightPlots 2.8

**Release date:** 30 April 2019

- Fix a small bug where new device was created for dotplots even when `plot = FALSE`
- Begin adding unit tests
- Allow plotting of bar plots with counts (instead of %) (removes inference)
- Allow transformations using the `transform` argument (a named list)

# iNZightPlots 2.7.13

**Release date**: 05 April 019

- Fix bug in `getPlotSummary` when passing variables names

# iNZightPlots 2.7

**Release date**: 23 March 2017

## Major Changes

- Introduction of the exportation of SVG and interactive HTML documents

## Bug Fixes

- Various

## Patches

### Patch 2.7.1 - 02/06/2017

- Mostly adjustments and improvements to the SVG and HTML export functionality
- Changes to dependency structure - made SVG/HTML functions suggested instead of imports
- Bumped R version up to 3.0
- Default line type for all trends is now 1; user can choose alternatives

### Patch 2.7.2 - 18/08/2017

- Changes to ensure CRAN checks pass
- More bug fixes in `exportX()` functions
- Single subset interactive plots and scatterplot maps now available

### Patch 2.7.3 - 02/10/2017

- Just a few more improvements for interactive plots.

### Patch 2.7.4 - 08/12/2017

- More interactive plot/SVG fixes

### Patch 2.7.5 - 23/01/2018

- More fixes and features for interactive plots

### Patch 2.7.6 - 21/02/2018

- Again, more fixes/features for interactive plots - Yu Han is working hard!

### Patch 2.7.7 - 18/04/2018

- More fixes and improvements to interactive plots

### Patch 2.7.8 - 23/05/2018

- Fix bug showing up in changes to R 3.5.0
- Latest interact updates
- Reword the inference output for two-way Chi-square test: proportions -> distributions

### Patch 2.7.9 - 14/08/2018

- Restructure templates dir into inst dir to make html/css/js templates accessible via `system.file()`
- Carious other bug fixes and changes to interactive plots

### Patch 2.7.10 - 17/08/2018

- Fix a bug in `exportHTML` where arguments not passed correctly

### Patch 2.7.11 - 15/11/2018

- Add interactive plot helper function
- Add dataset summary function
- Fix a bug in exporting thematic maps (and other fixes for interactive plots)

### Patch 2.7.12 - 26/11/2018

- Fix bug in exporting interactive maps after implementing Stamen Maps

---

# Version 2.6

**Release date**: 12 December 2016

## Major Changes

- Implementation of statistical hypothesis tests

## Minor Changes

- Adjust grid lines to be less distracting
- Allow legend to be hidden (`hide.legend`, logical)

## Bug Fixes

- Fix critical bug in inference output where CI's and p-values for reordered factors weren't reordered!
- Fix issue where confidence intervals for two-way table row proportions were ordered incorrectly

---

# Version 2.5

**Release date**: 5 September 2016

## Major Changes

- Made it possible to create new methods for plots (as done in
  iNZightMaps).
- Allow any colour function to be specified (at least, any
  that take a single argument `n`)
- Allow colour by in hex plots

## Minor Changes

- Changes to resizing algorithm (optional change between the
  two)
- Background colour now applies to INSIDE the plot, not the
  entire graphics window
- Allow setting symbol by a variable
- Hex is now the default plot for large data sets
- Size of "large" data sets increased to 5001
- Allow control of line type
- Change the g1/g2 group labels to light/dark grey respectively. Can use `col.sub = c("g1col", "g2col")` to adjust them.

## Bug Fixes

- Subsetting bug fix in dot plots and histograms
- Many other minor bugs fixed while implementing changes

## Patches

### 2.5.1 - 23/09/2016

- Fix a bug causing two-way bar plots to fail in presence of empty levels

---

# Changes in Version 2.3

**Release date**: 1 October 2015

## Minor Changes

- New argument added for dotplots to allow group labels to be
  placed inside the plot, rather than in the axis margin: `internal.labels=TRUE`

### Patch 2.3.1 - 06/10/2015

- Fix how viewports are named

### Patch 2.3.2 - 08/10/2015

- Fix a bug that prevented comparison lines to be drawn on
  categorical dotplots

### Patch 2.3.3 - 13/10/2015

- Fix a bug where adding comparison intervals would break the
  plot if any of the subgroups in a dotplot were too small. New
  behaviour ignores small groups and only compares large ones.

### Patch 2.3.4 - 13/10/2015

- Continuing from 2.3.3, but now use independent covariances

### Patch 2.3.4 - 13/10/2015

- Fix for Lite: dotplots by factor labels now also on histograms

### Patch 2.3.5 - 28/10/2015

- Fix the "number of missing observations" shown in summary output

### Patch 2.3.7 - 02/11/2015

- No longer redraws dotplots; instead, passes a logical
  attribute for whether the scaling has changed (and
  therefore that the plot should be redrawn).

### Patch 2.3.8 - 16/11/2015

- Fix bug in `colby` if the variable has only one unique value

---

# Changes in Version 2.2

**Release date**: 14 September 2015

## New Features - Survey Design

- Confidence intervals for histograms and bar plots
- Comparison intervals for histograms broken down by a factor
- Summary information for all basic plots (histograms, bar plots, and scatter plots)

## Bug Fixes

- Fixes a bug where missing information on barplots and scatter plots would cause the plotting function to die
- Fixes a bug in the printing of summary objects
- Fixes a bug where the minimum value of a single numeric variable summary was omitted
- And various other small Bug Fixes

---

# Changes in Version 2.1

**Release date**: 04 August 2015

## New Features

- Allow zooming of plots with the new `zoom`
  argument. Works for both univariate and bivariate plots, and a
  related functionality for 'zooming in' on bars in a barplot.

### Patch 2.1-1: 28 August 2015

- Fix a bug that occurs when all survey weights are equal

---

# Changes in Version 2.0.6

**Release date**: 03 August 2015

## Bug Fixes

- Fix an issue where requesting summary of 'dotplots' resulted
  in creating a new device, which resulted in errors on the Shiny
  server.

---

# Changes in Version 2.0.5

**Release date**: 27 July 2015

## New Features

- Additional arguments `xlim` and `ylim` allow users to
  specify the range of values shown on the plot

## Bug Fixes

- Several issues for dotplots have been fixed

- Weighting variable used when drawing a scatter plot of
  survey data

- `conf` now corresponds to Year 12 intervals in dot plot
  inferences (previously, `comp` corresponded to this interval)

---

# Changes in Version 2.0.3

**Release date**: 01 July 2015

## Bug Fixes

- Fix up the order of bars in segmented bar plots to
  correspond to the legend

---

# Changes in Version 2.0.2

**Release date**: 24 June 2015

## Minor Changes

- Remove facility where the colour-by variable is ignored if
  there are 'too many' levels---this is now left up to users to
  decide if colour by a particular variable makes sense of not.

---

# Changes in Version 2.0.1

**Release date**: 16 June 2015

## Minor Changes

- Dotplot locating implemented using new methodology, with the
  additional argument `label.extreme = numeric(2)`, allowing users
  to specify how many lower and upper points to identify,
  respectively.

- Equivalently, extreme points (by using Mahalanobis'
  distance) can be labelled on scatter plots using `label.extreme = numeric(1)`.

## Bug Fixes

- Fixed a small bug that stopped inference from working in
  dotplots when `x` is a factor and `y` is the numeric variable.

- Fix a bug that caused `nbins = 0` in some cases.

---

# Changes in Version 2.0

**Release date**: 26 May 2015

## Major Changes

- The entire package has been rewritten to accommodate complex
  survey designs. At present, survey objects are not fully
  supported, however the functionality will be added over time.

- A huge reduction in computation requirements for plots to
  increase efficiency.

- Algorithms used to compute inference intervals have been
  modified to use iNZightMR for comparisons.

- Lots of other changes to layout and presentation

- Added additional arguments `locate`, `locate.id`,
  `locate.col` (and others) for locating points by IDs.
  This is used in the improved locator functionality in the main
  `iNZight` program.

## Minor Changes

- The `col.by` and `size.by` arguments have been
  replaced by `colby` and `sizeby`

- Documentation has been added for several of the functions (finally!)

---

# Changes in Version 1.0.3

## Bug Fixes

- Specifying `g2.level` with numbers wasn't working, has been fixed for plots,
  summary and inference information.

- Added more space to the y-axis on scatter plots

---

# Changes in Version 1.0.2

## Bug Fixes

- An error where the response was printed instead of the
  x-variable name in summary output for quadratic curves has been
  fixed.

---

# Changes in Version 1.0.1

## Minor Changes

- The type of plot used can be specified by setting the
  `largesample` argument. When set to `TRUE`, it uses the histogram
  or grid-density plot; when `FALSE` it uses the dotplot or scatter
  plot. If set to `NULL`, it uses the sample size to determine which
  plot to draw (default).

- To allow identification features and any additional features
  to be added to plots afterwards, the last viewport is the one
  surrounding the main plot (excluding the plot labels and
  legend). Note that this only works if the data haven't been broken
  down by `g1`.

- Display which variables cannot be plot due to too many
  levels, as well as the number of levels, when attempting to draw
  bar plot. (max levels = 101).

- Trend lines and smoothers added to the legend.

- Alternative method of shading grid-tiles on the grid-density
  plot using quantiles rather than absolute counts. This prevents
  large counts having too large of an influence.

## Bug Fixes

- A bug where the grid-density plot is not using the correct scale has been fixed.

---

# Version 1.0

- New major release of iNZightPlots released, completely rewritten using `grid`
