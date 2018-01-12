#' ExportHTML
#'
#' \code{exportHTML} is designed to export the iNZight plot as a dynamic, interactive HTML page.
#' Currently only handles single panel plots. Coloured hex plots are currently not available yet.
#'
#' @param x An iNZight plot object or function (such as updatePlot) that captures iNZight environment
#' @param file Name of temporary HTML file generated
#' Additional parameters for scatterplots and dotplots only:
#' @param data dataset/dataframe that you wish to investigate and export more variables from
#' @param extra.vars extra variables specified by the user to be exported
#' @param width the desired width of the SVG plot
#' @param height the desired height of the SVG plot
#' @param ... extra arguments
#'
#' @return Opens up an HTML file of \code{x} with filename \code{file} in the browser (best performance on Chrome/Firefox)
#'
#' @examples
#' \dontrun{
#' x <- iNZightPlot(Petal.Width, Petal.Length, data = iris, colby = Species)
#' exportHTML(x, "index.html")
#'
#' #to export more variables for scatterplots:
#'  exportHTML(x, "index.html", data = iris, extra.vars = c("Sepal.Length", "Sepal.Width"))
#' }
#'
#' @author Yu Han Soh
#' @export
exportHTML <- function(x, file, data, extra.vars, ...) UseMethod("exportHTML")

#' @describeIn exportHTML method for an iNZightPlot-generating function
#' @export
exportHTML.function <- function(x, file = 'index.html', data = NULL, extra.vars = NULL,
                                width = dev.size()[1], height = dev.size()[2], ...) {

  #get current directory
  curdir <- getwd()
  on.exit(setwd(curdir))

  #set to temp directory
  tdir <- tempdir()
  setwd(tdir)
  pdf(NULL, width = width, height = height, onefile = TRUE)

  #do exporting:
  obj <- x()
  url <- exportHTML(obj, file, data, extra.vars)

  dev.off()
  setwd(curdir)

  ## pass URL from exportHTML.inzplotoutput
  invisible(url)
}

#this is generalized for every plot - involves binding everything together.
#' @describeIn exportHTML method for output from iNZightPlot
#' @export
exportHTML.inzplotoutput <- function(x, file = 'index.html', data = NULL, extra.vars = NULL, ...) {

  #suggest gridSVG, jsonlite, xtable:
  if(!requireNamespace("gridSVG",  quietly = TRUE) ||
     !requireNamespace("knitr",   quietly = TRUE) ||
     !requireNamespace("jsonlite", quietly = TRUE) ) {
    stop(paste("Required packages aren't installed",
               "Use 'install.packages('iNZightPlots', depends = TRUE)' to install them.",
               sep = "\n"))
  }

  curdir <- getwd()
  x <- x
  plot <- x$all$all

  if (is.null(plot)) {
    if (length(x$all) == 1) { ## for subsets = 1
      plot <- x$all[[1]]
      } else { ## subsets > 1
    warning("iNZight doesn't handle interactive panel plots yet!")
    return()
      }
  }

  #condition for colored hexplots - currently unavailable:
  if(attributes(x)$plottype == "hex" && !is.null(plot$colby)) {
    warning('iNZight cannot handle interactive colored hex plots yet!')
    return()
  }

  # if it passes the above: work in temp. directory
  setwd(tempdir())

  # Get data (refer to getInfo function):
  if (is.null(extra.vars) && is.null(data)) {
    info <- getInfo(plot, x)
  } else {
    info <- getInfo(plot, x, data, extra.vars)
  }

  tbl <- info$tbl
  js <- info$js

  # generate HTML table
  if (is.null(tbl)) {
    HTMLtable <- '<p> No table available. </p>'
  } else {
    # switched from xtable to knitr: table in desired format for dataTables to work
    HTMLtable <- knitr::kable(tbl$tab, format = "html", row.names = tbl$includeRow,
                              align = "c", caption = tbl$cap, table.attr = "id=\"table\" class=\"table table-condensed table-hover\" cellspacing=\"0\"")
  }

  # bind JSON to grid plot, generate SVG
  gridSVG::grid.script(paste0("var chart = ", js$chart,";"), inline = TRUE, name = "linkedJSONdata")
  svgOutput <- gridSVG::grid.export(NULL)$svg

  # A possibly more permanent solution using the XML package to remove width and height:
  # only implement this if we end up using the XML package more often
  # svgOutput <- XML::removeAttributes(doc, .attrs = c("width", "height"))

  #get JS code associated with plot type
  jsCode <-  js$jsFile
  svgCode <- paste(capture.output(svgOutput), collapse = "\n")

  #remove svg file and other scripts
  grid.remove("linkedJSONdata")

  #finding places where to substitute code:
  svgLine <- grep("SVG", HTMLtemplate)
  cssLine <- grep("styles.css", HTMLtemplate)
  functionLine <- grep("commonFunctions", HTMLtemplate)
  jsLine <- grep("JSfile", HTMLtemplate)
  tableLineOne <- grep("<!-- insert table -->", HTMLtemplate)

  # insert inline JS, CSS, table, SVG:
  HTMLtemplate[cssLine] <- paste(styles, collapse = "\n")
  HTMLtemplate[jsLine] <- paste(jsCode, collapse = "\n")

  # for now: the singleFunctions file is read through (for multi-panel plots, this will change.)
  if(length(plot$toplot) > 1) {
    HTMLtemplate[functionLine] <- "null"
  } else {
    HTMLtemplate[functionLine] <- paste(singleFunctions, collapse = "\n")
  }

  HTMLtemplate[tableLineOne] <- HTMLtable
  HTMLtemplate[svgLine] <- svgCode

  # Writing it out to an HTML file:
  write(HTMLtemplate, file)

  # Store url:
  # url <- file.path(getwd(), file)
  ## 'file.html' -> '/tmp/path/to/file.html'
  ## '/absolute/path/to/file.html' -> '/absolute/path/to/file.html'
  url <- normalizePath(file)
  class(url) <- "inzHTML"

  #reset back to original directory:
  setwd(curdir)

  #return url:
  invisible(url)
}


##' Print method for `inzHTML` object
##'
##' The default action is for the URL to be 'printed' (opened) in the browser,
##' unless `viewer` is specified as something else.
##' If `viewer = NULL`, then the URL is printed as a character string.
##' @param x      a URL that will be printed
##' @param viewer the viewing function to use to display the URL
##' @param ... additional arguments
##' @return NULL (it's a print function, after all)
##' @export
print.inzHTML <- function(x, viewer = getOption("viewer", utils::browseURL), ...) {

  if (!is.null(viewer))
    viewer(x)
  else
    print(as.character(x))

  invisible(NULL)
}


## getInfo method for retrieving table and JSON
## @param plot (for single panels)
## @param x the entire plot object
## @param data the data set
## @param extra.vars extra variables to be exported
## @return a list consisting of the table (tbl) and JSON (js)
## internal function
getInfo <- function(plot, x = NULL, data = NULL, extra.vars = NULL)  {
  UseMethod("getInfo")
}

getInfo.inzbar <- function(plot, x) {
  # generation of table of counts:
  # plot <- x$all$all
  prop <- plot$phat
  counts <- plot$tab
  percent <- plot$widths
  n <- attributes(x)$total.obs

  if (attributes(x)$total.missing != 0) {
    n <- attributes(x)$total.obs - attributes(x)$total.missing
  }

  # color matching:
  colorMatch <- plot$p.colby
  prop.df <- as.data.frame(t(prop))
  counts.df <- as.data.frame(counts)
  group <- length(percent)
  pct <- as.data.frame(t(round(prop*100, 2)))

  dt <- cbind(counts, pct)
  colnames(dt) <- c('varx', 'counts', 'pct')
  colCounts = NULL;
  order = NULL;

  # selecting appropriate JS code:
  jsFile <- bpJS

  if (all(percent != 1)) {
    # This condition is used to identify if it's a two way plot...
    # different table for two way bar plots
    tab  = cbind(round(prop,4), format(round(rowSums(prop),4), nsmall = 4), rowSums(counts))
    colnames(tab)[(ncol(prop)+1):ncol(tab)] <- c("Total", "Row N")

    ## for JSON:
    prop.df =  as.data.frame(prop)
    dt <- cbind(prop.df, counts.df$Freq)
    colnames(dt) <- c("var1", "var2", "pct", "counts")
    dt$pct <- round(dt$pct*100, 2)
    colCounts = c("Col N", round(colSums(counts)/n,4), 1)

  } else if (!is.null(colorMatch) && (all(c(0, 1) %in% colorMatch) == FALSE)) {
    # for stacked bar plots - a special two-way table
    colorMatchRev <- colorMatch[nrow(colorMatch):1, ]
    proportions <- round(rbind(colorMatchRev, colSums(colorMatchRev)), 4)
    tab <- rbind(proportions,counts)
    rownames(tab)[nrow(proportions):nrow(tab)] <- c("Total", "Col N")

  } else {
    # creating table for 1 way plots
    tab <- rbind(counts, round(prop*100,2))
    tab <- cbind(tab, rowSums(tab))
    tab[2,] <- paste0(tab[2,], "%")
    colnames(tab)[ncol(tab)] <- "Total"
    rownames(tab) <- c("Counts", "Percent")
  }

  ## JSON:
  if (is.null(colorMatch)) {
    colorMatch <- NA
  } else if (all(c(0, 1) %in% colorMatch) == TRUE) {
    ## test if the bar plot colby is the same
    colorMatch <- as.vector(colorMatch)
  } else {
    ## required for stacked bar plots, due to make up of polygons being reversed when plotted
    order <- matrix(1:length(colorMatch), ncol = ncol(colorMatch), byrow = TRUE)
    order <- as.vector(apply(order, 1, rev))
    dt <- as.data.frame(as.table(colorMatch), stringsAsFactors = FALSE)
    dt$pct <- round(dt$Freq*100, 2)
    #counts are in the counts value -> need to merge on var2
    colnames(counts.df) <- c("Var2", "c1")
    dt <- merge(dt, counts.df)
    dt <- dt[order(dt$Var1),]
    # calculate counts
    dt$counts <- with(dt, c1*Freq)
    dt <- cbind(dt, order)
    dt <- dt[order(dt$order), ]

    # reset colorMatch
    colorMatch <- TRUE
    jsFile <- bpstackedJS
  }

  # attributes for HTML table
  cap <- 'Table of Counts and Proportions'
  includeRow <- TRUE
  tableInfo <- list(caption = cap, includeRow = includeRow, tab = tab, n = n)
  #returning all data in a list:
  chart <- list(data = dt, colorMatch = colorMatch, colCounts = colCounts, group = group, order = order)
  JSData <- list(chart = jsonlite::toJSON(chart), jsFile = jsFile)
  return(list(tbl = tableInfo, js = JSData))
}

getInfo.inzhist <- function(plot, x) {
  #plot <- x$all$all or plot <- x$all[[1]]
  toPlot <- plot$toplot$all
  intervals <- toPlot$breaks
  counts <- toPlot$counts
  n <- attributes(x)$total.obs

  if (attributes(x)$total.missing != 0) {
    n <- attributes(x)$total.obs - attributes(x)$total.missing
  }

  # freq distribution table:
  lower <- round(intervals[-length(intervals)], 2)
  upper <- round(intervals[-1], 2)
  interval <- paste(lower, upper, sep ="-")
  tab <- cbind(interval, counts)
  colnames(tab) <- c("Class Interval", "Frequency")
  tab <- rbind(tab, c("Total", sum(counts)))

  ## Attributes for HTML table
  cap <- "Frequency Distribution Table"
  includeRow <- TRUE
  tableInfo <- list(caption = cap, includeRow = includeRow, tab = data.frame(tab), n = n)

  # next, store info as JSON
  pct <- round(counts/n*100, 2)
  tab <- as.data.frame(cbind(lower, upper, counts, pct))

  # To obtain box whisker plot information:
  boxInfo <- plot$boxinfo$all
  quantiles <- boxInfo$quantiles
  min <- boxInfo$min
  max <- boxInfo$max
  boxTable <- rbind(as.data.frame(quantiles), min, max)
  rownames(boxTable)[4:5] <- c("min", "max")
  jsFile <- histJS
  # Output as a list:
  chart <- list(type = "hist", data = tab, boxData = boxTable, n = n, inf = NULL)
  JSData <- list(chart = jsonlite::toJSON(chart), jsFile = histJS)
  return(list(tbl = tableInfo, js = JSData))
}

getInfo.inzdot <- function(plot, x, data = NULL, extra.vars = NULL) {
  plots <- plot$toplot
  levels <- names(plots)
  varNames <-attributes(x)$varnames

  if (length(levels) > 1)  { # for multi-level dot plots

    levList = list();
    data = list();
    boxList = list();
    countsTab = as.numeric();
    countsTab[1] = 0;

    for (i in 1:length(levels)) {
      #currently only takes variable plotted
      levList[[i]] = plots[[i]]$x
      data[[i]] = cbind(levList[[i]], levels[i])

      #obtain boxplot information
      box = plot$boxinfo
      quantiles = box[[i]]$quantiles
      min = box[[i]]$min
      max = box[[i]]$max

      # get cumulative frequency of counts for each group:
      countsTab[i+1] = sum(plots[[i]]$counts, countsTab[i])
      # get boxplot summaries for each group
      boxTable <- rbind(as.data.frame(quantiles), min, max)
      rownames(boxTable)[4:5] <- c("min", "max")
      boxList[[i]] <- boxTable
    }

    #bind all groups together:
    tab <- do.call("rbind", data)
    colnames(tab) <- c(attributes(x)$varnames$x, attributes(x)$varnames$y)

    jsFile <- multidotJS
    chart <- list(type = "dot", boxData = boxList, levList = levList,
                  countsTab = countsTab, levNames = names(plots), varNames = colnames(tab))

    JSData <- list(chart = jsonlite::toJSON(chart), jsFile = multidotJS)

  } else { #for single dot plots

    colGroupNo <- nlevels(plot$toplot$all$colby)
    pl <- plot$toplot$all
    order <- attr(pl, "order")

    # DEFAULT: only shows variables plotted.
    xVal <- pl$x
    tab <- as.data.frame(xVal)
    names(tab) <- varNames$x

    #variable selection
    tab <- varSelect(x, extra.vars, pl, data, tab, xVal,
                     NULL, order, varNames)

    #To obtain box whisker plot information:
    boxInfo <- plot$boxinfo$all
    quantiles <- boxInfo$quantiles
    min <- boxInfo$min
    max <- boxInfo$max
    boxTable <- rbind(as.data.frame(quantiles), min, max)
    rownames(boxTable)[4:5] <- c("min", "max")

    ## JS:
    chart <- list(type = "dot", varNames = names(tab), data = tab, colGroupNo = colGroupNo,
                  boxData = boxTable)
    JSData <- list(chart = jsonlite::toJSON(chart), jsFile = dpspJS)
  }

  ##Attributes for HTML table
  cap <- "Data"
  includeRow <- TRUE
  tableInfo <- list(caption = cap, includeRow = includeRow,
                    tab = data.frame(tab), varNames = varNames)
  return(list(tbl = tableInfo, js = JSData))
}

getInfo.inzscatter <- function(plot, x, data = NULL, extra.vars = NULL) {
  obj <- x
  x <- plot$x
  y <- plot$y
  order <- plot$point.order
  varNames <- attributes(obj)$varnames
  colGroupNo <- nlevels(plot$colby)

  tab <- cbind(as.data.frame(x), as.data.frame(y))
  names(tab) <- c(varNames$x, varNames$y)
  #test for variable selection:
  tab <- varSelect(obj, extra.vars, plot, data, tab,
                   x, y, order, varNames)

  ## Attributes for HTML table
  cap <- "Data"
  includeRow <- TRUE
  tbl <- list(caption = cap, includeRow = includeRow, tab = tab)

  # simplistic inference information: beta coefficients + R^2
  # For future purposes - if summary(obj) returns a list with these inside it, then we
  # could use summary(obj)$formula / summary(obj)$Rspearman instead....
  # currently does NOT take into account of survey design
  # only works if trends are plotted...
  if (is.null(plot$trend)) {
    trendInfo <- NA
  } else {
    trendInfo <- list(linear = NA, quadratic = NA, cubic = NA,  rank.cor = NA)

    if ("linear" %in% plot$trend) {
      beta <- signif(coef(lm(y ~ x)), 4)
      c <- round(cor(x, y), 2)
      dim(beta) <- NULL
      trendInfo$linear <- c(beta, c)
    }

    if ("quadratic" %in% plot$trend) {
      beta <- signif(coef(lm(y ~ x + I(x^2))), 4)
      dim(beta) <- NULL
      trendInfo$quadratic <- beta
    }

    if ("cubic" %in% plot$trend) {
      beta <- signif(coef(lm(y ~ x + I(x^2) + I(x^3))), 4)
      dim(beta) <- NULL
      trendInfo$cubic <- beta
    }
    trendInfo$rank.cor <- cor(x, y, method = "spearman")
  }

  # JS
  chart <- list(type = "scatter", varNames = names(tab), data = tab,
                colGroupNo = colGroupNo, trendInfo = trendInfo)
  JSData <- list(chart = jsonlite::toJSON(chart), jsFile = dpspJS)

  return(list(tbl = tbl, js = JSData))
}

#No table for hexplots yet!
getInfo.inzhex <- function(plot, x = NULL) {
  warning("No table available for hexbin plots.")
  tbl <- NULL

  # hexplots use S4 notation
  counts <- plot$hex@count
  xcm <- round(plot$hex@xcm, 2)
  ycm <- round(plot$hex@ycm, 2)
  n <- plot$hex@n

  tab <- as.data.frame(cbind(counts, xcm, ycm))
  tab$pct <- round(tab$counts/n*100, 2)

  # JS
  jsFile <- hexbinJS
  chart <- list(type = "hex", data = tab, n = n)
  JSData <- list(chart = jsonlite::toJSON(chart), jsFile = hexbinJS)

  return(list(tbl = tbl, js = JSData))
}

getInfo.default <- function(plot, x) {
  warning("Cannot generate table! There may not be an interactive version of this plot yet...")
  return()
}

# Variable selection: for dot plots and scatter plots
# Used when user wishes to export additional variables in the table/ in tooltips
varSelect <- function(x, extra.vars, pl, data, tab, xVal, yVal, order, varNames) {
  if (!is.null(extra.vars) && !is.null(data)) {
    # obtain column index
    colNum <- as.numeric(sapply(extra.vars, function(extra.vars) grep(extra.vars, colnames(data))))
    # obtain extra data columns
    extra.cols <- data[order, colNum]

    if (is.null(yVal)) {
      tab <- cbind(extra.cols, as.data.frame(xVal))
      names(tab) <- c(extra.vars, varNames$x)
    } else {
      tab <- cbind(extra.cols, as.data.frame(xVal), as.data.frame(yVal))
      names(tab) <- c(extra.vars, varNames$x, varNames$y)
    }
  } else if (ncol(data) < 10 && !is.null(data)) {
    tab <- data[order, ]
  } else if (is.null(data) && !is.null(extra.vars)) {
    stop("Error: no dataset specified to export extra variables! Please specify a dataset.",
         .call = FALSE)
  } else {
    tab <- tab
    # if there's a colby variable
    if (!is.null(pl$colby)) {
      colby <- pl$colby
      tab <- cbind(tab, as.data.frame(colby))
      names(tab)[ncol(tab)] <- varNames$colby
    }

    # if there's a sizeby variable
    if (!is.null(varNames$sizeby)) {
      sizeby <- pl$propsize
      tab <- cbind(tab, as.data.frame(sizeby))
      names(tab)[ncol(tab)] <- varNames$sizeby
    }
  }

  # for maps only
  if(x$gen$opts$plottype == "map") {
    names(tab) <- gsub("expression[(]\\.([[:alpha:]]*)[)]", "\\1", names(tab))
  }
  return(tab)
}
