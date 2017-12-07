#' ExportHTML
#'
#' \code{exportHTML} is designed to export the iNZight plot as a dynamic, interactive HTML page.
#' Currently only handles single panel plots. Coloured hex plots are currently not available yet.
#'
#' @param x An iNZight plot object or function (such as updatePlot) that captures iNZight environment
#' @param file Name of temporary HTML file generated
#' Additional parameters for scatterplots only:
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

  #create pdf graphics device into here:
  pdf(NULL, width = width, height = height, onefile = TRUE)

  #do exporting:
  obj <- x()
  url <- exportHTML(obj, file, data, extra.vars)

  #turn off device:
  dev.off()

  #reset back to original directory:
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
     !requireNamespace("xtable",   quietly = TRUE) ||
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

  # Get data (refer to getTable function):
  if (is.null(extra.vars) && is.null(data)) {
    tbl <- getTable(plot, x)
  } else {
    tbl <- getTable(plot, x, data, extra.vars)
  }

  # write HTML table using xtable:
  if (is.null(tbl)) {
    HTMLtable <- '<p> No table available. </p>'
  } else {
    HTMLtable <- print(xtable::xtable(tbl$tab, caption = tbl$cap, auto = TRUE),
                       type = "html", html.table.attributes = 'class="table table-striped table-bordered table-condensed hidden" id="table"',
                       caption.placement = "top", align = "center",
                       include.rownames = tbl$includeRow, print.results = FALSE)
  }

  # Get JS file  + JSON
  jsData <- convertToJS(plot, tbl)

  #bind JSON to grid plot, generate SVG
  gridSVG::grid.script(do.call("paste", c(jsData[-length(jsData)], sep="\n ")), inline = TRUE, name = "linkedJSONdata")
  svgOutput <- gridSVG::grid.export(NULL)$svg

  # A possibly more permanent solution using the XML package to remove width and height:
  # only implement this if we end up using the XML package more often
  # svgOutput <- XML::removeAttributes(doc, .attrs = c("width", "height"))

  #get JS code associated with plot type
  jsCode <-  jsData$jsFile

  svgCode <- paste(capture.output(svgOutput), collapse = "\n")

  #remove svg file and other scripts
  grid.remove("linkedJSONdata")

  #finding places where to substitute code:
  svgLine <- grep("SVG", HTMLtemplate)
  cssLine <- grep("styles.css", HTMLtemplate)
  functionLine <- grep("commonFunctions", HTMLtemplate)
  jsLine <- grep("JSfile", HTMLtemplate)
  tableLineOne <- grep("table", HTMLtemplate)

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


## Generating tables for each plot:

getTable <- function(plot, x = NULL, data = NULL, extra.vars = NULL)  {
  UseMethod("getTable")
}

getTable.inzbar <- function(plot, x) {

  # generation of table of counts:
  # plot <- x$all$all

  prop <- plot$phat
  counts <- plot$tab
  percent <- plot$widths
  n <- attributes(x)$total.obs
  if (attributes(x)$total.missing != 0) {
    n <- attributes(x)$total.obs - attributes(x)$total.missing
  }

  # color matching for colby:
  colorMatch <- plot$p.colby

  prop.df <- as.data.frame(t(prop))
  counts.df <- as.data.frame(counts)

  if (all(percent != 1)) {
    # This condition is used to identify if it's a two way plot...
    # different table for two way bar plots
    tab  = cbind(round(prop,4), format(round(rowSums(prop),4), nsmall = 4), rowSums(counts))
    colnames(tab)[(ncol(prop)+1):ncol(tab)] <- c("Total", "Row N")
  } else if (!is.null(colorMatch) && (all(c(0, 1) %in% colorMatch) == FALSE)) {
    # for stacked bar plots
    # creation of a special two-way table for stacked bars
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

  # attributes for HTML table
  cap <- 'Table of Counts and Proportions'
  includeRow <- TRUE
  tableInfo <- list(caption = cap, includeRow = includeRow, tab = tab, n = n)

  return(tableInfo)
}

getTable.inzhist <- function(plot, x) {

  #plot <- x$all$all or plot <- x$all[[1]]
  toPlot <- plot$toplot$all
  intervals <- toPlot$breaks
  counts <- toPlot$counts
  n <- attributes(x)$total.obs

  if (attributes(x)$total.missing != 0) {
    n <- attributes(x)$total.obs - attributes(x)$total.missing
  }

  # generation of freq distribution table:
  lower <- round(intervals[-length(intervals)], 2)
  upper <- round(intervals[-1], 2)
  interval <- paste(lower, upper, sep ="-")
  tab <- cbind(interval, counts)
  colnames(tab) <- c("Class Interval", "Frequency")
  tab <- rbind(tab, c("Total", sum(counts)))

  ## Attributes for HTML table
  cap <- "Frequency Distribution Table"
  includeRow <- FALSE
  tableInfo <- list(caption = cap, includeRow = includeRow, tab = tab, n = n)

  return(tableInfo)
}

getTable.inzdot <- function(plot, x, data = NULL, extra.vars = NULL) {

  plots <- plot$toplot
  levels <- names(plots)
  varNames <-attributes(x)$varnames

  if (length(levels) > 1)  { # for multi-level dot plots

    levList = list();
    data = list();

    for (i in 1:length(levels)) {
      #currently only takes variable plotted
      levList[[i]] = plots[[i]]$x
      data[[i]] = cbind(levList[[i]], levels[i])
    }

    #bind all groups together:
    tab <- do.call("rbind", data)
    colnames(tab) <- c(attributes(x)$varnames$x, attributes(x)$varnames$y)

  } else { #for single dot plots

    plot <- plot$toplot$all
    order <- attr(plot, "order")

    #DEFAULT: only shows variables plotted.
    xVal <- plot$x
    tab <- as.data.frame(xVal)
    names(tab) <- varNames$x

    #variable selection
    tab <- varSelect(x, extra.vars, plot, data, tab, xVal,
                     NULL, order, varNames)
  }

  ##Attributes for HTML table
  cap <- "Data"
  includeRow <- FALSE
  tableInfo <- list(caption = cap, includeRow = includeRow,
                    tab = tab, varNames = varNames)
  return(tableInfo)
}

getTable.inzscatter <- function(plot, x, data = NULL, extra.vars = NULL) {

  #For scatterplots, the user can choose to either export the whole dataset they've specified, or certain variables.
  #By default: only exports the two variables that are plotted (if no dataset is specified or extra.vars = NULL).

  xVal <- plot$x
  yVal <- plot$y
  order <- plot$point.order
  varNames <- attributes(x)$varnames

  tab <- cbind(as.data.frame(xVal), as.data.frame(yVal))
  names(tab) <- c(varNames$x, varNames$y)
  #test for variable selection:
  tab <- varSelect(x, extra.vars, plot, data, tab,
                   xVal, yVal, order, varNames)

  ## Attributes for HTML table
  cap <- "Data"
  includeRow <- FALSE
  tableInfo <- list(caption = cap, includeRow = includeRow, tab = tab)
  return(tableInfo)
}

#No table for hexplots yet!
getTable.inzhex <- function(plot, x = NULL) {
  warning("No table available for hexbin plots.")
  tableInfo <- NULL;
  return(tableInfo);
}

getTable.default <- function(plot, x) {
  warning("Cannot generate table! There may not be an interactive version of this plot yet...")
  return()
}

# Variable selection: for dot plots and scatter plots
# Used when user wishes to export additional variables in the table/ in tooltips.
varSelect <- function(x, extra.vars, pl, data, tab, xVal, yVal, order, varNames) {

  if (!is.null(extra.vars) && !is.null(data)) {
    #obtain column index
    colNum <- as.numeric(sapply(extra.vars, function(extra.vars) grep(extra.vars, colnames(data))))
    #obtain extra data columns
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
    stop("Error: no dataset specified to export extra variables! Please specify a dataset.", .call = FALSE)
  } else {
    tab <- tab
    #if there's a colby variable
    if (!is.null(pl$colby)) {
      colby <- pl$colby
      tab <- cbind(tab, as.data.frame(colby))
      names(tab)[ncol(tab)] <- varNames$colby
    }

    #if there's a sizeby variable
    if (!is.null(varNames$sizeby)) {
      sizeby <- pl$propsize
      tab <- cbind(tab, as.data.frame(sizeby))
      names(tab)[ncol(tab)] <- varNames$sizeby
    }
  }

  # for maps only:
  if(x$gen$opts$plottype == "map") {
    names(tab) <- gsub("expression[(]\\.([[:alpha:]]*)[)]", "\\1", names(tab))
  }
  return(tab)
}

## convert data to JSON
convertToJS <- function(plot, tbl= NULL) UseMethod("convertToJS")

convertToJS.inzbar <- function(plot, tbl) {

  #plot <- x$all$all
  prop <- plot$phat
  counts <- plot$tab
  percent <- plot$widths
  colorMatch <- plot$p.colby

  pct <- as.data.frame(t(round(prop*100, 2)))
  counts.df <- as.data.frame(counts)
  group <- paste0('var group = ', length(percent), ';')

  tab <- cbind(counts, pct)
  colnames(tab) <- c('varx', 'counts', 'pct')
  colCounts = 'var colCounts = null;'

  if (all(percent != 1)) { ## two way bar plots
    prop.df =  as.data.frame(prop)
    tab <- cbind(prop.df, counts.df$Freq)
    colnames(tab) <- c("var1", "var2", "pct", "counts")
    tab$pct <- round(tab$pct*100, 2)
    colCounts = paste0("var colCounts = ", jsonlite::toJSON(c("Col N", round(colSums(counts)/tbl$n,4), 1)), ";")
  }

  #set order:
  orderJSON = 'var order = null;'

  #selecting appropriate JS code:
  jsFile <- bpJS

  #Differentiating between one way bar plots - stacked bars, colored, and non-colored:
  if (is.null(colorMatch)) {
    ## test if bar plots have color
    colorMatchJSON = paste("var colorMatch = null;", sep = "");
  } else if ((all(c(0, 1) %in% colorMatch) == TRUE)){
    ## test if the bar plot colby is the same
    colorMatchJSON = paste("var colorMatch = '", gsub(",", "", jsonlite::toJSON(as.vector(colorMatch))), "';", sep ="")
  } else {
    ## required for stacked bar plots, due to make up of polygons being reversed when plotted
    order <- matrix(1:length(colorMatch), ncol=ncol(colorMatch), byrow = TRUE)
    order <- as.vector(apply(order,1 ,rev))
    orderJSON <- paste0("var order = ", jsonlite::toJSON(order), ";" )
    colorMatch <- as.table(colorMatch)
    colorMatchJSON = paste("var colorMatch = true;")

    tab <- as.data.frame(colorMatch, stringsAsFactors = FALSE)
    tab$pct <- round(tab$Freq*100, 2)

    #counts are in the counts value -> need to merge on var2
    colnames(counts.df) <- c("Var2", "c1")
    tab <- merge(tab, counts.df)
    tab <- tab[order(tab$Var1),]

    #now calculate counts:
    tab$counts <- with(tab, c1*Freq)
    tab <- cbind(tab, order)
    tab <- tab[order(tab$order), ]

    ## setting JS: stacked bar plots currently run on a different JS file
    jsFile <- bpstackedJS
  }

  tabJSON <- paste0("var tab = ", jsonlite::toJSON(tab), ";");
  #returning all data in a list:
  JSData <- list(tab = tabJSON, colorMatch = colorMatchJSON, colCounts = colCounts, group = group, order = orderJSON, jsFile = jsFile)
  return(JSData)
}

convertToJS.inzhist <- function(plot, tbl) {

  ## plot <- x$all$all or plot <- x$all[[1]]
  toPlot <- plot$toplot$all

  intervals <- toPlot$breaks
  lower <- round(intervals[-length(intervals)], 2)
  upper <- round(intervals[-1], 2)
  counts <- toPlot$counts
  pct <- round(counts/tbl$n*100, 2)

  tab <- as.data.frame(cbind(lower, upper, counts, pct))

  #To obtain box whisker plot information:
  boxInfo <- plot$boxinfo$all
  quantiles <-boxInfo$quantiles
  min <- boxInfo$min
  max <- boxInfo$max
  boxTable <- rbind(as.data.frame(quantiles), min, max)
  rownames(boxTable)[4:5] <- c("min", "max")

  ## Conversion to JSON data:
  tabJSON <- paste0("var tab = ", jsonlite::toJSON(tab), ";")
  boxJSON <- paste0("var boxData = ", jsonlite::toJSON(boxTable), ";")
  n <- paste0("var n = ", tbl$n)

  ## JS code:
  jsFile <- histJS

  ##Output as a list:
  JSData <- list(data = tabJSON, box = boxJSON, n = n, jsFile = jsFile)
  return(JSData)
}

convertToJS.inzdot <- function(plot, tbl) {

  levels = names(plot$toplot)

  if (length(levels) > 1) {
    plots <- plot$toplot
    #create lists:
    levList = list();
    boxList = list();
    countsTab = as.numeric();
    countsTab[1] = 0;

    for (i in 1:length(levels)) {
      # currently only takes variable plotted
      levList[[i]] = plots[[i]]$x
      #obtain boxplot information
      box = plot$boxinfo
      quantiles = box[[i]]$quantiles
      min = box[[i]]$min
      max = box[[i]]$max
      #get cumulative frequency of counts for each group:
      countsTab[i+1] = sum(plots[[i]]$counts, countsTab[i])
      #get boxplot summaries for each group
      boxTable <- rbind(as.data.frame(quantiles), min, max)
      rownames(boxTable)[4:5] <- c("min", "max")
      boxList[[i]] = boxTable
    }

    #convert to JSON:
    boxListJSON <- paste0("var boxData = ", jsonlite::toJSON(boxList), ";")
    levListJSON <- paste0("var levels = ", jsonlite::toJSON(levList), ";")
    countsTab <- paste0("var countsTab = ", jsonlite::toJSON(countsTab), ";")
    names <- paste0("var names = ", jsonlite::toJSON(tbl$varNames), ";")
    levNames <- paste0("var levNames = ", jsonlite::toJSON(names(plots)), ";")

    #JS:
    jsFile <- multidotJS
    JSData <- list(boxList = boxListJSON, levList = levListJSON, countsTab = countsTab, names = names, levNames = levNames, jsFile = jsFile)

  } else {
    #Extracting information:
    colGroupNo <- nlevels(plot$toplot$all$colby)

    namesJSON <- paste0("var names = ", jsonlite::toJSON(names(tbl$tab)), ";")
    tabJSON <- paste0("var tableData = ", jsonlite::toJSON(tbl$tab), ";")
    colGroupNo <- paste0("colGroupNo = ", colGroupNo, ";")

    #To obtain box whisker plot information:
    boxInfo <- plot$boxinfo$all
    quantiles <-boxInfo$quantiles
    min <- boxInfo$min
    max <- boxInfo$max
    boxTable <- rbind(as.data.frame(quantiles), min, max)
    rownames(boxTable)[4:5] <- c("min", "max")

    ## JS:
    boxJSON <- paste0("var boxData = ", jsonlite::toJSON(boxTable), ";");
    jsFile <- dpspJS
    JSData <- list(names = namesJSON, table = tabJSON, colGroupNo = colGroupNo, box = boxJSON, jsFile = jsFile)
  }
  return(JSData)
}

convertToJS.inzscatter <- function(plot, tbl) {

  colGroupNo <- nlevels(plot$colby)
  namesJSON <- paste0("var names = ", jsonlite::toJSON(names(tbl$tab)), ";")
  tabJSON <- paste0("var tableData = ", jsonlite::toJSON(tbl$tab), ";")
  colGroupNo <- paste0("colGroupNo = ", colGroupNo, ";")
  # JS
  jsFile <- dpspJS
  JSData <- list(names = namesJSON, tabs = tabJSON, colGroupNo = colGroupNo, jsFile = jsFile)
  return(JSData)
}

#Trialling hex plots:
convertToJS.inzhex <- function(plot, tbl = NULL) {

  #Hexplots use S4 notation:
  counts <- plot$hex@count
  xcm <- round(plot$hex@xcm, 2)
  ycm <- round(plot$hex@ycm, 2)
  n <- plot$hex@n

  tab <- as.data.frame(cbind(counts, xcm, ycm))
  tab$pct <- round(tab$counts/n*100, 2)
  n <- paste0("var n =", n)
  tabJSON <- paste0("var tab =", jsonlite::toJSON(tab), ';')

  # JS
  jsFile <- hexbinJS
  JSData <- list(data = tabJSON, n = n, jsFile = jsFile)
  return(JSData)

}

convertToJS.default <- function(plot, tbl = NULL) {
  warning("No JS available. :(")
  return()
}
