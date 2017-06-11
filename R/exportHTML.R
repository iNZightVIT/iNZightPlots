#' @title ExportHTML
#'
#' @description \code{exportHTML} is designed to export the iNZight plot as a dynamic, interactive HTML page.
#'  It opens the written HTML page in a web browser.
#' Currently only handles single panel plots. Coloured hex plots are currently not available yet.
#'
#' @details
#' Generates an appropriate HTML table, converts data objects into JSON and retrieves a JavaScript file based upon the plot. Converts the plot to an svg,
#' and inserts the svg plot, table, and JavaScript into the an HTML template to produce the page that is viewed in the browser.
#' This function acts as generic function (either takes in function or iNZight plot object) and is comprised of two other functions.
#' \code{getTable} aims to construct the appropriate table for the plot using information stored in the iNZight plot object, or data provided.
#' \code{convertToJS} converts appropriate data into JSON and writes the appropriate JS file to give interactivity to the HTML page.
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
#' exportHTML(x, "test.html")
#'
#' #to export more variables for scatterplots:
#'  exportHTML(x, "test.html", data = iris, extra.vars = c("Sepal.Length", "Sepal.Width"))
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
    warning("iNZight doesn't handle interactive panel plots ... yet!")
    return()
  }

  #condition for colored hexplots - currently unavailable:
  if(attributes(x)$plottype == "hex" && !is.null(plot$colby)) {
    warning('iNZight cannot handle interactive colored hex plots yet!')
    return()
  }


  # if it passes the above: work in temp. directory
  setwd(tempdir())

  #Create the table (refer to getTable function):
  if (is.null(extra.vars) && is.null(data)) {
    tbl <- getTable(plot, x)
  } else {
    tbl <- getTable(plot, x, data, extra.vars)
  }

  #write HTML table using xtable:
  if (is.null(tbl)) {
    HTMLtable <- '<p> No table available. </p>'
  } else {

    HTMLtable <- print(xtable::xtable(tbl$tab, caption = tbl$cap, auto = TRUE),
                       type = "html", html.table.attributes= 'class="table table-fit table-striped table-bordered table-responsive hidden" id="table"',
                       caption.placement = "top", align = "center",
                       include.rownames = tbl$includeRow, print.results = FALSE)
  }


  #Getting JS file  + creating JSObjects using jsonlite (refer to convertToJS function):
  jsData <- convertToJS(plot, tbl)


  #bind JSON to grid plot:
  gridSVG::grid.script(do.call("paste", c(jsData[-length(jsData)], sep="\n ")), inline = TRUE, name = "linkedJSONdata")

  svgOutput <- gridSVG::grid.export(NULL)$svg

  #get JS code associated with plot:
  jsCode <-  jsData$jsFile

  svgCode <- paste(capture.output(svgOutput), collapse = "\n")

  #remove svg file and other scripts:
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

  #scale columns according to devsize width - need to revise this - conflicts with png
  ##if (dev.size()[1] <= 8) {
  ##  HTMLtemplate <- gsub("(bsc)|(txsc)", 6, HTMLtemplate)
  ##} else if (dev.size()[1] <= 12 && dev.size()[1] > 8) {
  ##  #for svg:
  ##    HTMLtemplate <- gsub("bsc", round(dev.size()[1]-2), HTMLtemplate)
    #for table:
  ##    HTMLtemplate <- gsub("txsc", 12-round(dev.size()[1]-2) , HTMLtemplate)
  ##} else {
  ##  HTMLtemplate <- gsub("(bsc)|(txsc)", 12, HTMLtemplate)
  #}

  if (dev.size()[1] <= 8) {
    HTMLtemplate <- gsub("col-md-12 col-lg-12", "col-md-6 col-lg-6", HTMLtemplate)
  }

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
  return(invisible(url))

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

  #generation of table of counts:
  #plot <- x$all$all

  prop <- plot$phat
  counts <- plot$tab
  percent <- plot$widths
  n <- attributes(x)$total.obs
  if (attributes(x)$total.missing != 0) {
    n <- attributes(x)$total.obs - attributes(x)$total.missing
  }

  ##for color matching for colby:
  colorMatch <- plot$p.colby

  prop.df <- as.data.frame(t(prop))
  counts.df <- as.data.frame(counts)

  if (all(percent != 1)) { ## This condition is used to identify if it's a two way plot...
    ## different table for two way bar plots
    tab  = cbind(round(prop,4), format(round(rowSums(prop),4), nsmall = 4), rowSums(counts))
    colnames(tab)[(ncol(prop)+1):ncol(tab)] <- c("Total", "Row N")

  } else if (!is.null(colorMatch) && (all(c(0, 1) %in% colorMatch) == FALSE)) { ## for stacked bar plots

    ##creation of a special two-way table for stacked bars
    colorMatchRev <- colorMatch[nrow(colorMatch):1, ]
    proportions <- round(rbind(colorMatchRev, colSums(colorMatchRev)), 4)
    tab <- rbind(proportions,counts)
    rownames(tab)[nrow(proportions):nrow(tab)] <- c("Total", "Col N")

  } else {
    ##creating table for 1 way plots
    tab <- rbind(counts, round(prop*100,2))
    tab <- cbind(tab, rowSums(tab))
    tab[2,] <- paste0(tab[2,], "%")
    colnames(tab)[ncol(tab)] <- "Total"
    rownames(tab) <- c("Counts", "Percent")
  }

  #attributes for HTML table
  cap <- 'Table of Counts and Proportions'
  includeRow <- TRUE
  tableInfo <- list(caption = cap, includeRow = includeRow, tab = tab, n = n)

  return(tableInfo)

}

getTable.inzhist <- function(plot, x) {

  #extracting information:
  plot <- x$all$all
  #data <- data

  toPlot <- plot$toplot$all
  intervals <- toPlot$breaks
  counts <- toPlot$counts
  n <- attributes(x)$total.obs
  if (attributes(x)$total.missing != 0) {
    n <- attributes(x)$total.obs - attributes(x)$total.missing
  }

  #generation of HTML freq distribution table:
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

getTable.inzdot <- function(plot, x) {

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


    #Need to order.
    #data <- data[order(varX), ] # if plot.features order = -1 works, then remove this and the next line of code.
    #rownames(data) <- 1:nrow(data)

    #DEFAULT: only shows variables plotted.
    xVal <- plot$toplot$all$x
    tab <- as.data.frame(xVal)
    names(tab) <- attributes(x)$varnames$x

    if (!is.null(plot$toplot$all$colby)) {
      colby <- plot$toplot$all$colby
      tab <- cbind(tab, colby)
      names(tab)[ncol(tab)] <- attributes(x)$varnames$colby
    }

    if (!is.null(plot$toplot$all$sizeby)) {
      sizeby <- plot$toplot$all$sizeby
      tab <- cbind(tab, sizeby)
      names(tab)[ncol(tab)] <- attributes(x)$varnames$sizeby
    }

  }

  ##Attributes for HTML table
  cap <- "Data"
  includeRow <- FALSE
  tableInfo <- list(caption = cap, includeRow = includeRow, tab = tab, varNames = varNames)
  return(tableInfo)

}

getTable.inzscatter <- function(plot, x, data = NULL, extra.vars = NULL) {

  #For scatterplots, the user can choose to either export the whole dataset they've specified, or certain variables.
  #By default: only exports the two variables that are plotted (if no dataset is specified, or extra.vars = NULL).

  xVal <- plot$x
  yVal <- plot$y
  order <- plot$point.order

  tab <- cbind(as.data.frame(xVal), as.data.frame(yVal))
  names(tab) <- c(attributes(x)$varnames$x, attributes(x)$varnames$y)

  #if there's a colby variable
  if (!is.null(plot$colby)) {
    colby <- plot$colby
    tab <- cbind(tab, as.data.frame(colby))
    names(tab)[ncol(tab)] <- attributes(x)$varnames$colby
  }

  #if there's a sizeby variable
  if (!is.null(attributes(x)$varnames$sizeby)) {
    sizeby <- plot$propsize # TODO: might change this to actual values rather than proportions.
    tab <- cbind(tab, as.data.frame(sizeby))
    names(tab)[ncol(tab)] <- attributes(x)$varnames$sizeby
  }

  if (!is.null(extra.vars) && !is.null(data) && (extra.vars != 'all')) {

    #obtain column index
    colNum <- as.numeric(sapply(extra.vars, function(extra.vars) grep(extra.vars, colnames(data))))
    #obtain extra data columns
    extra.cols <- data[order, colNum]
    #bind altogether
    tab <- cbind(extra.cols, as.data.frame(xVal), as.data.frame(yVal))
    names(tab) <- c(extra.vars,attributes(x)$varnames$x, attributes(x)$varnames$y)

    #if there's a colby variable
    if (!is.null(plot$colby)) {
      colby <- plot$colby
      tab <- cbind(tab, as.data.frame(colby))
      names(tab)[ncol(tab)] <- attributes(x)$varnames$colby
    }

    #if there's a sizeby variable
    if (!is.null(attributes(x)$varnames$sizeby)) {
      sizeby <- plot$propsize
      tab <- cbind(tab, as.data.frame(sizeby))
      names(tab)[ncol(tab)] <- attributes(x)$varnames$sizeby
    }

  } else if ((ncol(data) < 10) && !is.null(data)) {

    tab <- data[order, ]

  } else if (is.null(data) && !is.null(extra.vars)) {

    warning("Error: no dataset specified to export extra variables! Please specify a dataset.
            Returning a static HTML plot.")
    return()

  } else {
    tab <- tab
  }

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


## generating data into JSON + JS code:

convertToJS <- function(plot, tbl= NULL) UseMethod("convertToJS")

convertToJS.inzbar <- function(plot, tbl) {

  #plot <- x$all$all
  prop <- plot$phat
  counts <- plot$tab
  percent <- plot$widths
  colorMatch <- plot$p.colby

  prop.df <- as.data.frame(t(prop))
  counts.df <- as.data.frame(counts)

  if (all(percent != 1)) { ##this is for stacked bar plots and two way bar plots
    prop.df =  as.data.frame(prop)
    percent.df = as.data.frame(percent)
    percent.df$percent = round(percent.df$percent, 4)
    percentJSON = paste0("var percent = ", jsonlite::toJSON(percent.df), ";")
    colCounts = paste0("var colCounts = ", jsonlite::toJSON(c("Col N", round(colSums(counts)/tbl$n,4), 1)), ";")

  } else {
    percentJSON = "null"
    colCounts = "null"
  }



  countsJ <- jsonlite::toJSON(counts.df)
  orderJSON = 'var order = null;'

  #selecting appropriate JS code:
  jsFile <- bpJS

  #Differentiating between one way bar plots - stacked bars, colored, and non-colored:
  if (is.null(colorMatch)) { ## test if bar plots have color
    colorMatchJSON = paste("var colorMatch = null;", sep = "");

  } else if ((all(c(0, 1) %in% colorMatch) == TRUE)){ ##test if the bar plot colby is the same

    colorMatchJSON = paste("var colorMatch = '", gsub(",", "", jsonlite::toJSON(as.vector(colorMatch))), "';", sep ="")

  } else {

    ## required for stacked bar plots, due to make up of polygons being reversed when plotted
    order <- matrix(1:length(colorMatch), ncol=ncol(colorMatch), byrow = TRUE)
    orderJ <- jsonlite::toJSON(as.vector(apply(order,1 ,rev)))
    orderJSON <- paste0("var order = ", orderJ, ";" )

    colorMatch.df = as.data.frame(colorMatch[nrow(colorMatch):1, ])
    colorMatchJ = jsonlite::toJSON(colorMatch.df)
    colorMatchJSON = paste("var colorMatch = ", colorMatchJ, ";", sep = "");
    countsJ = jsonlite::toJSON(counts.df[rev(rownames(counts.df)),])

    ## setting JS: stacked bar plots currently run on a different JS file
    jsFile <- bpstackedJS

  }


  #writing JS code - JSON data:
  propJSON = paste("var prop = ", jsonlite::toJSON(prop.df), ";", sep = "");
  countsJSON = paste("var counts = ", countsJ, ";", sep = "");

  #returning all data in a list:
  JSData <- list(prop = propJSON, counts = countsJSON, percent = percentJSON, colCounts = colCounts, colorMatch = colorMatchJSON, order = orderJSON, jsFile = jsFile)
  return(JSData)

}

convertToJS.inzhist <- function(plot, tbl) {

  #extracting information:
  ## plot <- x$all$all
  toPlot <- plot$toplot$all

  intervals <- toPlot$breaks
  counts <- toPlot$counts
  prop <- round(counts/tbl$n, 4) ##toPlot$density

  #To obtain box whisker plot information:
  boxInfo <- plot$boxinfo$all
  quantiles <-boxInfo$quantiles
  min <- boxInfo$min
  max <- boxInfo$max
  boxTable <- rbind(as.data.frame(quantiles), min, max)
  rownames(boxTable)[4:5] <- c("min", "max")

  ## Conversion to JSON data:
  intervalJSON <- paste0("var intervals = ", jsonlite::toJSON(intervals), ";")
  countsJSON <- paste0("var counts = ", jsonlite::toJSON(counts), ";")
  propJSON <- paste0("var prop = ", jsonlite::toJSON(prop), ";")
  boxJSON <- paste0("var boxData = ", jsonlite::toJSON(boxTable), ";")
  n <- paste0("var n = ", tbl$n)

  ## JS code:
  jsFile <- histJS

  ##Output as a list:
  JSData <- list(intervals = intervalJSON, counts = countsJSON, prop = propJSON, box = boxJSON, n = n, jsFile = jsFile)

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

      #currently only takes variable plotted
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

    ## Conversion to JSON data:
    boxJSON <- paste0("var boxData = ", jsonlite::toJSON(boxTable), ";");

    #JS:
    jsFile <- dpspJS

    #list:
    JSData <- list(names = namesJSON, table = tabJSON, colGroupNo = colGroupNo, box = boxJSON, jsFile = jsFile)
  }

  return(JSData)
}

convertToJS.inzscatter <- function(plot, tbl) {

  colGroupNo <- nlevels(plot$colby)

  namesJSON <- paste0("var names = ", jsonlite::toJSON(names(tbl$tab)), ";")
  tabJSON <- paste0("var tableData = ", jsonlite::toJSON(tbl$tab), ";")
  colGroupNo <- paste0("colGroupNo = ", colGroupNo, ";")

  #JS file:
  jsFile <- dpspJS

  #list:
  JSData <- list(names = namesJSON, tabs = tabJSON, colGroupNo = colGroupNo, jsFile = jsFile)

  return(JSData)
}

#Trialling hex plots:
convertToJS.inzhex <- function(plot, tbl = NULL) {

  #Hexplots use S4 notation:
  counts <- plot$hex@count
  xcm <- plot$hex@xcm
  ycm <- plot$hex@ycm
  n <- plot$hex@n

  countsJSON <- paste0("var counts = ", jsonlite::toJSON(counts), ";")
  xcmJSON <- paste0("var xcm = ", jsonlite::toJSON(xcm), ";")
  ycmJSON <- paste0("var ycm = ", jsonlite::toJSON(ycm), ";")
  n <- paste0("var n =", n)

  #JS file:
  jsFile <- hexbinJS

  #list:
  JSData <- list(counts = countsJSON, xcm = xcmJSON, ycm = ycmJSON, n = n, jsFile = jsFile)

  return(JSData)

}

convertToJS.default <- function(plot, tbl = NULL) {
  warning("No JS available. :(")
  return()
}
