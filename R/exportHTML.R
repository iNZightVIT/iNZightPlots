#' @title ExportHTML
#'
#' @description \code{exportHTML} is designed to export the iNZight plot as a dynamic, interactive HTML page.
#'  It opens the written HTML page in a web browser.
#' Currently, it only handles single panel plots. Exporting additional variables, multi-panel plots and coloured hex plots are currently not available yet.
#'
#' @details
#' It generates an appropriate HTML table, converts data objects into JSON and retrieves a JavaScript file based upon the plot. It converts the plot to an svg,
#' and inserts the svg plot, table, and JavaScript into the an HTML template to produce the page that is viewed in the browser.
#' This function acts as generic function (either takes in function or iNZight plot object) and is comprised of two other functions.
#' \code{getTable} aims to construct the appropriate table for the plot using information stored in the iNZight plot object, or data provided.
#' \code{convertToJS} converts appropriate data into JSON and writes the appropriate JS file to give interactivity to the HTML page.
#'
#' @param x An iNZight plot object or function (such as updatePlot) that captures iNZight environment
#' @param file Name of temporary HTML file generated
#'
#' @return Opens up an HTML file of \code{x} with filename \code{file} in the browser (best performance on Chrome/Firefox)
#'
#' @author Yu Han Soh
#'
#' @export
exportHTML <- function(x, file) UseMethod("exportHTML")

exportHTML.function <- function(x, file = 'index.html', width = dev.size()[1], height = dev.size()[2]) {

  #get current directory
  curdir <- getwd()
  on.exit(setwd(curdir))

  #set to temp directory
  tdir <- tempdir()
  setwd(tdir)

  #create pdf graphics device into here:
  pdf('tempfile.pdf', width = width, height = height, onefile = TRUE)

  #do exporting:
  obj <- x()
  exportHTML(obj, file)

  #turn off device:
  dev.off()

  #remove pdf:
  file.remove('tempfile.pdf')

  #reset back to original directory:
  setwd(curdir)


}

#this is generalized for every plot - involves the binding of everything together.
exportHTML.inzplotoutput <- function(x, file = 'index.html') {

  curdir <- getwd()
  x <- x
  plot <- x$all$all


  if (is.null(plot)) {
    warning("iNZight doesn't handle interactive panel plots ... yet!")
    return()
  }

  #condition for 1 categorical + 1 continuous variable
  if(length(plot$toplot) > 1) {
    warning('iNZight cannot handle interactive multi-factor dot plots yet!')
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
  tbl <- getTable(plot, x)

  #write table in HTML using xtable:
  if (is.null(tbl)) {
    HTMLtable <- '<p> No table available. </p>'
  } else {

    HTMLtable <- print(xtable::xtable(tbl$tab, caption = tbl$cap, auto = TRUE),
                     type = "html", html.table.attributes= 'class="table table-hover table-striped table-bordered hidden" id="table"',
                     caption.placement = "top", align = "center",
                     include.rownames = tbl$includeRow, print.results = FALSE)
  }

  #Getting JS file  + creating JSObjects using jsonlite (refer to convertToJS function):
  jsData <- convertToJS(plot, tbl)

  #bind JSON to grid plot:
  gridSVG::grid.script(do.call("paste", c(jsData[-length(jsData)], sep="\n ")), inline = TRUE, name = "linkedJSONdata")

  gridSVG::grid.export("inzightplot.svg")

  #get JS code associated with plot:
  jsCode <-  jsData$jsFile

  svgCode <- paste(readLines("inzightplot.svg", warn =FALSE), collapse="\n")

  #remove svg file and other scripts:
  file.remove('inzightplot.svg')
  grid.remove("linkedJSONdata")

  #finding places where to substitute code:
  svgLine <- grep("SVG", HTMLtemplate)
  cssLine <- grep("styles.css", HTMLtemplate)
  jsLine <- grep("JSfile", HTMLtemplate)
  tableLineOne <- grep("table", HTMLtemplate)

  # insert inline JS, CSS, table, SVG:
  HTMLtemplate[cssLine] <- paste(styles, collapse = "\n")
  HTMLtemplate[jsLine] <- paste(jsCode, collapse = "\n")
  HTMLtemplate[tableLineOne] <- HTMLtable
  HTMLtemplate[svgLine] <- svgCode

  #if the dev.size width is greater than 9, switch to large columns:
  if (dev.size()[1] > 9) {
    HTMLtemplate <- gsub("col-lg-6", "col-lg-12", HTMLtemplate)
    HTMLtemplate <- gsub("col-md-6", "col-md-12", HTMLtemplate)
  }

  #Step 4: Writing it out to an HTML file:
  write(HTMLtemplate, file)

  #Step 5: Open HTML file: - should be stored in temporary file
  browseURL(file.path(file))

  #reset back to original directory:
  setwd(curdir)

}


## Generating tables for each plot:

getTable <- function(plot, x= NULL)  {
  UseMethod("getTable")
}

getTable.inzbar <- function(plot, x = NULL) {

  #generation of table of counts:
  #plot <- x$all$all

  prop <- plot$phat
  counts <- plot$tab
  percent <- plot$widths

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
  tableInfo <- list(cap, includeRow, tab)
  names(tableInfo) <- c('caption', 'includeRow', 'tab')

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
  tableInfo <- list(cap, includeRow, tab, n)
  names(tableInfo) <- c('caption', 'includeRow', 'tab', 'n')

  return(tableInfo)

}

#for scatterplots and dot plots it currently only shows variables (+ colby) being plotted.
#exporting additional variables has not been set up yet...

getTable.inzdot <- function(plot, x) {

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

  tab <- tab

  if (!is.null(plot$toplot$all$sizeby)) {
    sizeby <- plot$toplot$all$sizeby
    tab <- cbind(tab, sizeby)
    names(tab)[ncol(tab)] <- attributes(x)$varnames$sizeby
  }


  ##Attributes for HTML table
  cap <- "Data"
  includeRow <- FALSE
  tableInfo <- list(cap, includeRow, tab, missing)
  names(tableInfo) <- c('caption', 'includeRow', 'tab')

  return(tableInfo)

}

getTable.inzscatter <- function(plot, x) {
  #this depends on what the user chooses if they wish to display more variables other than those plotted.
  #MUST SET ORDER (plot features) to -1 if they choose to add more variables to the table (or according to plotting sequence...)

  #DEFAULT:  # to only show variables plotted.
    xVal <- plot$x
    yVal <- plot$y
    tab <- cbind(as.data.frame(xVal), as.data.frame(yVal))
   names(tab) <- c(attributes(x)$varnames$x, attributes(x)$varnames$y)

    if (!is.null(plot$colby)) {
      colby <- plot$colby
      tab <- cbind(tab, as.data.frame(colby))
      names(tab)[3] <- attributes(x)$varnames$colby
    }

    ## Attributes for HTML table
  cap <- "Data"
  includeRow <- FALSE
  tableInfo <- list(cap, includeRow, tab)
  names(tableInfo) <- c('caption', 'includeRow', 'tab')
  return(tableInfo)

}

#No table for hexplots yet!
getTable.inzhex <- function(plot, x = NULL) {
  warning("No table available for hexbin plots.")
  tableInfo <- NULL;
  return(tableInfo);


}

getTable.default <- function(plot, x = NULL) {
  warning("Cannot generate table! There may not be an interactive version of this plot yet...")
  return()
}


## generating data into JSON + JS code:

convertToJS <- function(plot, tbl= NULL) UseMethod("convertToJS")

convertToJS.inzbar <- function(plot, tbl = NULL) {

   #plot <- x$all$all

    prop <- plot$phat
    counts <- plot$tab
    percent <- plot$widths

    colorMatch <- plot$p.colby

    prop.df <- as.data.frame(t(prop))
    counts.df <- as.data.frame(counts)

    if (all(percent != 1)) { ##this is for stacked bar plots and two way bar plots
      prop.df <-  as.data.frame(prop)
      percent.df = as.data.frame(percent)
      percent.df$percent = round(percent.df$percent, 4)
      percentJSON = paste("var percent = '", jsonlite::toJSON(percent.df), "';", sep = "");

    } else {
      percentJSON = "null";
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
      orderJSON <- paste0("var order = '", orderJ, "';" )

      colorMatch.df = as.data.frame(colorMatch[nrow(colorMatch):1, ])
      colorMatchJ = jsonlite::toJSON(colorMatch.df)
      colorMatchJSON = paste("var colorMatch = '", colorMatchJ, "';", sep = "");
      countsJ = jsonlite::toJSON(counts.df[rev(rownames(counts.df)),])

      ## setting JS: stacked bar plots currently run on a different JS file
      jsFile <- bpstackedJS

    }


    #writing JS code - JSON data:
    propJSON = paste("var prop = '", jsonlite::toJSON(prop.df), "';", sep = "");
    countsJSON = paste("var counts = '", countsJ, "';", sep = "");

    #returning all data in a list:
    JSData <- list(propJSON, countsJSON, percentJSON, colorMatchJSON, orderJSON, jsFile)
    names(JSData) <- c('propJSON', 'countsJSON', 'percentJSON', 'colorMatchJSON', 'orderJSON', 'jsFile')

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
  intervalJSON <- paste0("var intervals = '", jsonlite::toJSON(intervals), "';")
  countsJSON <- paste0("var counts = '", jsonlite::toJSON(counts), "';")
  propJSON <- paste0("var prop = '", jsonlite::toJSON(prop), "';")
  boxJSON <- paste0("var boxData = '", jsonlite::toJSON(boxTable), "';")
  n <- paste0("var n = ", tbl$n)

  ## JS code:
    jsFile <- histJS

  ##Output as a list:
  JSData <- list(intervalJSON, countsJSON, propJSON, boxJSON, n, jsFile)
  names(JSData) <- c('intervalJSON', 'countsJSON', 'propJSON', 'boxJSON', 'n', 'jsFile')

  return(JSData)

}

convertToJS.inzdot <- function(plot, tbl) {

   #Extracting information:
  colGroupNo <- nlevels(plot$toplot$all$colby)

  namesJSON <- paste0("var names = '", jsonlite::toJSON(names(tbl$tab)), "';")
  tabJSON <- paste0("var tableData = '", jsonlite::toJSON(tbl$tab), "';")
  colGroupNo <- paste0("colGroupNo = ", colGroupNo, ";")

  #To obtain box whisker plot information:
  boxInfo <- plot$boxinfo$all
  quantiles <-boxInfo$quantiles
  min <- boxInfo$min
  max <- boxInfo$max
  boxTable <- rbind(as.data.frame(quantiles), min, max)
  rownames(boxTable)[4:5] <- c("min", "max")

  ## Conversion to JSON data:
  boxJSON <- paste0("var boxData = '", jsonlite::toJSON(boxTable), "';");

  #JS:
    jsFile <- dpspJS

   #list:
   JSData <- list(namesJSON, tabJSON, colGroupNo, boxJSON, jsFile)
   names(JSData) <- c('namesJSON', 'tabJSON', 'colGroupNo', ' boxJSON', 'jsFile')

   return(JSData)
}

convertToJS.inzscatter <- function(plot, tbl) {

  colGroupNo <- nlevels(plot$colby)

  namesJSON <- paste0("var names = '", jsonlite::toJSON(names(tbl$tab)), "';")
  tabJSON <- paste0("var tableData = '", jsonlite::toJSON(tbl$tab), "';")
  colGroupNo <- paste0("colGroupNo = ", colGroupNo, ";")

  #JS file:
    jsFile <- dpspJS

  #list:
  JSData <- list(namesJSON, tabJSON, colGroupNo, jsFile)
  names(JSData) <- c("namesJSON", "tabJSON", "colGroupNo", "jsFile")

  return(JSData)
}

#Trialling hex plots:
convertToJS.inzhex <- function(plot, tbl = NULL) {

  #Hexplots use S4 notation:
  counts <- plot$hex@count
  xcm <- plot$hex@xcm
  ycm <- plot$hex@ycm
  n <- plot$hex@n

  countsJSON <- paste0("var counts = '", jsonlite::toJSON(counts), "';")
  xcmJSON <- paste0("var xcm = '", jsonlite::toJSON(xcm), "';")
  ycmJSON <- paste0("var ycm ='", jsonlite::toJSON(ycm), "';")
  n <- paste0("var n =", n)

  #JS file:
  jsFile <- hexbinJS

  #list:
  JSData <- list(countsJSON, xcmJSON, ycmJSON, n, jsFile)
  names(JSData) <- c("countsJSON", "xcmJSON", "ycmJSON", "n", "jsFile")

  return(JSData)

}

convertToJS.default <- function(plot, tbl = NULL) {
  warning("No JS available. :(")
  return()
}
