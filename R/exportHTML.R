exportHTML <- function(x, file) UseMethod("exportHTML")

exportHTML.function <- function(x, file, width = dev.size()[1], height = dev.size()[2]) {
  t <- tempfile()
  pdf(t, height = height, width = width)
  obj <- x()
  exportHTML(obj, file)
  dev.off()
  file.remove(t)
}

exportHTML.inzplotoutput <- function(x, file = "inzightplot.html") {
  plot <- x$all$all
  if (is.null(plot)) {
    warning("iNZight doesn't handle interactive panel plots ... yet!")
    return()
  }

  dir <- tmpdir()
  curdir <- getwd() 
  setwd(dir)
    
  x <<- x
  
  tbl <- getTable(plot)
  
  #write table in HTML using xtable:
  HTMLtable <- print(xtable(tbl$tab, caption = tbl$cap, auto = TRUE), 
                     type = "html", html.table.attributes= 'class="table table-striped table-bordered" id="table"', 
                     caption.placement = "top", align = "center", 
                     include.rownames = tbl$includeRow, print.results = FALSE)
  
   #Getting JS file  + creating JSObjects using jsonlite (refer to convertToJS):
  jsData <- convertToJS(plot, tbl)
  
  #bind JSON to plot: 
  grid.script(do.call("paste", c(jsData[-length(jsData)], sep="\n ")), inline = TRUE, name = "linkedJSONdata")
  
  grid.export("inzightplot.svg")
  
  #write JS script code for HTML:
  jsFile <-  jsData$jsFile
  jsCode <- paste("<script defer type = 'text/javascript' src='", jsFile, "' > </script>")

  
  svgCode <- paste(readLines("inzightplot.svg", warn =FALSE), collapse="\n")
  
  #remove svg file and other scripts:
  file.remove('inzightplot.svg')
  grid.remove("linkedJSONdata")
  
  #finding places where to substitute code:
  svgLine <- grep("SVG", HTMLtemplate)
  jsLine <- grep("JSfile", HTMLtemplate)
  tableLineOne <- grep("table", HTMLtemplate)
  
  # substitution of JS code:
  HTMLtemplate[jsLine] = jsCode
  HTMLtemplate[tableLineOne] = HTMLtable
  HTMLtemplate[svgLine] = svgCode
  
  #Writing it out to a temp HTML file and opening it:
  write(HTMLtemplate, file)
  browseURL(paste('file://', file.path(getwd(), file))) 

  #reset back to original directory:
  setwd(curdir)
 
}
  
  return(file)
}

# updatePlot <- function() iNZightPlot(x, y)
# exportHTML(updatePlot, file)


## Generating tables for each plot:
  
getTable <- function(plot) UseMethod("getTable")

getTable.inzbar <- function(plot) {
  
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

getTable.inzhist <- function(plot) {
  
  #extracting information:
  plot <- x$all$all
  #data <- data
  
  toPlot <- plot$toplot$all
  intervals <- toPlot$breaks
  counts <- toPlot$counts

  #To obtain box whisker plot information:
  box <- plot$boxinfo$all
  quantiles <- box$quantiles
  min <- box$min
  max <- box$max
  boxTable <- rbind(as.data.frame(quantiles), min, max)
  rownames(boxTable)[4:5] <- c("min", "max")
  
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
  tableInfo <- list(cap, includeRow, tab)
  names(tableInfo) <- c('caption', 'includeRow', 'tab')
  
  return(tableInfo)
  
}

#for scatterplots and dot plots it currently only shows 1 / double variable (+ colby) that's being plotted.
#exporting additional variables/data has not been set up yet....

getTable.inzdot <- function(plot) {
  
  #Need to order if additional variables are exported. 
  #data <- data[order(varX), ] # if plot.features list(order = -1) works...
  #rownames(data) <- 1:nrow(data)
  
  #DEFAULT: imports just that one variable.
  xVal <- plot$toplot$all$x
  tab <- as.data.frame(xVal) 
  names(tab) <- attributes(x)$varnames$x 
  
  if (!is.null(plot$toplot$all$colby)) {
    colby <- plot$toplot$all$colby 
    tab <- cbind(tab, colby)
    names(tab)[2] <- attributes(x)$varnames$colby 
  }
  
  ##Attributes for HTML table
  cap <- "Data"
  includeRow <- TRUE
  tableInfo <- list(cap, includeRow, tab)
  names(tableInfo) <- c('caption', 'includeRow', 'tab')
  
  return(tableInfo)
  
}

getTable.inzscatter <- function(plot) { 
  #this depends on what the user chooses if they wish to display more variables other than those plotted.
  #MUST SET ORDER (plot features) to -1 if they choose to add more variables to the table...

  #DEFAULT: only shows the two variables.
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
  includeRow <- TRUE
  tableInfo <- list(cap, includeRow, tab)
  names(tableInfo) <- c('caption', 'includeRow', 'tab')
  return(tableInfo)
    
} 

#Not set up yet! - for future purposes
getTable.inzhex <- function(plot) {
  warning("Hexbin plots currently do not have a table... yet!")
  return()
}

getTable.default <- function(plot) {
  warning("Cannot generate table!")
  return()
}

  
## generating data into JSON + JS code:

convertToJS <- function(plot, tbl= NULL) {
  UseMethod("convertToJS")
}

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
      percentJSON = paste("var percent = '", toJSON(percent.df), "';", sep = "");
      
    } else {
      percentJSON = "null";
    }
    
    countsJ <- toJSON(counts.df)
    orderJSON = 'var order = null;'
    
    #writing and getting the javascript file:
    write(bpJS, file = 'bp.js')
    jsFile <- 'bp.js'
    
    #Differentiating between one way bar plots - stacked bars, colored, and non-colored:
    if (is.null(colorMatch)) { ## test if bar plots have color
      colorMatchJSON = paste("var colorMatch = null;", sep = "");
      
    } else if ((all(c(0, 1) %in% colorMatch) == TRUE)){ ##test if the bar plot colby is the same
      
      colorMatchJSON = paste("var colorMatch = '", gsub(",", "", toJSON(as.vector(colorMatch))), "';", sep ="")
      
    } else { 
      
      ## required for stacked bar plots, due to make up of polygons being reversed when plotted
      order <- matrix(1:length(colorMatch), ncol=ncol(colorMatch), byrow = TRUE)
      orderJ <- toJSON(as.vector(apply(order,1 ,rev)))
      orderJSON <- paste0("var order = '", orderJ, "';" )
      
      colorMatch.df = as.data.frame(colorMatch[nrow(colorMatch):1, ])
      colorMatchJ = toJSON(colorMatch.df)
      colorMatchJSON = paste("var colorMatch = '", colorMatchJ, "';", sep = "");
      countsJ = toJSON(counts.df[rev(rownames(counts.df)),])
      
      ## setting JS: stacked bar plots currently run on a different JS file
      write(bpstackedJS, file = 'bpstacked.js')
      jsFile <- 'bpstacked.js'
      
    }
    
    
    #writing JS code - JSON data:
    propJSON = paste("var prop = '", toJSON(prop.df), "';", sep = "");
    countsJSON = paste("var counts = '", countsJ, "';", sep = "");
    
    #returning all data in a list:
    JSData <- list(propJSON, countsJSON, percentJSON, colorMatchJSON, orderJSON, jsFile)
    names(JSData) <- c('propJSON', 'countsJSON', 'percentJSON', 'colorMatchJSON', 'orderJSON', 'jsFile')
      
   return(JSData)
    
  }

convertToJS.inzhist <- function(plot, tbl = NULL) {
  
  #extracting information:
  ## plot <- x$all$all
  toPlot <- plot$toplot$all
  
  intervals <- toPlot$breaks
  counts <- toPlot$counts
  prop <- round(counts/attributes(x)$total.obs, 4) ##toPlot$density differs? 
  
  #To obtain box whisker plot information:
  boxInfo <- plot$boxinfo$all
  quantiles <-boxInfo$quantiles
  min <- boxInfo$min
  max <- boxInfo$max
  boxTable <- rbind(as.data.frame(quantiles), min, max)
  rownames(boxTable)[4:5] <- c("min", "max")
  
  ## Conversion to JSON data:
  intervalJSON <- paste0("var intervals = '", toJSON(intervals), "';")
  countsJSON <- paste0("var counts = '", toJSON(counts), "';")
  propJSON <- paste0("var prop = '", toJSON(prop), "';")
  boxJSON <- paste0("var boxData = '", toJSON(boxTable), "';");

  ##JS file in temp directory:
   write(histJS, file = 'histogram.js')
    jsFile <- 'histogram.js'
  
  ##Output as a list:
  JSData <- list(intervalJSON, countsJSON, propJSON, boxJSON, jsFile)
  names(JSData) <- c('intervalJSON', 'countsJSON', 'propJSON', 'boxJSON', 'jsFile')
  
  return(JSData)
  
}

convertToJS.inzdot <- function(plot, tbl) {

   #Extracting information:
  colGroupNo <- nlevels(plot$toplot$all$colby)
  
  namesJSON <- paste0("var names = '", toJSON(names(tbl$tab)), "';")
  tabJSON <- paste0("var tableData = '", toJSON(tbl$tab), "';")
  colGroupNo <- paste0("colGroupNo = ", colGroupNo, ";")
  
  #To obtain box whisker plot information:
  boxInfo <- plot$boxinfo$all
  quantiles <-boxInfo$quantiles
  min <- boxInfo$min
  max <- boxInfo$max
  boxTable <- rbind(as.data.frame(quantiles), min, max)
  rownames(boxTable)[4:5] <- c("min", "max")
  
  ## Conversion to JSON data:
  boxJSON <- paste0("var boxData = '", toJSON(boxTable), "';");
  
  #JS:
    write(dpspJS, file = 'dpsp.js')
    jsFile <- 'dpsp.js'
  
   #list:
   JSData <- list(namesJSON, tabJSON, colGroupNo, boxJSON, jsFile)
   names(JSData) <- c('namesJSON', 'tabJSON', 'colGroupNo', ' boxJSON', 'jsFile')
   
   return(JSData)
}

convertToJS.inzscatter <- function(plot, tbl) {
  
  colGroupNo <- nlevels(plot$colby)
  
  namesJSON <- paste0("var names = '", toJSON(names(tbl$tab)), "';")
  tabJSON <- paste0("var tableData = '", toJSON(tbl$tab), "';")
  colGroupNo <- paste0("colGroupNo = ", colGroupNo, ";")

#JS file:
 write(dpspJS, file = 'dpsp.js')
    jsFile <- 'dpsp.js'
 
 #list:
 JSData <- list(namesJSON, tabJSON, colGroupNo, jsFile)
 names(JSData) <- c("namesJSON", "tabJSON", "colGroupNo", "jsFile")
 
  return(JSData)
}

#Not set up yet! - for future purposes
convertToJS.inzhex <- function(plot, tbl = NULL) {
  
  warning("No JS available for hexbin plots... yet!")
  return()
  
}

convertToJS.default <- function(plot, tbl = NULL) {
  warning("No JS available. :(")
  return()
}
