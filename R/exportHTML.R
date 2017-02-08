exportHTML <- function(x, file) UseMethod("exportHTML")

exportHTML.function <- function(x, file, width = dev.size()[1], height = dev.size()[2]) {
  t <- tempfile()
  pdf(t, height = height, width = width)
  obj <- x()
  out <- exportHTML(obj, file)
  dev.off()
  ## delete pdf too
  return(out)
}

exportHTML.inzplotoutput <- function(x, file) {
  plot <- x$all$all
  if (is.null(plot)) {
    warning("iNZight doesn't handle interactive panel plots ... yet!")
    return()
  }
  
  dir <- tmpdir()
  curdir <- getwd()
  setwd(dir)
  svg <- grid.export("inzightplot.svg", con)
  close(con)
  setwd(curdir)
  
  ## now work with files in `dir`
  svgpath <- file.path(dir, "inzightplot.svg")
  
  tbl <- getTable(plot)
  
  return(htmlfile)
}

# updatePlot <- function() iNZightPlot(x, y)
# exportHTML(updatePlot, file)



getTable <- function(plot) UseMethod("getTable")
  
getTable.inzdot <- function(plot) {
  # get the table from plot  
}
