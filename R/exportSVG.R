#' @title Export iNZightPlots as an SVG
#'
#' @description \code{exportSVG} is designed to export the iNZight plot as a temporary SVG that is opened in a web browser.
#' The iNZightPlot must be drawn to a graphics device before exporting can occur.
#'
#' @param x iNZight plot object or function that captures iNZight environment
#' @param file Name of temporary svg file generated (by default: 'inzightplot.svg')
#' @return Opens up an SVG file of \code{x} with filename \code{file} in a web browser
#' @author Yu Han Soh
#' @export
exportSVG <- function(x, file) UseMethod('exportSVG')

exportSVG.function <- function(x, file = 'inzightplot.svg', width = dev.size()[1], height = dev.size()[2]) {
  #get current directory
  curdir <- getwd()

  #set directory to temp directory
  tdir <- tempdir()
  setwd(tdir)

  #create pdf graphics device into here:
  pdf('tempfile.pdf', width = width, height = height, onefile = TRUE)

  #do exporting:
  obj <- x()
  exportSVG(obj, file)

  #turn off device:
  dev.off()

  #remove pdf:
  file.remove('tempfile.pdf')

  #reset back to original directory:
  setwd(curdir)

}

exportSVG.inzplotoutput <- function(x, file = 'inzightplot.svg') {

  curdir <- getwd()
  #work in a temp directory
  setwd(tempdir())

  gridSVG::grid.export(file)

  #open in browser?
  browseURL(file.path(file))

  #return:
  setwd(curdir)

}
