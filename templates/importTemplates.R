## This script is for updating the sysdata.rda that contains the HTML template and JS files for each iNZight plot.
#Requires devtools:

dirname <- "templates"

#load HTML template: change path to wherever it's stored...
HTMLtemplate <- readLines(file.path(dirname, 'template.html'))

#load CSS:
styles <- readLines(file.path(dirname, 'style.css'))

#load singleFunctions file:
singleFunctions <- readLines(file.path(dirname, 'singleFunctions.js'))

#load JS files:
bpJS <- readLines(file.path(dirname, 'bp.js'))
bpstackedJS <- readLines(file.path(dirname, 'bp-stacked.js'))
dpspJS <- readLines(file.path(dirname, 'dpsp.js'))
histJS <- readLines(file.path(dirname, 'histogram.js'))
hexbinJS <- readLines(file.path(dirname, 'hexbin.js'))

devtools::use_data(HTMLtemplate, styles, singleFunctions, bpJS, bpstackedJS, dpspJS, histJS, hexbinJS, internal = TRUE, overwrite = TRUE)

#add/delete files where necessary: - what is written here would be the same in the exportHTML function.

## TEST RUN:
#load_all()
#or any of the files you've added.


##shortened version: - using a list?
#files <- list('bp.js', 'bp-stacked.js', 'dpsp.js', 'histogram.js')
#filePaths <- do.call(file.path, list('~/Desktop/jsFinal', files))
#hello <- lapply(filePaths, readLines)
