## This script is for updating the sysdata.rda that contains the HTML template and JS files for each iNZight plot.
#Requires devtools:

library(devtools)

#load HTML template: change path to wherever it's stored...
HTMLtemplate <- readLines(file.path('~/Desktop/jsFinal', 'template.html'))

#load JS files:
bpJS <- readLines(file.path('~/Desktop/jsFinal', 'bp.js'))
bpstackedJS <- readLines(file.path('~/Desktop/jsFinal', 'bp-stacked.js'))
dpspJS <- readLines(file.path('~/Desktop/jsFinal', 'dpsp.js'))
histJS <- readLines(file.path('~/Desktop/jsFinal', 'histogram.js'))

#For later: hex plots
#hexJS <- readLines(file.path('~/Desktop/', 'hexbin.js'))

#set working directory into iNZightPlots package
setwd('~/Desktop/iNZightPlots/')
#add/delete files where necessary: - what is written here would be the same in the exportHTML function.
use_data(HTMLtemplate, bpJS, bpstackedJS, dpspJS, histJS, internal = TRUE, overwrite = TRUE)

## TEST RUN:
load_all()  
#or any of the files you've added.


##shortened version: - using a list?
#files <- list('bp.js', 'bp-stacked.js', 'dpsp.js', 'histogram.js')
#filePaths <- do.call(file.path, list('~/Desktop/jsFinal', files))
#hello <- lapply(filePaths, readLines)


