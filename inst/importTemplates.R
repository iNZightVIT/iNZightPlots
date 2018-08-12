## DEV-SCRIPT: updating the sysdata.rda that contains the interactive scripts
# for each iNZight plot.
dirname <- "inst"
# HTML template, CSS
HTMLtemplate <- readLines(file.path(dirname, 'template.html'))
styles <- paste(readLines(file.path(dirname, 'style.css')), collapse = "\n")

# store vendor/library files
#vendorFiles <- list(jquery.min.js = "https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js",
#                    d3.v4.min.js = "https://d3js.org/d3.v4.min.js",
#                    jquery.dataTables.min.js = "https://cdn.datatables.net/1.10.16/js/jquery.dataTables.min.js",
#                    bootstrap.min.js = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/js/bootstrap.min.js",
#                    dataTables.bootstrap.min.js = "https://cdn.datatables.net/1.10.16/js/dataTables.bootstrap.min.js",
#                    dataTables.bootstrap.min.css = "https://cdn.datatables.net/1.10.16/css/dataTables.bootstrap.min.css",
#                    bootstrap.min.css = "https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css")
#
#vendor <- lapply(vendorFiles, function(x) {
#  con <- url(x)
#  text <- paste(readLines(con), collapse = "\n")
#  close(con)
#  return(text)
#})

#names(vendor) <- names(vendorFiles)
#assets <- system.file("templates", "assets.zip")

#load JS files
files <- list(inzJS = "inzplot.js", bpJS = "barplot.js", dpspJS = "dpsp.js",
              histJS = "histogram.js", hexbinJS = "hexbin.js", mapsJS = "maps.js")
filePaths <- do.call(file.path, list(dirname, files))
jsFiles <- lapply(filePaths, function(x) {
  paste(readLines(x), collapse = "\n")
})

names(jsFiles) <- names(files)

devtools::use_data(HTMLtemplate, styles, jsFiles, internal = TRUE, overwrite = TRUE)
