## DEV-SCRIPT: updating the sysdata.rda that contains the interactive scripts
# for each iNZight plot.
dirname <- "templates"
# HTML template, CSS
HTMLtemplate <- readLines(file.path(dirname, 'template.html'))
styles <- paste(readLines(file.path(dirname, 'style.css')), collapse = "\n")

#load JS files
files <- list(inzJS = "inzplot.js", bpJS = "barplot.js", dpspJS = "dpsp.js",
              histJS = "histogram.js", hexbinJS = "hexbin.js", mapsJS = "maps.js")
filePaths <- do.call(file.path, list(dirname, files))
jsFiles <- lapply(filePaths, function(x) {
  paste(readLines(x), collapse = "\n")
})

names(jsFiles) <- names(files)

devtools::use_data(HTMLtemplate, styles, jsFiles, internal = TRUE, overwrite = TRUE)
