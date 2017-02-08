exportHTML <- function(plot, file) {
  dir <- tmpdir()
  curdir <- getwd()
  setwd(dir)
  svg <- grid.export("inzightplot.svg", con)
  close(con)
  setwd(curdir)
  
  ## now work with files in `dir`
  svgpath <- file.path(dir, "inzightplot.svg")
  
  
}
