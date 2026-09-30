# Compile demo/plots.R into the frontend schema and the Rserve launcher.
#
#   Rscript build.R
#
# Writes src/plots.rserve.ts and src/plots.rserve.R.
# Start the server with: Rscript src/plots.rserve.R

demo_dir <- {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg)) {
        dirname(normalizePath(sub("^--file=", "", file_arg[[1L]])))
    } else {
        getwd()
    }
}

setwd(demo_dir)

library(RserveTS)

port <- 6312L
ts_compile(
    "plots.R",
    filename = "src/plots.rserve",
    port = port
)

message("Schema: src/plots.rserve.ts")
message("Rserve: Rscript src/plots.rserve.R  (port ", port, ")")
