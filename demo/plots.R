library(RserveTS)

find_pkg_root <- function() {
    env <- Sys.getenv("INZIGHT_PLOTS_PATH", unset = "")
    if (nzchar(env)) {
        return(normalizePath(env, mustWork = TRUE))
    }

    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    starts <- getwd()
    if (length(file_arg)) {
        starts <- c(
            dirname(normalizePath(sub("^--file=", "", file_arg[[1L]]))),
            starts
        )
    }

    for (start in unique(starts)) {
        dir <- normalizePath(start, mustWork = FALSE)
        for (i in 0:4) {
            if (file.exists(file.path(dir, "DESCRIPTION")) &&
                file.exists(file.path(dir, "R", "as_plot.R"))) {
                return(dir)
            }
            parent <- dirname(dir)
            if (identical(parent, dir)) break
            dir <- parent
        }
    }

    stop("Could not find iNZightPlots. Set INZIGHT_PLOTS_PATH.")
}

pkgload::load_all(find_pkg_root(), quiet = TRUE)

cas <- iNZightMR::census.at.school.5000

## Keep a one-row data-frame column as a JSON array. Factors become strings.
preserve_columns <- function(x) {
    if (is.data.frame(x)) {
        x[] <- lapply(x, function(col) {
            if (is.factor(col)) col <- as.character(col)
            I(col)
        })
        return(x)
    }
    if (is.list(x)) return(lapply(x, preserve_columns))
    x
}

structure_json <- function(obj) {
    json <- jsonlite::toJSON(
        preserve_columns(as_plot(obj)),
        auto_unbox = TRUE,
        digits = NA,
        dataframe = "columns"
    )
    trimws(paste(as.character(json), collapse = ""))
}

plot_example <- ts_list(
    id = ts_character(1L),
    title = ts_character(1L),
    structureJson = ts_character(1L)
)

examples <- list(
    list(
        id = "bar",
        title = "Bar: travel",
        plot = iNZightPlot(travel, data = cas)
    ),
    list(
        id = "bar-two-way",
        title = "Bar: travel by gender",
        plot = iNZightPlot(travel, gender, data = cas)
    ),
    list(
        id = "bar-segmented",
        title = "Bar: travel stacked by gender",
        plot = iNZightPlot(travel, colby = gender, data = cas)
    ),
    list(
        id = "dot",
        title = "Dot: height by gender",
        plot = iNZightPlot(height, gender, data = cas)
    ),
    list(
        id = "hist",
        title = "Histogram: height",
        plot = iNZightPlot(height, data = cas, plottype = "hist")
    ),
    list(
        id = "scatter",
        title = "Scatter: height and armspan",
        plot = iNZightPlot(height, armspan, colby = gender, data = cas)
    ),
    list(
        id = "scatter-facet",
        title = "Scatter: height and armspan by gender",
        plot = iNZightPlot(height, armspan, g1 = gender, data = cas)
    ),
    list(
        id = "hex",
        title = "Hex: height and armspan",
        plot = iNZightPlot(height, armspan, data = cas, plottype = "hex")
    ),
    list(
        id = "grid",
        title = "Grid: height and armspan",
        plot = suppressWarnings(
            iNZightPlot(height, armspan, data = cas, plottype = "grid")
        )
    )
)

examples <- lapply(examples, function(example) {
    list(
        id = example$id,
        title = example$title,
        structureJson = structure_json(example$plot)
    )
})

plots <- ts_function(
    function() examples,
    result = ts_list(plot_example),
    export = TRUE
)
