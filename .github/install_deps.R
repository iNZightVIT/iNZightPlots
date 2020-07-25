# R script
github_deps <- c(
    "iNZightVIT/iNZightTools@1.9",
    "iNZightVIT/iNZightMR@2.2.5"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))

if (.Platform$OS.type == "windows") {
    utils::install.packages("XML")
    if (!requireNamespace("XML")) {
        download.file(
            sprintf(
                "https://cran.r-project.org/bin/windows/contrib/%s/XML_3.99-0.3.zip",
                paste(strsplit(as.character(getRversion()), "\\.")[[1]][1:2], collapse = ".")
            ),
            "xml.zip"
        )
        unzip("xml.zip", exdir = .libPaths()[1])
        unlink("xml.zip")
    }
}

remotes::install_github(github_deps)
remotes::install_deps(dependencies = TRUE)
remotes::install_cran("rcmdcheck")
