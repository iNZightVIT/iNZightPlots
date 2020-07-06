# R script
github_deps <- c(
    "iNZightVIT/iNZightTools@dev"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))

if (.Platform$OS.type == "windows" && getRversion() < numeric_version("4")) {
    download.file(
        "https://cran.r-project.org/bin/windows/contrib/3.6/XML_3.99-0.3.zip",
        "xml.zip"
    )
    unzip("xml.zip", .libPaths())
    unlink("xml.zip")
}

remotes::install_github(github_deps)
remotes::install_deps(dependencies = TRUE)
remotes::install_cran("rcmdcheck")
