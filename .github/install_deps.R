# R script
github_deps <- c(
    "iNZightVIT/iNZightTools@1.13.0",
    "iNZightVIT/iNZightMR@2.2.7"
)

OS <- Sys.getenv("OS_TYPE")
options(
    repos = c(
        if (OS == "Linux") RSPM <- Sys.getenv("RSPM"),
        CRAN = "https://cloud.r-project.org"
    ),
    install.packages.compile.from.source = "never"
)

if (OS != "Linux" && !requireNamespace("XML", quietly = TRUE)) {
    install.packages("XML", type = "binary")
}

# install estimability package version 1.4.1, if not already installed
if (getRversion() < numeric_version(4.3) &&
    !requireNamespace("estimability", quietly = TRUE)) {
    remotes::install_version("estimability", version = "1.4.1")
}

remotes::install_github(github_deps)
remotes::install_deps(dependencies = TRUE)
remotes::install_cran("rcmdcheck")
