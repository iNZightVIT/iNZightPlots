# R script
github_deps <- c(
    "iNZightVIT/iNZightTools@feature/guinz",
    "iNZightVIT/iNZightMR@2.2.5"
)

OS <- Sys.getenv("OS_TYPE")
options(
    repos = c(
        if (OS == "Linux") RSPM = Sys.getenv("RSPM"),
        CRAN = "https://cloud.r-project.org"
    ),
    install.packages.compile.from.source = "never"
)

if (OS != "Linux" && !requireNamespace("XML", quietly = TRUE))
    install.packages("XML", type = "binary")

remotes::install_github(github_deps)
remotes::install_deps(dependencies = TRUE)
remotes::install_cran("rcmdcheck")
