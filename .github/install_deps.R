# R script
github_deps <- c(
    "iNZightVIT/iNZightTools@1.9",
    "iNZightVIT/iNZightMR@2.2.5"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))
if (.Platform$OS.type == "windows") {
    utils::install.packages("XML", type = "binary")
}

remotes::install_github(github_deps)
remotes::install_deps(dependencies = TRUE)
remotes::install_cran("rcmdcheck")
