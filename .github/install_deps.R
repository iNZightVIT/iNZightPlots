# R script
github_deps <- c(
    "iNZightVIT/iNZightTools@dev"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))
remotes::install_github(github_deps)
remotes::install_deps(dependencies = TRUE)
remotes::install_cran("rcmdcheck")
