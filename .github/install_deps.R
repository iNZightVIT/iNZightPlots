# R script
github_deps <- c(
    "iNZightVIT/iNZightTools@dev"
)

options(repos = c(CRAN = "https://cloud.r-project.org"))

remotes::install_github(github_deps)

op <- options()

if (.Platform$OS.type == "windows")
    op <- options(install.packages.compile.from.source = "never")

remotes::install_deps(dependencies = TRUE)

options(op)
remotes::install_cran("rcmdcheck")
