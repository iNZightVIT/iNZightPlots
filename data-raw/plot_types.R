## code to prepare `plot_types` dataset goes here
plot_types <- read.csv("data-raw/plot_types.csv")
usethis::use_data(plot_types, internal = TRUE, overwrite = TRUE)
