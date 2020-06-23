## code to prepare `plot_types` dataset goes here
plot_types <- read.csv("data-raw/plot_types.csv")
rownames(plot_types) <- plot_types$argument
plot_types$argument <- NULL
plot_types <- as.matrix(plot_types)
usethis::use_data(plot_types, internal = TRUE, overwrite = TRUE)
