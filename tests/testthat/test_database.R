context("Database connection")

library(RSQLite)
library(dplyr)
library(dbplyr)
nhanes <- read.csv("nhanes.csv")
con <- dbConnect(SQLite(), "nhanes.db")
dbWriteTable(con, "nhanes", nhanes)
unlink("nhanes")


test_that("Database connection can be graphed", {
    inzplot(~Height, con = con, data = "nhanes")

})
