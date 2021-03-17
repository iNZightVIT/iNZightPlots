context("Database connection")

library(RSQLite)
library(dplyr)
library(dbplyr)

nhanes <- read.csv("nhanes.csv")
db <- tempfile(fileext = ".db")
on.exit(dbDisconnect(con) && unlink(db))
con <- dbConnect(SQLite(), db)
dbWriteTable(con, "nhanes", nhanes)
rm("nhanes")

test_that("Database connection can be graphed", {
    inzplot(~Height, con = con, data = "nhanes")

})
