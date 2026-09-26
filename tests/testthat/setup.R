# iNZightTools metadata CSVs (e.g. chis2) need '#' comments skipped by readr.
# Default option is NULL; without this, smart_read treats the meta header as data.
options(inzighttools.comment = "#")
