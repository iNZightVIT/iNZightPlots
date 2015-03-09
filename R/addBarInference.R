addBarInference <- function(inflist, center, opts) {
    bs <- attr(inflist, "bootstrap")
    col1 <- ifelse(bs, opts$inf.col.comp[2], opts$inf.col.comp[1])
    col2 <- ifelse(bs, opts$inf.col.conf[2], opts$inf.col.conf[1])

    lapply(c("conf", "comp"), function(n) {
        if (!is.null(inflist[[n]])) {
            low <- c(inflist[[n]]$lower)
            upp <- c(inflist[[n]]$upper)
            cis <- c(rbind(low, upp))

            grid.polyline(x = unit(rep(center, each = 2), "native"),
                          y = unit(cis, "native"),
                          id = rep(1:length(center), each = 2),
                          gp =
                          gpar(col = switch(n, 'comp' = col1, 'conf' = col2),
                               lwd = switch(n,
                                   'comp' = opts$inf.lwd.comp / sqrt(nrow(inflist[[n]]$lower)),
                                   'conf' = opts$inf.lwd.conf / sqrt(nrow(inflist[[n]]$lower))
                                   ),
                               lineend = "butt"))
        }
    })
}
