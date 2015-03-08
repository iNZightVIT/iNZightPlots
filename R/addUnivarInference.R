addUnivarInference <- function(inflist, i) {
    bs <- attr(inflist, "bootstrap")
    col1 <- ifelse(bs, "green4", "blue")
    col2 <- ifelse(bs, "magenta", "red")
    lapply(c("conf", "comp"), function(n) {
        if (!is.null(inflist[[n]])) {
            ci <- inflist[[n]][i, ]
            grid.lines(x = unit(ci, units = "native"),
                       y = unit(0.5, "npc"),
                       gp =
                       gpar(col = switch(n, 'comp' = col1, 'conf' = col2),
                            lwd = switch(n, 'comp' = 4, conf = 2),
                            lineend = "butt"))
        }
    })
}

addUnivarCompLines <- function(inflist) {
    guides <- unique(c(inflist$comp))
    if (!is.null(guides))
        grid.polyline(x = unit(rep(guides, each = 2), "native"),
                      y = rep(c(0, 1), length(guides)),
                      id = rep(1:length(guides), each = 2),
                      gp = gpar(lty = 3, col = "grey50", lwd = 0.5))
}
