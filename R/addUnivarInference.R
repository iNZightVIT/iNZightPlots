addUnivarInference <- function(inflist, i) {
    lapply(c("conf", "comp"), function(n) {
        if (!is.null(inflist[[n]])) {
            ci <- inflist[[n]][i, ]
            grid.lines(x = unit(ci, units = "native"),
                       y = unit(0.5, "npc"),
                       gp =
                       gpar(col = switch(n, 'comp' = "blue", 'conf' = "red"),
                            lwd = switch(n, 'comp' = 4, conf = 2),
                            lineend = "butt"))
        }
    })
}
