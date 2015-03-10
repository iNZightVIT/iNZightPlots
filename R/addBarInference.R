addBarInference <- function(inflist, center, opts) {
    bs <- attr(inflist, "bootstrap")
    col1 <- ifelse(bs, opts$inf.col.comp[2], opts$inf.col.comp[1])
    col2 <- ifelse(bs, opts$inf.col.conf[2], opts$inf.col.conf[1])

    ## Add vertical lines to bars
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

addBarCompLines <- function(comp, bounds, phat, opts) {

    ## Add horizontal comparison lines
    
    n1 <- ncol(phat)
    n2 <- nrow(phat)
    
    if (n2 == 1) {
        x <- rep(range(bounds), n1 * 2)
    } else {
        x <- c(apply(apply(matrix(bounds, nrow = 2 * n2), 2, range),
                     2, rep, times = n2 * 2))
    }
    
    y <- rep(c(do.call(rbind, comp)), each = 2)
    id <- rep(1:(n2 * n1 * 2), each = 2)
    
    grid.polyline(x = unit(x, "native"), y = unit(y, "native"),
                  id = id, gp = gpar(lty = 3, col = "grey50", lwd = 0.5))
}
