inzpar <- function(...) {
    dots <- list(...)
    
    ip <- list(pch            = 1,
               col.pt         = "grey50",
               col.missing    = "#cccccc",
               cex            = 1,
               cex.pt         = 0.8,
               cex.dotpt      = 0.5,
               cex.lab        = 1,
               cex.axis       = 0.8,
               cex.main       = 1.2,
               cex.text       = 1,
               alpha          = 1,
               bg             = "white",
               fill.pt        = "transparent",
               lwd            = 1,
               lty            = 1,
               lwd.pt         = 2,
               col.line       = "blue",
               col.sub        = "wheat",
               jitter         = "",
               rugs           = "",
               trend          = NULL,
               smooth         = 0,
               smoothby.lty   = 4,  # MUST be numeric
               quant.smooth   = NULL,
               LOE            = FALSE,
               join           = FALSE,
               lines.by       = TRUE,
               col.trend =
               list(linear = "blue",
                    quadratic = "red",
                    cubic ="green4"),
               trend.by       = FALSE,
               trend.parallel = TRUE,
               col.smooth     = c("magenta"),
               col.LOE        = "black",
               lty.LOE        = 2,
               boxplot        = TRUE,
               box.lwd        = c(2, 0.7),
               box.col        = "black",
               box.fill       = "grey90",
               bar.lwd        = 1,
               bar.col        = "black",
               bar.fill       = "darkgreen",
               full.height    = FALSE,
               inf.lwd.comp   = 4,
               inf.lwd.conf   = 2,
               inf.col.comp   = "black",
               inf.col.conf   = "red",
               inference.type = NULL,
               inference.par  = NULL,
               bs.inference   = FALSE,
               n.boot         = 1500,
               large.sample.size = 2000,
               largesample    = NULL,
               scatter.grid.bins = 200,
               hex.bins       = NULL,
               hist.bins      = 100,
               quant.cutoff   = c(200, 1000),
               plottype       = "default",
               matchplots     = FALSE)

    # update any user has specified
    if (length(dots) > 0) {
        for (i in names(dots))
            ip[[i]] <- dots[[i]]
    }

    class(ip) <- "inzpar.list"
    ip
}
