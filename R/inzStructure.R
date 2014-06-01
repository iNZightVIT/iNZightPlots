inzStructure <- function(arglist = list(id = 1:nrow(data)),
                         data = NULL, force.to.int = FALSE) {
  # Creates an object of class `inz.structure`, which accompanies a
  # dataset, and contains information of the data structure.

  # arglist: a list containing any of `freq`, OR `ids`, `probs`, `strata`, `fpc`, `weights`

    # grab the arguments and the data frame if supplied:
    m <- match.call(expand.dots = FALSE)
    env <- parent.frame()
    md <- eval(m$data, env)

    if ("freq" %in% names(m$arglist)) {
        type = "frequency"
        fr <- eval(m$arglist$freq, md, env)
        
        if (any(!is.finite(fr))) {
            fr[!is.finite(fr)] <- 0
            warning("Missing frequencies treated as 0")
        }

        if (any(fr %% 1 > 0)) {
            if (force.to.int)
                fr <- as.integer(fr)
            else
                stop(paste0("Frequencies should be integers. Use `force.to.int = TRUE` to force ",
                            "values to integers."))
        }

        fr.name <- getName(deparse(m$arglist$freq))
        
        st <- list(type = type,
                   freqs = fr,
                   varname = eval(fr.name))
        
    } else {
        type = "survey"
        stop("Survey design not yet implemented for iNZightPlots. Try using `freq` = `weights` for now.")
    }

    class(st) <- "inz.structure"
    st
}

getName <- function(x) {           
    if (grepl("\\$", x)) strsplit(x, "\\$")[[1]][2] else x
}
