

inzStructure <- function(type, vars, data = NULL, force.to.int = FALSE) {
  # Creates an object of class `inz.structure`, which accompanies a
  # dataset, and contains information of the data structure.

    # grab the arguments and the data frame if supplied:
    m <- match.call(expand.dots = FALSE)
    env <- parent.frame()
    md <- eval(m$data, env)
    
    if (type %in% c("freq", "frequency")) {
        if (length(m$vars) != 1L) {
            stop("Frequency data structure requires 1 variable.")
        }

        fr <- eval(m$vars, md, env)
        
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

        fr.name <- getName(deparse(substitute(vars)))
        
        st <- list(type = type,
                   freqs = fr,
                   varname = eval(fr.name))
        
    } else if (type %in% c("survey")) {
        stop("Survey structure not yet implemented.")
    }

    class(st) <- "inz.structure"
    st
}

getName <- function(x) {           
    if (grepl("\\$", x)) strsplit(x, "\\$")[[1]][2] else x
}
