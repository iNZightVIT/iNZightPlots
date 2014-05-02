inzStructure <- function(type, vars, data = NULL) {
  # Creates an object of class `inz.structure`, which accompanies a
  # dataset, and contains information of the data structure.

    # grab the arguments and the data frame is supplied:
    m <- match.call(expand.dots = FALSE)
    env <- parent.frame()
    md <- eval(m$data, env)
    
    if (type %in% c("freq", "frequency")) {
        if (length(m$vars) != 1L) {
            stop("Frequency data structure requires 1 variable.")
        }

        fr <- eval(m$vars, md, env)
        if (any(fr %% 1 > 0))
            stop(paste0("Frequencies should be integers. Use `as.integer` to\n",
                        "coerce non-integer values before calling `inzStructure`."))

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
