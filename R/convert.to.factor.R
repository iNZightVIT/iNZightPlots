convert.to.factor <-
function(x) {
    if (is.factor(x)) {
      # to simplify coding elsewhere, allow convert to factor to simply return
      # the supplied x vector if it is already a factor.
        x.fact <- x
    } else {
    
        ## converts a 
        if (length(unique(x)) < 5)
            x.fact <- factor(x)
        else {  
            x.quantiles <- round((quantile(x, na.rm = TRUE)), 0)  
            x.fact <- try(cut(x, c(-Inf, ifelse(unique(x.quantiles[2:4]) == 3,
                                                x.quantiles[2:4],
                                                unique(x.quantiles[2:4])),
                                   Inf)))
            
            if (inherits(x.fact, "try-error")) {
                eps <- .Machine$double.eps
                x.quantiles <- round((quantile(x, na.rm = TRUE)), 2) + eps * (0:10)
                x.fact <- cut(x, c(-Inf, ifelse(unique(x.quantiles[2:4]) == 3,
                                                x.quantiles[2:4],
                                                unique(x.quantiles[2:4])),
                                   Inf))
            }
            
            if ((x.quantiles[2] == x.quantiles[3]) && (x.quantiles[3] == x.quantiles[4]))
                levels(x.fact) <-
                    c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
                      paste(c("(", x.quantiles[2], " - ", x.quantiles[5], "]"), collapse = ""))
            else if (x.quantiles[2] == x.quantiles[3])
                levels(x.fact) <-
                    c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
                      paste(c("(", x.quantiles[2], " - ", x.quantiles[4], "]"), collapse = ""),
                      paste(c("(", x.quantiles[4], " - ", x.quantiles[5], "]"), collapse = ""))
            else if (x.quantiles[3] == x.quantiles[4])
                levels(x.fact) <-
                    c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
                      paste(c("(", x.quantiles[2], " - ", x.quantiles[3], "]"), collapse = ""),
                      paste(c("(", x.quantiles[3], " - ", x.quantiles[5], "]"), collapse = ""))
            else
                levels(x.fact) <-
                    c(paste(c("[", x.quantiles[1], " - ", x.quantiles[2], "]"), collapse = ""),
                      paste(c("(", x.quantiles[2], " - ", x.quantiles[3], "]"), collapse = ""),
                      paste(c("(", x.quantiles[3], " - ", x.quantiles[4], "]"), collapse = ""),
                      paste(c("(", x.quantiles[4], " - ", x.quantiles[5], "]"), collapse = ""))
        }
    }

  # Remove any empty levels -_-
    factor(x.fact) 
}
