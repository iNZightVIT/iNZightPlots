#' iNZightPlot Formula Interface
#'
#' @param f A formula in the form of \code{y ~ x | g}. See Details.
#' @param data Dataset to plot
#' @param ... Any arguments to pass to \code{\link{iNZightPlot}}
#' 
#' @details
#' \code{iNZPlot} is a simple wrapper around the \code{\link{iNZightPlot}} function. 
#' 
#' There are four options for the formula passed in:
#' 
#' \code{y} will produce a plot of the single variable \code{y}.
#' 
#' \code{y ~ x} will produce a plot of \code{y} against \code{x}.
#' 
#' \code{y ~ x | g1} will produce a plot of \code{y} against \code{x} subset by \code{g1}.
#' 
#' \code{y ~ x | g1 + g2} will produce a plot of \code{y} against \code{x} subset by \code{g1} and \code{g2}.
#' 
#' @return An \code{inzightplotoutput} object, which contains the information
#'         displayed in the plot
#' @export
#' 
#' @seealso iNZightPlot
#'
#' @examples
#' data("CO2")
#' iNZPlot(uptake, data = CO2)
#' iNZPlot(uptake ~ Treatment, data = CO2)
#' iNZPlot(uptake ~ Treatment | Type, data = CO2)
#' iNZPlot(uptake ~ Treatment | Type, 
#' data = CO2, g1.level = "Quebec")
iNZPlot <- function(f, data = NULL, ...) {
    f <- match.call()[["f"]] 
    if (!rlang::is_formula(f)) {
        eval(rlang::expr(iNZightPlot(x = !!f,  data = !!match.call()[["data"]], ...)))
    } else {
        f.list <- as.list(f)
        
        if (lengths(f.list)[3] == 1) {
            eval(rlang::expr(iNZightPlot(x = !!f.list[[3]], y = !!f.list[[2]], data = !!match.call()[["data"]], ...)))
        } else {
            f.list2 <- as.list(f.list[[3]])
            if (lengths(f.list2)[3] == 1) {
                eval(rlang::expr(iNZightPlot(x = !!f.list2[[2]], y = !!f.list[[2]], g1 = !!f.list2[[3]], data = !!match.call()[["data"]], ...)))
            } else {
                f.list3 <- as.list(f.list2[[3]])
                eval(rlang::expr(iNZightPlot(x = !!f.list2[[2]], y = !!f.list[[2]], g1 = !!f.list3[[2]], g2 = !!f.list3[[3]], data = !!match.call()[["data"]], ...)))
            }
        }
    }
}
