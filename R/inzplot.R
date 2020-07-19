#' iNZight Plot Method
#'
#' A generic function used to plot objects within the iNZight ecosystem.
#' @param x An object
#' @param ... additional arguments for methods
#' @param ... an environment to evaluate things
#' @return The output depends on the type of input, but is usually called for
#'         the side-effect of producing a plot.
#' @export
inzplot <- function(x, ..., env = parent.frame())
    UseMethod("inzplot")

inzplot.default <- function(x, ..., env = parent.frame())
    stop("That type of object is not supported.")

#' iNZight Plot
#'
#' @param x A formula in the form of \code{y ~ x | g}. See Details.
#' @param data Dataset to plotq
#' @param design A survey design to use
#' @param ... Any arguments to pass to \code{\link{iNZightPlot}}
#' @param env the parent environment to pass to the plot function
#'
#' @rdname inzplot
#' @details
#' \code{inzplot} is a simple wrapper around the \code{\link{iNZightPlot}} function.
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
#' inzplot(uptake, data = CO2)
#' inzplot(uptake ~ Treatment, data = CO2)
#' inzplot(uptake ~ Treatment | Type, data = CO2)
#' inzplot(uptake ~ Treatment | Type,
#' data = CO2, g1.level = "Quebec")
inzplot.formula <- function(x, data = NULL, design = NULL, ..., env = parent.frame()) {
    dots <- rlang::enexprs(...)
    f.list <- as.list(x)

    if (length(f.list) == 2) {
        ## formula: ~ x
        eval(
            rlang::expr(
                iNZightPlot(x = !!f.list[[2]],
                    data = !!match.call()[["data"]],
                    design = !!match.call()[["design"]],
                    !!!dots,
                    env = !!env
                )
            ),
            envir = env
        )
    } else if (lengths(f.list)[3] == 1) {
        ## formula: y ~ x
        if (f.list[[3]] == ".") {
            f.list[[3]] <- f.list[[2]]
            f.list[2] <- list(NULL)
        } else {
            varx <- data[[f.list[[3]]]]
            vary <- data[[f.list[[2]]]]
            if ((is_cat(varx) || is_cat(vary))) {
                f.list <- f.list[c(1, 3:2)]
            }
        }
        eval(
            rlang::expr(
                iNZightPlot(x = !!f.list[[3]], y = !!f.list[[2]],
                    data = !!match.call()[["data"]],
                    design = !!match.call()[["design"]],
                    !!!dots,
                    env = !!env
                )
            ),
            envir = env
        )
    } else {
        ## formula: y ~ x | g1 (+ g2)
        f.list2 <- as.list(f.list[[3]])
        if (f.list2[[2]] == ".") {
            f.list2[[2]] <- f.list[[2]]
            f.list[2] <- list(NULL)
        } else {
            varx <- data[[f.list2[[2]]]]
            vary <- data[[f.list[[2]]]]

            if ((is_cat(varx) || is_cat(vary))) {
                f.list2[[2]] <- f.list[[2]]
                f.list[[2]] <- f.list[[3]][[2]]
            }
        }
        if (lengths(f.list2)[3] == 1) {
            eval(
                rlang::expr(
                    iNZightPlot(
                        x = !!f.list2[[2]],
                        y = !!f.list[[2]],
                        g1 = !!f.list2[[3]],
                        data = !!match.call()[["data"]],
                        design = !!match.call()[["design"]],
                        !!!dots,
                        env = !!env
                    )
                ),
                envir = env
            )
        } else {
            f.list3 <- as.list(f.list2[[3]])
            eval(
                rlang::expr(
                    iNZightPlot(
                        x = !!f.list2[[2]],
                        y = !!f.list[[2]],
                        g1 = !!f.list3[[2]],
                        g2 = !!f.list3[[3]],
                        data = !!match.call()[["data"]],
                        design = !!match.call()[["design"]],
                        !!!dots,
                        env = !!env
                    )
                ),
                envir = env
            )
        }
    }
}
