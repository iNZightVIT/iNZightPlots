getPlotSummary <- function(x, y = NULL, g1 = NULL, g2 = NULL,
                           g1.level = NULL, g2.level = NULL,
                           varnames = list(),
                           ...) {
  # Returns summary information relating to the plot.
  # Based on addSumm from chrisfns.R

    opts <- modifyList(inzPlotDefaults(), list(...))

  # --------------------------------------------------------------------------- #
  #                                              set up required variable names

  # For variables which are supplied, but have no varname supplied, just use
  # the name of the variable

    getName <- function(name) {
        if (grepl("\\$", name))
            name <- strsplit(name, "\\$")[[1]][2]
        name
    }
    
    if (is.null(varnames$x))
        varnames$x <- getName(deparse(substitute(x)))
    if (!is.null(y) & is.null(varnames$y))
        varnames$y <- getName(deparse(substitute(y)))
    if (!is.null(g1) & is.null(varnames$g1))
        varnames$g1 <- getName(deparse(substitute(g1)))
    if (!is.null(g2) & is.null(varnames$g2))
        varnames$g2 <- getName(deparse(substitute(g2)))
    
    # This is a temporary fix for a "Vertical Dot Plot"
    # 
    if (is.factor(x) & is.numeric(y)) {
        x.tmp <- x
        x <- y
        y <- x.tmp
        x.name <- varnames$x
        varnames$x <- varnames$y
        varnames$y <- x.name
    }

    # --------------------------------------------------------------------------- #
    #                                                       Remove missing values


    na <- is.na(x)
    if (!is.null(y))
        na <- na | is.na(y)
    if (!is.null(g1))
        na <- na | is.na(g1)
    if (!is.null(g2))
        na <- na | is.na(g2)

  # Before we remove any missing values, we need to keep track of the original
  # subsetting variables
    g2.o <- g2
    y.o <- if (is.null(g2) & is.null(g2.level)) y else subset(y, g2 == g2.level)
    g1.o <- if (is.null(g2) & is.null(g2.level)) g1 else subset(g1, g2 == g2.level)
    missing <- if (is.null(g2) & is.null(g2.level)) na else subset(na, g2 == g2.level)
    
    x <- x[!na]
    if (!is.null(y))  y  <-  y[!na]
    if (!is.null(g1)) g1 <- g1[!na]
    if (!is.null(g2)) g2 <- g2[!na]
  
  # This function returns a character string thingy, called o

    o <- ""  # empty line at the beginning

  # --------------------------------------------------------------------------- #
  #                                                                Subset by g2
   
    if (!is.null(g2) & !is.null(g2.level)) {
      # Check for iNZightCentral value:
        if (g2.level != "_ALL") {
            
          # Only use the observations according to g2.level
            if (is.numeric(g2))
                g2 <- convert.to.factor(g2)
            
            x <- subset(x, g2 == g2.level)
            if (!is.null(y))
                y <- subset(y, g2 == g2.level)
            if (!is.null(g1)) {
                g1 <- subset(g1, g2 == g2.level)
                g1.o <- subset(g1.o, g2.o == g2.level)
            }
            missing <- subset(missing, g2.o == g2.level)
        }

      # ******************************************************************* #
        msg <- paste0("For the subset of the data where ",
                      varnames$g2, " = ", g2.level, ".")
        o <- c(o, msg, paste(rep('-', nchar(msg)), collapse = ''))
    }

  # --------------------------------------------------------------------------- #
  #                                                             Subdivide by g1

    if (!is.null(g1)) {
      # Check for iNZightCentral value
        if (!is.null(g1.level))
            if (g1.level == "_MULTI") g1.level <- NULL
        
      # Necessary to allow continuous variables to be used to subset
        if (!is.factor(g1))
            g1 <- convert.to.factor(g1)
        
      # If level is supplied as a number, convert to text
        if (is.numeric(g1.level)) {
            if (g1.level > length(levels(g1)))
                stop("g1.level must not be greater than the number of levels in g1")
            
            if (as.integer(g1.level) != g1.level) {
                g1.level <- as.integer(g1.level)
                warning(paste0("g1.level truncated to ", g1.level, "."))
            }
                
            g1.level <- if (g1.level == 0) levels(g1) else levels(g1)[g1.level]
            
        }

      # If plotting all levels, or nothing specified, supply all
      # levels of the factor
        if (is.null(g1.level))
            g1.level <- levels(g1)

      # Create a list for all other variables, for each level of g1
        x.list <- lapply(g1.level,
                         function(l) subset(x, g1 == l))
        names(x.list) <- g1.level
            
        if (!is.null(y)) {
            y.list <- lapply(g1.level,
                             function(l) subset(y, g1 == l))
            names(y.list) <- g1.level
        }

      # Still tracking missing values:
        n.missing <- tapply(missing, g1.o, sum)

      # Also need to know the level:
        lev <- g1.level        
    } else {
        x.list <- list(all = x)
        if (!is.null(y))
            y.list <- list(all = y)
        n.missing <- sum(missing)
        lev <- levels(g1)
    }

  # --------------------------------------------------------------------------- #
  #                                      Begin creating the summary information

    if (is.numeric(x)) {
        if (is.null(y)) {
          # We have x on its own. One summary for each level of g1
            for (i in 1:length(x.list)) {
                X <- x.list[[i]]
                M <- if (is.null(g1)) n.missing else n.missing[lev[i]]
                
                sum <- summary(X)
                values <- as.vector(sum)
                values[c(2:3, 5)] <- fivenum(X)[2:4]

                sum.tab <- cbind(t(matrix(values)), signif(sd(X), 5),
                                 length(X), sum(M), deparse.level = 0)
                colnames(sum.tab) <- c(names(sum), "Std.dev", "Sample.Size", "NA's")
                rownames(sum.tab) <- rep("", nrow(sum.tab))
                
                o <- if (i > 1) c(o, paste(rep('_', 80), collapse = ''), '') else c(o, '')
                
                command <- paste0("Summary of ", varnames$x,
                                  ifelse(is.null(g1), ':',
                                         paste0(' for ', varnames$g1, ' = ', lev[i], ':')))
                o <- c(o, command, '')

                out <- capture.output(matprint(sum.tab))
                o <- c(o, eval(out))
            }
        } else if (is.factor(y)) {
          # X is numeric, Y is a factor
            for (i in 1:length(x.list)) {
                X <- x.list[[i]]  # numeric
                Y <- y.list[[i]]  # factor

              # Missing X and Y values:
                M <-
                    if (is.null(g1)) tapply(missing, y.o, sum)
                    else tapply(subset(missing, g1.o == lev[i]),
                                subset(y.o, g1.o == lev[i]), sum)                              

                sum.tab <- t(sapply(split(X, Y), summary))
                sd.col <- sapply(split(X, Y), sd)
                size <- sapply(split(X, Y), length)
                sum.tab <- cbind(sum.tab, Std.dev = signif(sd.col, 3),
                                 Sample.Size = size, "NA's" = M)
                sum.tab[!is.finite(sum.tab)] <- ''

                o <- if (i > 1) c(o, paste(rep('_', 80), collapse = ''), '') else c(o, '')

                command <- paste0("Summary of ", varnames$x, " by ", varnames$y,
                                  ifelse(is.null(g1), ':',
                                         paste0(' for ', varnames$g1, ' = ', lev[i], ':')))
                o <- c(o, command, '')
                out <- capture.output(matprint(sum.tab))
                o <- c(o, eval(out))

              # There was originally some ANOVA output ???
            }
        } else {
          # X and Y are numeric
            for (i in 1:length(x.list)) {
                X <- x.list[[i]]
                Y <- y.list[[i]]

                if (!is.null(opts$trend)) {
                  # Need to add inference information for trend lines:
                    o <- if (i > 1) c(o, paste(rep('_', 80), collapse = ''), '') else c(o, '')
                    if (!is.null(g1))
                        o <- c(o, paste0("Summary for ", varnames$g1, ' = ', lev[i]), '')
                    
                    if ("linear" %in% opts$trend) {
                        o <- c(o, "Linear Trend", '')
                        fit <- lm(Y ~ X)
                        o <- c(o, paste0(varnames$y, ' = ', signif(coef(fit)[2], 5), ' * ',
                                         varnames$x, ' + ', round(coef(fit)[1], 2)),
                               paste0("Correlation = ", signif(cor(X, Y), 5)), '', '')
                    }

                    if ("quadratic" %in% opts$trend) {
                        o <- c(o, "Quadratic Trend", '')
                        fit <- lm(Y ~ X + I(X^2))
                        B <- coef(fit)
                        o <- c(o, paste0(varnames$y, ' = ', signif(B[2], 5), ' * ',
                                         varnames$x, ' + ', signif(B[3], 5), ' * ',
                                         varnames$y, '^2', ' + ', signif(B[1], 5)), '', '')
                               
                    }

                    if ("cubic" %in% opts$trend) {
                        o <- c(o, "Cubic Trend", '')
                        fit <- lm(Y ~ X + I(X^2) + I(X^3))
                        B <- coef(fit)
                        o <- c(o, paste0(varnames$y, ' = ', signif(B[2], 5), ' * ',
                                         varnames$x, ' + ', signif(B[3], 5), ' * ',
                                         varnames$x, '^2', ' + ', signif(B[4], 5), ' * ',
                                         varnames$x, '^3', ' + ', signif(B[1], 5)), '', '')
                    }
                } else {
                  # Need to add inference information

                    o <- c(o, "Add trend lines to the scatter plot in order to get a summary.",
                           "(Use the Add To Plot feature at the bottom of the graphics window)")
                }
            }
        }
    } else {
      # X is a factor
        if (is.null(y)) {
            for (i in 1:length(x.list)) {
                X <- x.list[[i]]
                n <- length(levels(X))
              #  M <- if (is.null(g1)) n.missing else n.missing[lev[i]]
                
                o <- if (i > 1) c(o, paste(rep('_', 80), collapse = ''), '') else c(o, '')
                
              # Table of counts
                if (n > 10)
                    sum.tab <- verticalTable.1(X, pc = FALSE)
                else
                    sum.tab <- t(verticalTable.1(X, pc = FALSE))

                out <- capture.output(matprint(cbind(sum.tab)))#, N.Missing = M)))
                o <- c(o, paste0("Table of counts for ", varnames$x,
                                 ifelse(is.null(g1), ':',
                                        paste0(' for ', varnames$g1, ' = ', lev[i], ':'))),
                       '', out, '')

              # Frequency table
                if (n > 10)
                    sum.tab <- verticalTable.1(X, pc = TRUE)
                else
                    sum.tab <- t(verticalTable.1(X, pc = TRUE))

                out <- capture.output(matprint(sum.tab))
                o <- c(o, paste0("Frequency table for ", varnames$x,
                                ifelse(is.null(g1), ':',
                                       paste0(' for ', varnames$g1, ' = ', lev[i], ':'))),
                       '', out, '')
            }
        } else if (is.factor(y)) {
          # X and Y are factors
            for (i in 1:length(x.list)) {
                X <- x.list[[i]]
                Y <- y.list[[i]]

              #  M <-
              #      if (is.null(g1)) tapply(missing, y.o, sum)
              #      else tapply(subset(missing, g1.o == lev[i]),
              #                  subset(y.o, g1.o == lev[i]), sum)  

                o <- if (i > 1) c(o, paste(rep('_', 80), collapse = ''), '') else c(o, '')

              # Table of counts
                if (length(levels(X)) < 10 & length(levels(Y)) < 10)
                    sum.tab <- verticalTable.2(Y, X, pc = FALSE)
                else
                    sum.tab <- t(verticalTable.2(Y, X, pc = FALSE))

                out <- capture.output(matprint(cbind(sum.tab)))#, N.Missing = M)))
                o <- c(o, paste0("Table of counts for ", varnames$x, " by ", varnames$y,
                                 ifelse(is.null(g1), ':',
                                        paste0(' for ', varnames$g1, ' = ', lev[i], ':'))),
                       '', out, '')

              # Frequency table
                if (length(levels(X)) < 10 & length(levels(Y)) < 10)
                    sum.tab <- verticalTable.2(Y, X, pc = TRUE)
                else
                    sum.tab <- t(verticalTable.2(Y, X, pc = TRUE))

                out <- capture.output(matprint(sum.tab))
                o <- c(o, paste0("Frequency table for ", varnames$x, " by ", varnames$y,
                                 ifelse(is.null(g1), ':',
                                        paste0(' for ', varnames$g1, ' = ', lev[i], ':'))),
                       '', out, '')
            }
        } else {
          # Y is numeric
          # This should not happen (:
        }
    }

    o <- c(o, '\n')
    
    class(o) <- "iNZightPlotSummary"
    o
}

###############################################################################################
# End of getPlotSummary()

print.iNZightPlotSummary <- function(x) {
    cat(paste(x, collapse = "\n"))
}

verticalTable.1 <- function(x, pc = TRUE, digits = 3, ...) {
    counts <- table(x)
    res <- data.frame(Count =
                      format(c(counts, sum(counts)), sci = FALSE),
                      Percent =
                      paste(format(c(signif(100 * counts / sum(counts), digits), 100)),
                            "%", sep = ""),
                      stringsAsFactors = FALSE)

    rownames(res) <- c(names(counts), "Total")
    if (pc)
        as.matrix(res)
    else
        as.matrix(res[, 1, drop = FALSE])
}

verticalTable.2 <- function(x1, x2, pc = TRUE, digits = 3, ...) {
    tbl <- table(x1,x2)
    Rowsums <- rowSums(tbl)
    Colsums <- c(colSums(tbl), sum(colSums(tbl)))

    if (pc) {
        tbl[] <- 100 * sweep(tbl, 1, Rowsums, "/")
        tbl <- cbind(tbl, Total = rowSums(tbl))
        tbl[] <- paste(format(signif(tbl, digits = digits), trim = TRUE, nsmall = digits),
                       "%", sep = "")
        tbl <- cbind(tbl, "Row N" = Rowsums)

        tbl[tbl == "NaN%"] <- ""
    } else {
        tbl <- cbind(tbl, "Row Total" = Rowsums)
        tbl <- rbind(tbl, "Col Total" = Colsums)
    }
    tbl <- tbl[Rowsums > 0, , drop = FALSE]
    as.matrix(tbl) 
}

matprint <- function(x, sep = "   ") {
    rlabs <- !is.null(rownames(x))
    clabs <- !is.null(colnames(x))
    
    l <- matrix("", nrow(x) + clabs, ncol(x) + rlabs)
    if (rlabs)
        l[, 1] <- format(c(if (clabs) "" else character(),
                           rownames(x)), justify = "l")

    for (i in 1:ncol(x)) {
        l[, i + rlabs] <- format(c(if(clabs) colnames(x)[i] else character(),
                                   format(x[, i], justify = "r")), justify = "r")
    }

    for(i in 1:nrow(l))
        cat(l[i,], sep = sep, "\n")
}
