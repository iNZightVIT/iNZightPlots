getPlotInference <- function(x, y = NULL, g1 = NULL, g2 = NULL,
                             g1.level = NULL, g2.level = NULL,
                             varnames = list(),
                             ...) {
  # Returns summary information relating to the plot.
  # Based on addSumm from chrisfns.R

    if (!is.null(g2.level))
        if (g2.level == "_MULTI")
            return("Not yet implemented.")
    
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


    x[is.infinite(x)] <- NA
    if (!is.null(y)) y[is.infinite(y)] <- NA
    
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

  # Returns a character string thingy, called o
    o <- ""  # empty line at the beginning

  # Initial message:
    msg <- paste0("Method used to generate inference: ",
                  ifelse(opts$bs.inference, "Bootstrap theory.", "Normal theory."))
    o <- c(o, msg, '')

  # --------------------------------------------------------------------------- #
  #                                                                Subset by g2

    if (!is.null(g2)) { if (is.numeric(g2)) g2 <- convert.to.factor(g2) }
    if (!is.null(g1)) {
        if (is.numeric(g1)) g1 <- convert.to.factor(g1)
    } else {
      # g1 is null! Check that g2 is not set to _MULTI:
        if (!is.null(g2.level)) {
            if (g2.level == "_MULTI") {
                v <- varnames
                v$g1 <- varnames$g2
                v$g2 <- varnames$g1
                g1.tmp <- g2
                g2 <- g1
                g1 <- g1.tmp
            }
        }
    }
    
    if (!is.null(g2) & !is.null(g2.level)) {
        if (is.numeric(g2.level)) {
            if (g2.level > length(levels(g2)))
                stop("g2.level must not be greater than the number of levels in g2")

            if (as.integer(g2.level) != g2.level) {
                g2.level <- as.integer(g2.level)
                warning(paste0("g2.level truncated to ", g2.level, "."))
            }
            
            g2.level <- if (g2.level == 0) "_ALL" else levels(g2)[g2.level]
            
        }
        
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
        if (!is.null(g2.level)) {
            msg <- paste0("For the subset of the data where ",
                          varnames$g2, " = ", g2.level, ".")
            o <- c(o, msg, paste(rep('-', nchar(msg)), collapse = ''))
        }
    }

    o <- c(o, paste(rep('_', 80), collapse = ''), '')

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
    n.missing[is.na(n.missing)] <- 0

  # --------------------------------------------------------------------------- #
  #                                      Begin creating the summary information

    N <- length(x.list)
    
    if (is.numeric(x)) {
        if (is.null(y)) {
          # X is numeric.
            for (i in 1:N) {
                o <- if (i > 1) c(o, paste(rep('_', 80), collapse = ''), '') else c(o)

              # Some summary information
                X   <- x.list[[i]]
                n   <- length(X)
                mu  <- signif(mean(X), 5)
                med <- signif(median(X), 5)
                s5  <- fivenum(X)
                iqr <- signif(s5[4] - s5[2], 5)

                o <- c(o, paste0(varnames$x,
                                 ifelse(is.null(g1), '',
                                        paste0(" for ", varnames$g1, ' = ', lev[i]))), '')

                if (opts$bs.inference) {
                  # Bootstrap inference
                    if (n > 10) {
                        b <- boot(X, function(x, d)
                                  c(means = mean(x[d]),
                                    medians = median(x[d]),
                                    iqrs = fivenum(x[d])[4] - fivenum(x[d])[2]),
                                  R = 1500)
                        ci.mean <- boot.ci(b, type = "perc", index = 1)
                        ci.med  <- boot.ci(b, type = "perc", index = 2)
                        ci.iqr  <- boot.ci(b, type = "perc", index = 3)

                        ci.l.mean <- signif(ci.mean$percent[1, 4], 5)
                        ci.u.mean <- signif(ci.mean$percent[1, 5], 5)
                        ci.l.med  <- signif(ci.med$percent[1, 4], 5)
                        ci.u.med  <- signif(ci.med$percent[1, 5], 5)
                        ci.l.iqr  <- signif(ci.iqr$percent[1, 4], 5)
                        ci.u.iqr  <- signif(ci.iqr$percent[1, 5], 5)

                        o <- c(o, 'Group Means with percentile Bootstrap Confidence Intervals')
                        
                        inf.df <- data.frame(ci.lower = ci.l.mean, estimate = mu,
                                             ci.upper = ci.u.mean)
                        inf.mat <- capture.output(matprint(as.matrix(inf.df)))
                        o <- c(o, eval(inf.mat), '')

                        o <- c(o, paste0('Group Medians with percentile ',
                                         'Bootstrap Confidence Intervals'))

                        inf.df <- data.frame(ci.lower = ci.l.med, estimate = med,
                                             ci.upper = ci.u.med)
                        inf.mat <- capture.output(matprint(as.matrix(inf.df)))
                        o <- c(o, eval(inf.mat), '')

                        o <- c(o, paste0('Group Inter-Quartile-Ranges with percentile ',
                                         'Bootstrap Confidence Intervals'))

                        inf.df <- data.frame(ci.lower = ci.l.iqr, estimate = iqr,
                                             ci.upper = ci.u.iqr)
                        inf.mat <- capture.output(matprint(as.matrix(inf.df)))
                        o <- c(o, eval(inf.mat))
                    } else {
                        o <- c(o, paste0("No inference output ",
                                         "(number of observations too small for ",
                                         "bootstrap inference.)"))
                    }
                } else {
                  # Normal inference
                    if (n > 1) {
                        o <- c(o, 'Group means with 95% Confidence Intervals')

                        err <- signif(qt(0.975, df = n - 1) * (sd(X) / sqrt(n)), 5)
                        inf.df <- data.frame(ci.lower = mu - err, estimate = mu,
                                             ci.upper = mu + err)
                        inf.mat <- capture.output(matprint(as.matrix(inf.df)))
                        o <- c(o, eval(inf.mat))
                    } else {
                        o <- c(o, "No inference output (not enough observations!)")
                    }
                }

                o <- c(o, "", paste0("Inference based on ", n, " observation",
                                     ifelse(n > 1, "s", ""), "."))
                M <- if (is.null(g1)) n.missing else n.missing[lev[i]]
                if (M > 0) {
                    o <- c(o, paste0("(", M, " observation", ifelse(M > 1, "s ", " "),
                                     "removed due to missing values)"))
                }
            }
        } else if (is.factor(y)) {
          # X is numeric, Y is a factor
            for (i in 1:N) {
                o <- if (i > 1) c(o, paste(rep('_', 80), collapse = ''), '') else c(o)

                o <- c(o, paste0(varnames$x, " by ", varnames$y,
                                 ifelse(is.null(g1), '',
                                        paste0(" for ", varnames$g1, ' = ', lev[i]))), '')

                X <- x.list[[i]]
                Y <- y.list[[i]]

                if (opts$bs.inference) {
                  # Bootstrap inference
                    names <- levels(Y)
                    inf.tab1 <- matrix(".", nrow = length(names), ncol = 3,
                                       dimnames =
                                       list(names, c("ci.lower", "estimate", "ci.upper")))
                    inf.tab2 <- inf.tab3 <- inf.tab1

                    for (j in 1:length(names)) {
                        x.subset <- x[Y == names[j]]
                        n <- length(x.subset)
                        mu <- signif(mean(x.subset), 5)
                        med <- signif(median(x.subset, 5))
                        s5 <- fivenum(x.subset)
                        iqr <- signif(s5[4] - s5[2], 5)

                        if (n > 10) {
                            b <- boot(x.subset, function(x, d)
                                      c(means = mean(x[d]),
                                        medians = median(x[d]),
                                        iqrs = fivenum(x[d])[4] - fivenum(x[d])[2]),
                                      R = 1500)
                            ci.mean <- boot.ci(b, type = "perc", index = 1)
                            ci.med  <- boot.ci(b, type = "perc", index = 2)
                            ci.iqr  <- boot.ci(b, type = "perc", index = 3)
                            
                            ci.l.mean <- signif(ci.mean$percent[1, 4], 5)
                            ci.u.mean <- signif(ci.mean$percent[1, 5], 5)
                            ci.l.med  <- signif(ci.med$percent[1, 4], 5)
                            ci.u.med  <- signif(ci.med$percent[1, 5], 5)
                            ci.l.iqr  <- signif(ci.iqr$percent[1, 4], 5)
                            ci.u.iqr  <- signif(ci.iqr$percent[1, 5], 5)

                            inf.tab1[j, ] <- c(ci.l.mean, mu, ci.u.mean)
                            inf.tab2[j, ] <- c(ci.l.med, med, ci.u.med)
                            inf.tab3[j, ] <- c(ci.l.iqr, iqr, ci.u.iqr)
                        } else {
                            inf.tab1[j, ] <- c("-", ifelse(!is.finite(mu), "-", mu), "-")
                            inf.tab2[j, ] <- c("-", ifelse(!is.finite(med), "-", med), "-")
                            inf.tab3[j, ] <- c("-", ifelse(!is.finite(iqr), "-", iqr), "-")
                        }
                    }
                    
                    inf.mat <- capture.output(matprint(inf.tab1))
                    o <- c(o, "Group Means with Percentile Bootstrap Confidence Intervals",
                           eval(inf.mat))

                    inf.mat <- capture.output(matprint(inf.tab2))
                    o <- c(o, '', paste("Group Medians with Percentile",
                                    "Bootstrap Confidence Intervals"),
                           eval(inf.mat))

                    inf.mat <- capture.output(matprint(inf.tab3))
                    o <- c(o, '', paste("Group Inter-Quartile-Ranges with",
                                    "Percentile Bootstrap Confidence Intervals"),
                           eval(inf.mat), '')
                } else {
                  # Normal Inference
                    fit <- try(lm(X ~ Y))
                    if (inherits(fit, "try-error")) {
                        o <- c(o, "Need more than 1 group OR not enough observations.")
                    } else {
                        names <- levels(Y)

                        if (length(names) == 2) {
                          # Welsh test?
                        }

                        inf.tab <- matrix(".", nrow = length(names), ncol = 3,
                                          dimnames =
                                          list(names, c("ci.lower", "estimate",
                                                        "ci.upper")))

                        for (j in 1:length(names)) {
                            x.subset <- X[Y == names[j]]
                            n <- length(x.subset)

                            if (n > 1) {
                                mu <- signif(mean(x.subset), 5)
                                err <- signif(qt(0.975, df = n - 1) *
                                              (sd(x.subset) / sqrt(n)), 5)
                                inf.tab[j, ] <- c(mu - err, mu, mu + err)
                            } else {
                                mu <- signif(mean(x.subset), 5)
                                inf.tab[j, ] <- c("-", ifelse(!is.finite(mu), "-", mu), "-")
                            }
                        }

                        out <- capture.output(matprint(inf.tab))
                        o <- c(o, "Group Means with 95% Confidence Intervals",
                               eval(out))

                        mult.tab <- try(multipleComp(fit))
                        
                        
                        if (!inherits(mult.tab, "try-error")) {
                            if (nrow(as.matrix(mult.tab)) == 1) {
                                out <- capture.output(eval(mult.tab))
                                o <- c(o, '', out)
                            } else {
                                if (length(levels(Y)) > 1) {
                                  # Begin F-stat calculations

                                    f.info <- calcF(X, as.character(Y))

                                    o <- c(o, '', "Overall F-test")
                                    fstat <- paste("F:", signif(f.info$fstat, 5))
                                    dfs <- paste("df:", f.info$dfb, ',', f.info$dfw)
                                    pval <- paste("p-value", format.pval(f.info$p))

                                    o <- c(o, paste(fstat, dfs, pval, sep = '  '), '')
                                  # End F-stat

                                    o <- c(o,
                                           "Differences between Group Means (col - row)",
                                           "-------------------------------------------", '')

                                  # the suppressWarnings() is used otherwise R complains
                                  # about coercing strings to NAs but this is what
                                  # we want it to do.
                                    mat <- triangularMatrix(levels(Y), mult.tab, "estimates")
                                    mat[is.na(suppressWarnings(as.numeric(mat))) &
                                        !upper.tri(mat)] <- '-'
                                    out <- capture.output(matprint(mat))
                                    o <- c(o, "Estimates", out)

                                    mat <- triangularMatrix(levels(Y), mult.tab, "ci")
                                    mat[is.na(suppressWarnings(as.numeric(mat)))] <- ''
                                    out <- capture.output(matprint(mat))
                                    o <- c(o, '', paste("95% Confidence Intervals",
                                                        "(Adjusted for multiple comparison)"),
                                           out)
                                    
                                    mat <- triangularMatrix(levels(Y), mult.tab, "p-values")
                                    mat[is.na(suppressWarnings(as.numeric(mat)))
                                        & !upper.tri(mat)] <- '-'
                                    out <- capture.output(matprint(mat))
                                    o <- c(o, '', "p-values", out)
                                }
                            }
                        }
                    }
                }
                o <- c(o, "", paste0("Inference based on ", n, " observation",
                                     ifelse(n > 1, "s", ""), "."))
                M <- if (is.null(g1)) n.missing else n.missing[lev[i]]
                if (M > 0) {
                    o <- c(o, paste0("(", M, " observation", ifelse(M > 1, "s ", " "),
                                     "removed due to missing values)"))
                }
            }
        } else {
          # X and Y are numeric
            
            if (is.null(opts$trend)) {
              # Nothing to get inference of ...
                o <- c(o, paste("Add trend lines to the scatter plot in order",
                                "to get an inference"),
                       paste("(Use the Add To Plot feature at the bottom",
                             "of the graphics window)"))
            } else {
                for (i in 1:N) {
                    if (!is.null(g1)) {
                        o <- if (i > 1) c(o, paste(rep('_', 80), collapse = ''), '') else c(o)
                        o <- c(o, paste0("Inference for ", varnames$g1, ' = ', lev[i]), '')
                    }
                    
                    X <- x.list[[i]]
                    Y <- y.list[[i]]
                    
                    if (opts$bs.inference) {
                      # Bootstrap inference
                        if ("linear" %in% opts$trend) {
                            fit <- lm(Y ~ X)
                            names(fit$coefficients)[2] <- varnames$x
                            o <- c(o, "Linear Trend", '')
                            
                            b <- boot(data.frame(X, Y),
                                      function(data, id, xname) {
                                          d <- data[id, ]
                                          fit <- lm(d[, 2] ~ d[, 1])
                                          names(fit$coefficients)[2] <- xname
                                          
                                          if (any(apply(d, 2, sd) == 0)) {
                                              corr <- NA
                                          } else {
                                              corr <- cor(d[, 2], d[, 1])
                                          }
                                          
                                          c(slope = coef(fit)[2], intercept = coef(fit)[1],
                                            correlation = corr)
                                      }, R = 1500, xname = varnames$x)
                            
                            ci.slope <- boot.ci(b, type = "perc", index = 1)
                            ci.int   <- boot.ci(b, type = "perc", index = 2)
                            ci.corr  <- boot.ci(b, type = "perc", index = 3)
                            
                            ci.l.slope <- signif(ci.slope$percent[1, 4], 5)
                            ci.u.slope <- signif(ci.slope$percent[1, 5], 5)
                            ci.l.int   <- signif(ci.int$percent[1, 4], 5)
                            ci.u.int   <- signif(ci.int$percent[1, 5], 5)
                            ci.l.corr  <- signif(ci.corr$percent[1, 4], 5)
                            ci.u.corr  <- signif(ci.corr$percent[1, 5], 5)
                            
                            inf.df <-
                                data.frame(ci.lower = c(ci.l.slope, ci.l.int, ci.l.corr),
                                           estimate = c(signif(coef(fit)[2], 5),
                                               signif(coef(fit)[1], 5),
                                               signif(cor(Y, X), 5)),
                                           ci.upper = c(ci.u.slope, ci.u.int, ci.u.corr),
                                           row.names = c("slope", "intercept", "correlation"))
                            inf.mat <- capture.output(matprint(as.matrix(inf.df)))
                            
                            o <- c(o, "Estimates with Percentile Bootstrap Confidence Intervals",
                                   '', eval(inf.mat), '')
                            
                            if ("quadratic" %in% opts$trend) {
                                o <- c(o, "Quadratic Trend: No inference included.", '')
                            }
                            if ("cubic" %in% opts$trend) {
                                o <- c(o, "Cubic Trend: No inference included.", '')
                            }
                        }
                    } else {
                      # Normal inference
                        if ("linear" %in% opts$trend) {
                            fit <- lm(Y ~ X)
                          # Need to set the correct names:
                            names(fit$coefficients)[2] <- varnames$x
                            B <- coef(summary(fit))
                            o <- c(o, "Linear Trend", '',
                                   paste0("Slope = ", signif(B[2, 1], 5),
                                          "   p-value = ", signif(B[2, 4], 5)),
                                   paste0("Intercept = ", signif(B[1, 1], 5)), '',
                                   "Confidence Intervals:", capture.output(ciReg(fit)), '')
                        }
                        
                        if ("quadratic" %in% opts$trend) {
                            o <- c(o, "Quadratic Trend: No inference included.", '')
                        }
                        if ("cubic" %in% opts$trend) {
                            o <- c(o, "Cubic Trend: No inference included.", '')
                        }
                    }
                
                    n <- length(X)
                    o <- c(o, "", paste0("Inference based on ", n, " observation",
                                         ifelse(n > 1, "s", ""), "."))
                    M <- if (is.null(g1)) n.missing else n.missing[lev[i]]
                    if (M > 0) {
                        o <- c(o, paste0("(", M, " observation", ifelse(M > 1, "s ", " "),
                                         "removed due to missing values)"))
                    }
                }
            }
        }
    } else {
        if (is.null(y)) {
          # X is a factor
            for (i in 1:N) {
                o <- if (i > 1) c(o, paste(rep('_', 80), collapse = ''), '') else c(o)

                o <- c(o, paste0(varnames$x,
                                 ifelse(is.null(g1), '',
                                        paste0(" for ", varnames$g1, ' = ', lev[i]))), '')

                X <- x.list[[i]]
                n <- length(X)

                if (opts$bs.inference) {
                  # Bootstrap inference
                    if (n > 10) {
                        b <- boot(X,
                                  function(x, d) {
                                      tab <- table(x[d])
                                      tab / sum(tab)
                                  },
                                  R = opts$n.boot)
                        phat <- as.numeric(b$t0)
                        ci.p <- apply(b$t, 2, quantile, probs = c(0.025, 0.975))
                        ci.l <- signif(ci.p[1, ], 5)
                        ci.u <- signif(ci.p[2, ], 5)

                        o <- c(o, "Proportions with percentile Bootstrap Confidence Intervals")

                        inf.df <- data.frame(ci.lower = ci.l,
                                             estimate = signif(phat, 5),
                                             ci.upper = ci.u)
                        rownames(inf.df) <- levels(X)
                        inf.mat <- capture.output(matprint(as.matrix(inf.df)))
                        o <- c(o, eval(inf.mat), '')
                        
                    } else {
                        o <- c(o, paste0("No inference output ",
                                         "(number of observations too small for ",
                                         "bootstrap inference.)"))
                    }
                } else {
                  # Normal inference
                    if (n > 1) {
                        tab <- table(X)
                        phat <- tab / n
                        se <- sqrt(phat * (1 - phat) / n)
                        qval <- 1.96
                        inf.mat <-
                            capture.output(matprint(matrix(c(signif(phat - qval * se, 5),
                                                             signif(phat, 5),
                                                             signif(phat + qval * se, 5)),
                                                           ncol = 3,
                                                           dimnames =
                                                           list(levels(X),
                                                                c("ci.lower", "estimate",
                                                                  "ci.upper")))))
                        o <- c(o,
                               "Proportions with 95% Confidence Intervals",
                               "-----------------------------------------", '',
                               eval(inf.mat))

                        if (length(levels(X)) > 1) {
                            o <- c(o, '',
                                   "Differences between Proportions (col - row)",
                                   "-------------------------------------------", '')

                            out <- capture.output(matprint(freq1way.edited(tab, "estimates")))
                            o <- c(o, "Estimates:", eval(out))

                            out <- capture.output(matprint(freq1way.edited(tab, "ci")))
                            o <- c(o, '', "95% Confidence Intervals:", eval(out))

                            uniTest <- try(capture.output(chiSquareTest(tab)))
                            if (!inherits(uniTest, "try-error")) {
                                o <- c(o, '',
                                       "Chi-square test for equal probabilities:",
                                       eval(uniTest))
                            }

                            o <- c(o, '')
                        }
                    } else {
                        o <- c(o, "No inference output (not enough observations!)")
                    }
                }
                o <- c(o, "", paste0("Inference based on ", n, " observation",
                                     ifelse(n > 1, "s", ""), "."))
                M <- if (is.null(g1)) n.missing else n.missing[lev[i]]
                if (M > 0) {
                    o <- c(o, paste0("(", M, " observation", ifelse(M > 1, "s ", " "),
                                     "removed due to missing values)"))
                }
            }
        } else if (is.factor(y)) {
          # X and Y are factors
            for (i in 1:N) {
                o <- if (i > 1) c(o, paste(rep('_', 80), collapse = ''), '') else c(o)
                
                o <- c(o, paste0("Distribution of ", varnames$x, " by ",
                                 varnames$y,
                                 ifelse(is.null(g1), '',
                                        paste0(" for ", varnames$g1, ' = ', lev[i]))), '')

                X <- x.list[[i]]  # check these are in the correct order (:
                Y <- y.list[[i]]
                n <- length(X)

                n1 <- length(levels(Y))
                tab <- table(Y, X)
                tot <- rsum <- rowSums(tab)
                phat <- sweep(tab, 1, rsum, "/")
                nr <- nrow(tab)

                if (opts$bs.inference) {
                  # Bootstrap inference
                    






                    
                 # o <- c(o, "Not yet implemented :(")
                } else {
                  # Normal inference
                    mat <- cbind(phat, Row.sums = rowSums(phat))
                    inf.mat <- capture.output(matprint(signif(mat, 3)))
                    o <- c(o, "Estimates", inf.mat)
                    
                    mat <- matrix(NA, nrow = 2 * length(levels(Y)),
                                  ncol = length(levels(X)),
                                  dimnames =
                                  list(c(rbind(levels(Y), '')), levels(X)))
                    
                    for (j in 1:n1) {
                        se <- sqrt(phat[j, ] * (1 - phat[j, ]) / rsum[j])
                        mat[j * 2 - 1, ] <- signif(phat[j, ] - 1.96 * se, 3)
                        mat[j * 2, ] <- signif(phat[j, ] + 1.96 * se, 3)
                    }
                    
                    o <- c(o, '', "95% Confidence Intervals",
                           capture.output(matprint(mat)))
                    
                    if (all(tot > 1)) {
                        for (j in 1:length(levels(X))) {
                            o <- c(o, '',
                                   paste0("Differences between proportions of ", varnames$y,
                                          " for ", varnames$x, " = ", levels(X)[j],
                                          " (col - row)"))
                            p <- phat[, j]
                            n <- length(p)
                            diff.mat <- signif(outer(p[-1], p[-length(p)],
                                                     function(p1, p2) p1 - p2), 3)
                            diff.mat[upper.tri(diff.mat)] <- ''
                            o <- c(o, '', "Estimates",
                                   capture.output(matprint(diff.mat)))
                            
                            ci.mat <- matrix(NA, nrow = 2 * (n - 1), ncol = n - 1,
                                             dimnames =
                                             list(c(rbind(levels(Y)[-n], '')), levels(Y)[-1]))

                          # Add the confidence intervals as calculated by function pDiffCI()
                            for (k in 2:n) {
                                for (l in 1:(k - 1)) {
                                    wr <- (k - 2) * 2 + 1
                                    ci.mat[wr:(wr + 1), l] <-
                                        signif(pDiffCI(p[k], p[l], tot[k], tot[l]), 3)
                                }
                            }
                            ci.mat[is.na(ci.mat)] <- ''
                            o <- c(o, '', "95% Confidence Intervals",
                                   capture.output(matprint(ci.mat)))
                        }

                      # Chi-squared test for independence
                        chiTest <- try(capture.output(chiSquareTest(tab)))
                        if (!inherits(chiTest, "try-error")) {
                            o <- c(o, "", "Chi-square test for independence",
                                   paste0("(i.e. is the distribution of ", varnames$x,
                                          " independent of ", varnames$y, "?)"), '',
                                   eval(chiTest), '')
                        }
                    } else {
                        o <- c(o, "No Inference included")
                    }
                }

                n <- length(X)
                o <- c(o, "", paste0("Inference based on ", n, " observation",
                                     ifelse(n > 1, "s", ""), "."))

                M <- if (is.null(g1)) n.missing else n.missing[lev[i]]
                if (M > 0) {
                    o <- c(o, paste0("(", M, " observation", ifelse(M > 1, "s ", " "),
                                     "removed due to missing values)"))
                }
            }
        } else {
          # Y is numeric
          # This should not happen (:
        }
    }

    o <- c(o, '\n')
    
    class(o) <- "iNZightPlotInference"
    o
}

###############################################################################################
# End of getPlotInference()

print.iNZightPlotInference <- function(x) {
    cat(paste(x, collapse = "\n"))
}


freq1way.edited <- function(tbl, inf.type = "estimates", conf.level = 0.95) {
    ## Before freq1way is called should output the variable name in the table
    level.names <- names(tbl)
    n <- sum(tbl)
    ncats <- length(tbl)
    ncatsC2 <- choose(ncats, 2)

    if (is.null(level.names)) level.names <- 1:ncats

    conf.pc <- conf.level * 100
    phat <- tbl / sum(tbl)

    qval <- abs(qnorm((1 - conf.level) / (2 * ncats)))

    matw <- matrix(NA, ncats - 1, ncats - 1)

    dimnames(matw) <- list(level.names[-length(level.names)], level.names[-1])

    qval.adjusted <- abs(qnorm((1 - conf.level) / (2 * ncatsC2)))

    tempw <- ""

    if (inf.type == "estimates") {
        for(i1 in 1:(ncats - 1)) {
            for(i2 in 2:ncats) {
                tempw <- phat[i1] - phat[i2]
                tempw <- signif(tempw, 5)
                matw[i1, i2 - 1] <- ifelse((i1 < i2), tempw , " ")
            }
        }
        t(matw)
    } else {
        testMatrix <- matrix(" ", ncats - 1, 2 * ncats - 2)
        count <- 1
        count.2 <- 0
        for(i1 in 1:(ncats - 1)) {
            count <- 0
            for(i2 in 2:ncats) {
                tempw <- phat[i1] - phat[i2] +
                    abs(qnorm((1 - conf.level) / (2 * ncatsC2))) * c(-1, 1) *
                        sqrt(((phat[i1] + phat[i2]) - ((phat[i1] - phat[i2])^2)) / n)
                tempw <- signif(tempw, 5)
                matw[i1, i2 - 1] <- ifelse((i1 < i2),
                                           paste("(", tempw[1], ",", tempw[2], ")", sep = ""),
                                           " ")
                
                if(i2 == 2)
                    count <- i2 - 2
                if(i1 < i2) {
                    testMatrix[i1, count + 1 + count.2] <- tempw[1]
                    testMatrix[i1, (count = count + 2) + count.2] <- tempw[2]
                }
            }
            count.2 <- count.2 + 2
        }

        rowNames <- rep("", ncats * 2)
        temp <- 1:(ncats * 2)
        rowNames[(temp %% 2 != 0)] <- level.names

        testMatrix <- t(testMatrix)
        rownames(testMatrix) <- rowNames[-c(1, 2)]
        colnames(testMatrix) <- level.names[-ncats]

        testMatrix
    }
}

chiSquareTest <- function(tbl) {
    chitest <- chisq.test(tbl)

    estimated <- 0
    digits <- 2
    ncats <- length(tbl)
    dfs <- ncats - 1
    expectedCounts <- chitest$exp

    chitest$p.value <- 1 - pchisq(chitest$statistic, dfs - estimated)
    cat(names(chitest$statistic), " = ",
        format(signif(chitest$statistic, 5)), ", ", sep = "")
    cat(paste(names(chitest$parameter), " = ",
              format(signif(chitest$parameter -  estimated, 5)), ",", sep = ""), "")
    cat("p-value =", format.pval(chitest$p.value, digits = 4))
}

calcF <- function(x, y, fun = mean) {
    uy <- unique(y)
    gstat <- fun(x)
    sstats <- ns <- numeric(length(uy))
    j <- 1
    SSW <- 0
    for (i in uy) {
        samp <- x[y == i]
        sstats[j] <- fun(samp)
        ns[j] <- length(samp)
        SSW <- SSW + sum((samp - sstats[j])^2)
        j <- j + 1
    }

    SSB <- sum(ns * ((sstats - gstat)^2))
    dfB <- length(uy) - 1
    dfW <- length(x) - length(uy)
    fstat <- (SSB / dfB) / (SSW / dfW)
    pval <- pf(fstat, dfB, dfW, lower.tail = FALSE)
    list(fstat = fstat, dfb = dfB, dfw = dfW, p = pval)
}


triangularMatrix <- function(factorLvls, output, statType) {
    statsMatrix <- as.matrix(output)
    colNames <- colnames(statsMatrix)

    condition <- FALSE
    if(!is.null(colNames)) {
        if (all(colnames(statsMatrix) == c("Estimate", "Tukey.L", "Tukey.U", "Tukey.p"))) {
            condition <- TRUE

            Nlev <- length(factorLvls)
            rns <- c()
            for (i in 1:(Nlev-1))
                rns <- c(rns, paste(factorLvls[i], " - ", factorLvls[(i+1):Nlev]))
            
            
            output.df <- as.data.frame(statsMatrix)
            output.df$name <- rownames(output.df)
            
            fake <- data.frame(name=rns)
            statsMatrix <- as.matrix(merge(output.df, fake, by = "name", all.y = TRUE)[, -1])
            rownames(statsMatrix) <- rns
        }
        else condition <- FALSE
    } else {
        condition <- FALSE
    }

    if (statType == "estimates") {
        if (condition)
            values <- statsMatrix[, 1]
        else
            values <- statsMatrix[1, ]
    } else if (statType == "p-values") {
        if (condition)
            values <- statsMatrix[, 4]
        else
            values <- statsMatrix[4, ]
    } else if (statType == "ci") {
        count <- 1
        i <- count
        
        if(condition) {
            values <- numeric(nrow(statsMatrix) * 2)
            while (count < nrow(statsMatrix) + 1) {
                values[c(i, i + 1)] <- c(statsMatrix[count, 2], statsMatrix[count, 3])
                count <- count + 1
                i <- i + 2
            }
        } else {
            values <- numeric(ncol(statsMatrix) * 2)
            while (count < ncol(statsMatrix) + 1) {
                values[c(i, i + 1)] <- c(statsMatrix[2, count], statsMatrix[3, count])
                count <- count + 1
                i = i + 2
            }
        }
    }

    num <- length(factorLvls)
    newMatrix <- matrix(NA, ncol <- num, nrow <- num)

    if (statType %in% c("estimates", "p-values")) {
        stopAt <- 0
        for(i in 1:num) {
            if (i == 1) startAt <- 1
            stopAt <- (stopAt - i) + num
            
            if (i == num) extra <- numeric(0)
            else extra <- values[startAt:stopAt]

            newMatrix[, i] <- format(c(rep("", num + i - num), signif(extra, 5)),
                                     width = 5, justify = "r")
            startAt <- stopAt + 1
        }
        rownames(newMatrix) <- factorLvls
        colnames(newMatrix) <- factorLvls
        x <- ncol(newMatrix)
        newMatrix <- as.matrix(newMatrix[-1, -x])

        if (x == 2) newMatrix <- t(newMatrix)
        rownames(newMatrix) <- factorLvls[2:length(factorLvls)]
        colnames(newMatrix) <- factorLvls[1:(length(factorLvls) - 1)]

    } else if (statType == "ci") {
        stopAt <- 0
        doubleNum <- num * 2
        newMatrix <- matrix(NA, ncol = num, nrow = doubleNum)

        for (i in 1:num) {
            if (i == 1) startAt <- 1
            stopAt <- (stopAt - i * 2) + doubleNum

            if (i == num) extra <- numeric(0)
            else extra <- values[startAt:stopAt]

            newMatrix[, i] <- format(c(rep("", (doubleNum + i - doubleNum) * 2),
                                       signif(extra, 5)), width = 5, justify = "r")
            startAt <- stopAt + 1
        }
        rowNames <- rep("", doubleNum)
        temp <- 1:doubleNum
        rowNames[(temp %% 2 != 0)] <- factorLvls

        x <- ncol(newMatrix)
        newMatrix <- as.matrix(newMatrix[-(1:2), -x])

        rownames(newMatrix) <- rowNames[-c(1, 2)]
        colnames(newMatrix) <- factorLvls[-x]
    }
    newMatrix
}

pDiffCI <- function(p1, p2, n1, n2, z = 1.96) {
    p <- p1 - p2
    se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)

    p + c(-1, 1) * z * se
}
