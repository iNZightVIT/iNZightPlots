make_privacy_controls <- function(ctrls = NULL) {
    if (is.null(ctrls)) return(NULL)

    if (is.null(ctrls$rounding)) ctrls$rounding <- ""
    if (is.null(ctrls$suppression)) ctrls$suppression <- -Inf
    if (is.null(ctrls$suppression_raw_counts)) ctrls$suppression_raw_counts <- NA_integer_
    if (is.null(ctrls$secondary_suppression)) ctrls$secondary_suppression <- TRUE
    if (is.null(ctrls$check_rse)) ctrls["check_rse"] <- list(NULL)
    if (is.null(ctrls$symbol)) ctrls$symbol <- "S"
    if (is.null(ctrls$seed)) ctrls$seed <- NA

    list(
        round =
            if (is.numeric(ctrls$rounding)) {
                function(x) round(x / ctrls$rounding) * ctrls$rounding
            } else {
                switch(as.character(ctrls$rounding),
                    "RR3" = function(x) {
                        d <- dim(x)
                        dn <- dimnames(x)
                        x <- rr(x, 3L)
                        dim(x) <- d
                        dimnames(x) <- dn
                        x
                    },
                    "GRR" = function(x) {
                        d <- dim(x)
                        dn <- dimnames(x)
                        x <- rr(x,
                            c(3L, 2L, 5L, 10L, 100L),
                            c(19L, 20L, 100L, 1000L)
                        )
                        dim(x) <- d
                        dimnames(x) <- dn
                        x
                    },
                    function(x) x
                )
            },
        suppression_matrix = function(x, using_raw = FALSE) {
            d <- dim(x)
            dn <- dimnames(x)
            t <- ifelse(using_raw, ctrls$suppression_raw_counts, ctrls$suppression)
            x <- x < t
            dim(x) <- d
            dimnames(x) <- dn
            if (ctrls$secondary_suppression) {
                if (length(d) == 2) {
                    # two way
                    x <- cbind(x, apply(x, 1, sum) == 1L)
                } else {
                    # one way
                    x <- c(x, sum(x) == 1L)
                }
            } else {
                if (length(d) == 2) {
                    # two way
                    x <- cbind(x, FALSE)
                } else {
                    # one way
                    x <- c(x, FALSE)
                }
            }
            x
        },
        suppress = function(x, mat, symbol) {
            if (is.null(mat)) return(x)
            if (missing(symbol)) symbol <- ctrls$symbol
            x[mat] <- symbol
            x
        },
        rse_matrix = function(x, e) {
            d <- dim(x)
            dn <- dimnames(x)
            if (is.null(ctrls$check_rse)) {
                return(matrix(rep("", length(x)), d[1], d[2], dimnames = dn))
            }

            z <- 100 * e / x

            c <- unique(sort(c(0, ctrls$check_rse$cut)))
            o <- c(NA_character_, ctrls$check_rse$output)
            if (length(c) != length(o)) stop("invalid cut or output")

            ci <- sapply(z, function(zi) max(which(c <= zi)))
            o <- o[ci]
            dim(o) <- d
            dimnames(o) <- dn

            cbind(o, NA_character_)
        },
        markup = function(x, mat, exclude = "suppress") {
            d <- dim(x)
            dn <- dimnames(x)
            if (is.null(mat)) return(x)

            mat[is.na(mat)] <- ""
            y <- paste0(x, mat)
            y[mat %in% exclude] <- x[mat %in% exclude]
            dim(y) <- d
            dimnames(y) <- dn
            y

        },
        has = function(x) {
            !is.null(ctrls[[x]]) &&
            (length(ctrls[[x]]) > 1L || (
                !is.na(ctrls[[x]]) &&
                ifelse(is.numeric(ctrls[[x]]),
                    is.finite(ctrls[[x]]),
                    ctrls[[x]] != ""
                )
            ))
        },
        get = function(x) ctrls[[x]]
    )
}

# randomly round x to base b, optionally graduated at cut-points g
rr <- function(x, b, g) {
    if (!missing(g)) {
        g <- sort(unique(c(0, g)))
        if (length(b) != length(g)) stop("invalid g")
        gi <- sapply(x, function(xi) max(which(g <= xi)))
        b <- b[gi]
    }

    u <- runif(length(x))
    r <- x %% b
    p <- (b - r) / b

    x + b - r - b * (u < p)
}
