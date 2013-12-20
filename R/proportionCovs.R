proportionCovs <- function(tbl, phat = tbl / sum(tbl), n = sum(tbl)) {
    covs <- -outer(phat, phat) / n
    print(covs)
    diag(covs) <- phat * (1 - phat) / n
    covs
}

errorbarsize <- function(Covs) {
    stopifnot(is.matrix(Covs))
    stopifnot((k <- nrow(Covs)) == ncol(Covs))  # Must be square.
    
    Vars <- diag(Covs)
    if (k < 3)
        out <- sqrt(Vars)
    
    mat <- matrix(0, nrow = k, ncol = k)
    keep <- col(Covs) > row(Covs)  # Upper triangle, above diag.
    k2 <- sum(keep)
    Xr <- row(mat)[keep]
    Xc <- col(mat)[keep]
    X <- outer(1:k2, 1:k, function(x,y) y == Xr[x] | y == Xc[x])
    Y = sqrt(outer(Vars, Vars, "+") - 2 * Covs)[keep]
    out <- drop(solve(crossprod(X)) %*% t(X) %*% Y)

    out
}
