proportionalPointSize <- function(x, cex) {
  # Calculates point sizes, depending on the
  # value of a numeric variable.

  # Try from half cex to 2 x cex
    cex.range <- cex * c(0.5, 4)
    x.range <- range(x)

  # Calculate new sizes
    pr.size <- (x - x.range[1]) / (x.range[2] - x.range[1])
    size <- pr.size * (cex.range[2] - cex.range[1]) + cex.range[1]

    size
}
