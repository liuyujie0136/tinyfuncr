#' Draw Spirograph
#'
#' @description Draw Spirograph using ggplot2. See reference at https://blog.csdn.net/w573719227/article/details/104584879
#'
#' @param R outer circle radius, should only contain one value
#' @param r numeric vector of inner circle radius
#' @param p numeric vector of distance between pen-point and center of inner circle
#' @param col character vector of line color
#' @param width numeric vector of line width. Typically, `r`, `p`, `col` and `width` should be the same length, but you can only provide `r` and `p` to use default color and line width.
#'
#' @import ggplot2
#' @importFrom RColorBrewer brewer.pal
#' @importFrom gmp gcd
#'
#' @examples
#' spirograph()
#' spirograph(31, seq(11, 20, 2), seq(6, 15, 2))
#'
#' @author Yujie Liu
#' @export


spirograph <- function(R = 25,
                       r = 12,
                       p = 5,
                       col = "skyblue",
                       width = 0.2) {

  # check and remedy
  if (length(r) != length(p)) {
    stop("Layer Data Length Error!")
  }
  if (length(r) != length(col)) {
    message("Length of color data is not correct! Using RColorBrewer to generate colors...")
    col <- RColorBrewer::brewer.pal(length(r), "Set3")
  }
  if (length(r) != length(width)) {
    message("Length of pen width data is not correct! Using 0.2 for each line...")
    width <- rep(0.2, length(r))
  }

  spiro <- ggplot() + theme_void() + coord_fixed()
  for (idx in 1:length(r)) {
    # calc para
    R <- R
    k <- r[idx] / R
    l <- p[idx] / r[idx]
    n <- r[idx] / gmp::gcd(r[idx], R)

    # compute line position
    pos <- data.frame()
    for (theta in ((1:(360 * n)) * pi / 180)) {
      x <- R * ((1 - k) * cos(theta) + l * k * cos((1 - k) / k * theta))
      y <- R * ((1 - k) * sin(theta) - l * k * sin((1 - k) / k * theta))
      pos <- rbind(pos, data.frame(x = x, y = y))
    }

    # plot
    spiro <-
      spiro + geom_path(data = pos,
                        aes(x = x, y = y),
                        color = col[idx],
                        size = width[idx])
  }
  spiro
}
